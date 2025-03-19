### Read data
# cdm$matched_raw <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "matched_raw"))) %>%
#   compute(name = "matched_raw", overwrite = TRUE, temporary = FALSE)

### Add end of observation
cdm$matched <- cdm$matched_raw |>
  select(-"cohort_end_date") |>
  addFutureObservation(futureObservationType = "date") |>
  mutate(reason = "End of observation") |>
  compute(name = "matched", temporary = FALSE)

### Subject level censoring
# cohort start date --> index date
# cohort end date --> observation end / next pregnancy start
# pregnancy end date
cdm$matched <- cdm$matched %>%
  left_join(
    cdm$vaccine_schema %>%
      filter(dose_id <= 4) %>%
      select(subject_id, dose_id, vaccine_date) %>%
      pivot_wider(names_from = dose_id, values_from = vaccine_date) %>%
      select(subject_id, "partial" = "1", "complete" = "2", "booster_1" = "3", "booster_2" = "4"),
    by = "subject_id"
  ) %>%
  mutate(
    ## do not follow primary schema treatment
    recommended_lower = if_else(vaccine_brand == "pfizer", as.Date(!!dateadd("cohort_start_date", pfizer[1])), as.Date(!!dateadd("cohort_start_date", moderna[1]))),
    recommended_upper = if_else(vaccine_brand == "pfizer", as.Date(!!dateadd("cohort_start_date", pfizer[2])), as.Date(!!dateadd("cohort_start_date", moderna[2]))),
    reason = case_when(
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & is.na(.data$complete) & .data$future_observation > .data$recommended_upper ~ "No second dose", # censor at window recommended days upper
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & .data$complete < .data$recommended_lower & .data$future_observation >= .data$complete ~ "Second dose before recommended time", # censor at 2nd dose - 1 day
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & .data$complete > .data$recommended_upper & .data$future_observation >= .data$recommended_upper ~ "Second dose after recommended time", # censor at window recommended days upper
      .default = .data$reason
    ),
    ## update cohort end date
    cohort_end_date =  case_when(
      .data$reason == "No second dose" ~ recommended_upper, # censor at window recommended days upper
      .data$reason == "Second dose before recommended time" ~ as.Date(!!dateadd("complete", -1)), # censor at 2nd dose
      .data$reason == "Second dose after recommended time" ~ recommended_upper,
      .default = .data$future_observation
    ),
    ## deviate from treatment strategy
    reason = case_when(
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 1 & .data$cohort_end_date >= .data$booster_1 ~ "Exposed 3rd dose",
      .data$cohort_definition_id %in% 1:2 & .data$exposed == 0 & .data$cohort_end_date >= .data$partial ~ "Unexposed 1st dose",
      .data$cohort_definition_id %in% 3:4 & .data$exposed == 1 & .data$cohort_end_date >= .data$booster_2 ~ "Exposed 4th dose",
      .data$cohort_definition_id %in% 3:4 & .data$exposed == 0 & .data$cohort_end_date >= .data$booster_1 ~ "Unexposed 3rd dose",
      .default = .data$reason
    ),
    ## update individual end date
    cohort_end_date = case_when(
      .data$reason == "Exposed 3rd dose" | .data$reason == "Unexposed 3rd dose" ~ as.Date(add_days(.data$booster_1, -1)),
      .data$reason == "Unexposed 1st dose" ~ as.Date(add_days(.data$partial, -1)),
      .data$reason == "Exposed 4th dose" ~ as.Date(add_days(.data$booster_2, -1)),
      .default = cohort_end_date
    ),
    end_strategy_date = if_else(
      reason != "End of observation", cohort_end_date, NA
    )
  ) %>%
  compute(name = "matched", temporary = FALSE)

### Match level censoring
cdm$matched  <- cdm$matched  %>%
  select(-reason, -cohort_end_date, -end_strategy_date) %>%
  inner_join(
    cdm$matched  %>%
      group_by(cohort_definition_id, match_id) %>%
      filter(cohort_end_date == min(cohort_end_date)) %>%
      ungroup() %>%
      distinct(cohort_definition_id, match_id, cohort_end_date, reason) %>%
      collect() %>%
      group_by(cohort_definition_id, match_id) %>%
      mutate(reason = str_flatten(reason, collapse = "; ")) %>%
      ungroup() %>%
      distinct(),
    by = c("cohort_definition_id", "match_id"),
    copy = TRUE
  ) |>
  left_join(
    cdm$matched  %>%
      group_by(cohort_definition_id, match_id) %>%
      filter(end_strategy_date == min(end_strategy_date)) %>%
      ungroup() %>%
      distinct(cohort_definition_id, match_id, end_strategy_date, reason) %>%
      collect() %>%
      group_by(cohort_definition_id, match_id) %>%
      mutate(reason_pregnancy = str_flatten(reason, collapse = "; ")) %>%
      ungroup() %>%
      select(!"reason") %>%
      distinct(),
    by = c("cohort_definition_id", "match_id"),
    copy = TRUE
  ) |>
  mutate(
    cohort_end_date_pregnancy = case_when(
      is.na(end_strategy_date) ~ pregnancy_end_date,
      end_strategy_date < pregnancy_end_date ~ end_strategy_date,
      .default = pregnancy_end_date
    ),
    reason_pregnancy = case_when(
      is.na(end_strategy_date) ~ "End of pregnancy",
      end_strategy_date < pregnancy_end_date ~ reason,
      .default = "End of pregnancy"
    ),
    reason_pregnancy = if_else(
      exposed == 1,
      paste0(reason_pregnancy, " - Exposed"),
      paste0(reason_pregnancy, " - Unexposed")
    ),
    reason = if_else(
      exposed == 1,
      paste0(reason, " - Exposed"),
      paste0(reason, " - Unexposed")
    )
  ) |>
  compute(name = "matched", temporary = FALSE)

### Report censoring
censoring_observation <- cdm$matched  %>%
  inner_join(cohort_set, by = "cohort_definition_id", copy = TRUE) %>%
  mutate(time = !!datediff("cohort_start_date", "cohort_end_date")) %>%
  collect() |>
  group_by(cohort_name, reason) %>%
  summarise(n = n(), mean = mean(time), sd = sd(time), median = quantile(time, 0.5), q25 = quantile(time, 0.25), q75 = quantile(time, 0.75), min = min(time), max = max(time)) %>%
  bind_rows(
    cdm$matched  %>%
      inner_join(cohort_set, by = "cohort_definition_id", copy = TRUE) %>%
      mutate(time = !!datediff("cohort_start_date", "cohort_end_date")) %>%
      collect() %>%
      group_by(cohort_name) %>%
      summarise(n = n(), mean = mean(time), sd = sd(time), median = quantile(time, 0.5), q25 = quantile(time, 0.25), q75 = quantile(time, 0.75), min = min(time), max = max(time)) %>%
      mutate(reason = "overall")
  ) |>
  mutate(cdm_name = cdmName(cdm), followup_end = "observation_end")

censoring_pregnancy <- cdm$matched  %>%
  inner_join(cohort_set, by = "cohort_definition_id", copy = TRUE) %>%
  mutate(time = !!datediff("cohort_start_date", "cohort_end_date_pregnancy")) %>%
  collect() |>
  group_by(cohort_name, reason_pregnancy) %>%
  summarise(n = n(), mean = mean(time), sd = sd(time), median = quantile(time, 0.5), q25 = quantile(time, 0.25), q75 = quantile(time, 0.75), min = min(time), max = max(time)) %>%
  rename("reason" = "reason_pregnancy") |>
  bind_rows(
    cdm$matched  %>%
      inner_join(cohort_set, by = "cohort_definition_id", copy = TRUE) %>%
      mutate(time = !!datediff("cohort_start_date", "cohort_end_date_pregnancy")) %>%
      collect() %>%
      group_by(cohort_name) %>%
      summarise(n = n(), mean = mean(time), sd = sd(time), median = quantile(time, 0.5), q25 = quantile(time, 0.25), q75 = quantile(time, 0.75), min = min(time), max = max(time)) %>%
      mutate(reason = "overall")
  ) |>
  mutate(cdm_name = cdmName(cdm), followup_end = "pregnancy_end")

write_csv(
  censoring_pregnancy |>
    bind_rows(censoring_observation) |>
    mutate(across(
      .cols = all_of(c("n", "mean", "sd", "median", "q25", "q75")),
      ~ if_else(n < 5, NA, .x)
    )),
  file = here(output_folder, paste0("censoring_", cdmName(cdm), ".csv"))
)

cdm$matched <- cdm$matched %>%
  group_by(cohort_definition_id, match_id) %>%
  mutate(cohort_end_date = min(cohort_end_date)) %>%
  select(-c(
    "maternal_age", "recommended_lower", "recommended_upper", "reason", "reason_pregnancy",
    "end_strategy_date", "future_observation"
  )) %>%
  compute(name = "matched", temporary = FALSE) %>%
  omopgenerics::newCohortTable(cohortSetRef = cohort_set, cohortAttritionRef = cohort_attrition)

write_csv(
  attrition(cdm$matched) |> inner_join(settings(cdm$matched)) |> mutate(cdm_name = cdmName(cdm)),
  file = here(output_folder, paste0("population_attrition_", cdmName(cdm), ".csv"))
)
