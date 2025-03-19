info(logger, "Getting cohort stats: ")
cohort <- cdm$matched %>%
  addCohortName() %>%
  mutate(overall = "overall") %>%
  compute()

# sample independence
info(logger, " - Control reenrollment as exposed")
recontributions <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort %>%
      filter(exposed == 1) %>%
      select(all_of(c("cohort_name", "subject_id", x))) %>%
      inner_join(
        cohort %>%
          filter(exposed == 0) %>%
          select(all_of(c("cohort_name", "subject_id", x))),
        by = c("cohort_name", "subject_id", x)
      ) %>%
      group_by(.data$cohort_name, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  rename("group_level" = "cohort_name") |>
  mutate(
    result_type = "recontributions",
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = NA,
    cdm_name = cdmName(cdm),
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level))

# index date distribution
info(logger, " - Index date distribution")
index_date_bins <- lapply(
  list("overall", "vaccine_brand", "trimester"),
  function(x) {
    cohort %>%
      group_by(cohort_name, cohort_start_date, .data[[x]])%>%
      tally(name = "estimate_value") %>%
      collect()
  }) %>% bind_rows() |>
  mutate(
    result_type = "index_date",
    cdm_name = cdmName(cdm),
    estimate_name = "count",
    estimate_type = "integer",
    variable_name = "number subjects",
    variable_level = cohort_start_date,
    estimate_value = as.character(estimate_value),
    group_name = "cohort_name",
    group_level = cohort_name,
    additional_name = "overall",
    additional_level = "overall"
  ) |>
  pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
               names_to = "strata_name", values_to = "strata_level") |>
  filter(!is.na(strata_level)) |>
  select(!c("cohort_name", "cohort_start_date"))

# vaccine uptake
vaccine_uptake_bins <- NULL
for (dose in c("partial", "complete", "booster_1", "booster_2")) {
  doseNum <- switch(dose, "partial" = 1, "complete" = 2, "booster_1" = 3, "booster_2" = 4)
  vaccine_uptake_bins <- vaccine_uptake_bins %>%
    union_all(
      lapply(
        list("overall", "vaccine_brand", "trimester"),
        function(x, doseId = dose) {
          cohort %>%
            rename("dose_date" := !!doseId) %>%
            group_by(cohort_name, exposed, dose_date, .data[[x]])%>%
            filter(!is.na(dose_date)) |>
            tally(name = "estimate_value") %>%
            collect()
        }) %>% bind_rows() |>
        mutate(
          result_type = "vaccine_uptake",
          cdm_name = cdmName(cdm),
          estimate_name = "count",
          estimate_type = "integer",
          variable_name = paste0("dose number: ", doseNum),
          variable_level = dose_date,
          estimate_value = as.character(estimate_value),
          group_name = "cohort_name",
          group_level = cohort_name,
          additional_name = "exposed",
          additional_level = as.character(exposed)
        ) |>
        pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
                     names_to = "strata_name", values_to = "strata_level") |>
        filter(!is.na(strata_level)) |>
        select(!c("cohort_name", "dose_date", "exposed"))
    )
}

# vax during pregnancy
pregnant_uptake <- NULL
for (dose in c("partial", "complete", "booster_1", "booster_2")) {
  doseNum <- switch(dose, "partial" = 1, "complete" = 2, "booster_1" = 3, "booster_2" = 4)
  pregnant_uptake <- pregnant_uptake %>%
    union_all(
      lapply(
        list("overall", "vaccine_brand", "trimester"),
        function(x, doseId = dose) {
          cohort %>%
            rename("dose_date" := !!doseId) %>%
            filter(dose_date >= pregnancy_start_date & pregnancy_end_date >= dose_date) %>%
            group_by(cohort_name, exposed, .data[[x]])%>%
            tally(name = "estimate_value") %>%
            collect()
        }) %>% bind_rows() |>
        mutate(
          result_type = "pregnant_vaccination",
          cdm_name = cdmName(cdm),
          estimate_name = "count",
          estimate_type = "integer",
          variable_name = paste0("dose number: ", doseNum),
          variable_level = NA,
          estimate_value = as.character(estimate_value),
          group_name = "cohort_name",
          group_level = cohort_name,
          additional_name = "exposed",
          additional_level = as.character(exposed)
        ) |>
        pivot_longer(cols = c("overall", "vaccine_brand", "trimester"),
                     names_to = "strata_name", values_to = "strata_level") |>
        filter(!is.na(strata_level)) |>
        select(!c("cohort_name", "exposed"))
    )
}

write_csv(
  bind_rows(index_date_bins, recontributions, vaccine_uptake_bins, pregnant_uptake) %>%
    mutate(
      package_name = "StudyCode",
      package_version = today()
    ),
  here(output_folder, paste0("cohort_stats_", cdmName(cdm), ".csv"))
)
