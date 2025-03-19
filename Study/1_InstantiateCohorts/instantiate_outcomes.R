info(logger, "Covid outcome cohorts")
cdm$temp_covid <- cdm$covid %>%
  compute(name = "temp_covid", temporary = FALSE)
cdm$temp_delivery <- tbl(db, inSchema(schema = mother_table_schema, table = mother_table_name)) %>%
  {if (grepl("CPRD", database_name)) {
    rename(., "pregnancy_outcome" = "original_outcome")
  } else . } %>%
  filter(pregnancy_outcome == 4092289) |>
  compute(name = "temp_delivery", temporary = FALSE, overwrite = TRUE)

info(logger, "Inpatient outcome cohorts")
ip.codes <- c(9201, 262)
ip.codes.w.desc <- cdm$concept_ancestor %>%
  filter(ancestor_concept_id  %in% ip.codes ) %>%
  select(descendant_concept_id) %>%
  distinct() %>%
  pull()
cdm <- generateVisitRelatedOutcomes(
  codes = ip.codes.w.desc, window = c(-3, 21), name = "inpatient", attritionReason = "COVID-19 related hospitalisation"
)

info(logger, "ICU outcome cohorts")
icu.codes <- c(32037)
cdm <- generateVisitRelatedOutcomes(
  codes = icu.codes, window = c(-3, 21), name = "icu", attritionReason = "COVID-19 related ICU"
)

# info(logger, "Death outcome cohorts")
# cdm$temp_death <- cdm$temp_covid %>%
#   inner_join(cdm$death %>% select(subject_id = person_id, death_date), by = "subject_id") %>%
#   mutate(diff_days = death_date - cohort_start_date) %>%
#   filter(diff_days >= 0 & diff_days <= 28) %>%
#   select(-death_date, -diff_days) %>%
#
#   distinct() %>%
#   compute(name = "temp_death", temporary = FALSE) %>%
#   recordCohortAttrition(reason = "COVID-19 related death") %>%
#   newCohortTable(cohortSetRef = settings(cdm$temp_covid) %>% mutate(cohort_name = paste0("death_", cohort_name)))

# outcome cohort
cdm <- omopgenerics::bind(cdm$temp_covid, cdm$temp_inpatient, cdm$temp_inpatient_delivery,
                          cdm$temp_icu, cdm$temp_icu_delivery, name = "outcomes")

# export counts
read_csv(file = here(output_folder, paste0("json_cohort_counts_", database_name, ".csv"))) %>%
  union_all(cdm$outcomes %>%
              settings() %>%
              inner_join(cdm$outcomes %>%
                           cohort_count() %>%
                           mutate(cohort_group = "outcomes"),
                         by = "cohort_definition_id")) %>%
  mutate(cdm_name = cdmName(cdm)) %>%
  write_csv(file = here(output_folder, paste0("json_cohort_counts_", database_name, ".csv")))

omopgenerics::dropTable(cdm, starts_with("temp"))
