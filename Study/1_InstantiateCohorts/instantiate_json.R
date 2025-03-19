# Read medication concept sets
info(logger, "Instantiate characterisation cohorts: ")

# Medications
info(logger, "  - Medications")
medications_codelist <- CodelistGenerator::codesFromConceptSet(
  path = here("1_InstantiateCohorts", "Cohorts", "MedicationsConceptSet"),
  cdm = cdm
)
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = medications_table_name,
  conceptSet = medications_codelist
)

# Read condition cohorts
info(logger, "  - Conditions")
conditions_cohort_set <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts", "Comorbidities")
)
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = conditions_cohort_set,
  name = conditions_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# COVID
info(logger, "  - COVID-19")
covid_json_cohort_set <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "Covid")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = covid_json_cohort_set,
  name = covid_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# COVID-19 vaccines
info(logger, "  - COVID-19 vaccines")
vax_codes <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "CovidVaccines")
)
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = vax_codes,
  name = vaccine_json_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# Ohter vax
info(logger, "  - Other vaccines")
other_vaccines_cohort_set <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "OtherVaccines")
)
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = other_vaccines_cohort_set,
  name = other_vaccines_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# Conditions to prioritize vaccination CAT
info(logger, "  - Conditions for prior vaccination")
ps_covariates_cohort_set <- readCohortSet(
  here("1_InstantiateCohorts", "Cohorts", "Matching")
)
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = ps_covariates_cohort_set,
  name = ps_covariates_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# export counts
json_cohort_counts <- cdm[[medications_table_name]] %>%
  settings() %>%
  select(cohort_definition_id, cohort_name) %>%
  inner_join(cdm[[medications_table_name]] %>%
               cohort_count() %>%
               mutate(
                 number_records = as.integer64(number_records),
                 number_subjects = as.integer64(number_subjects),
                 cohort_group = "medications"
               ),
             by = "cohort_definition_id"
  ) %>%
  union_all(cdm[[conditions_table_name]] %>%
              settings() %>%
              select(cohort_definition_id, cohort_name) %>%
              inner_join(cdm[[conditions_table_name]] %>%
                           cohort_count() %>%
                           mutate(
                             number_records = as.integer64(number_records),
                             number_subjects = as.integer64(number_subjects),
                             cohort_group = "conditions"),
                         by = "cohort_definition_id")) %>%
  union_all(cdm[[vaccine_json_table_name]] %>%
              settings()  %>%
              select(cohort_definition_id, cohort_name) %>%
              inner_join(cdm[[vaccine_json_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "covid_vaccines"),
                         by = "cohort_definition_id")) %>%
  union_all(cdm[[other_vaccines_table_name]] %>%
              settings() %>%
              select(cohort_definition_id, cohort_name) %>%
              inner_join(cdm[[other_vaccines_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "other_vaccines"),
                         by = "cohort_definition_id"))  %>%
  union_all(cdm[[ps_covariates_table_name]] %>%
              settings() %>%
              select(cohort_definition_id, cohort_name) %>%
              inner_join(cdm[[ps_covariates_table_name]] %>%
                           cohort_count() %>%
                           mutate(cohort_group = "ps_covariates"),
                         by = "cohort_definition_id"))

write_csv(json_cohort_counts,
          here(output_folder, paste0("json_cohort_counts_", database_name, ".csv")))
