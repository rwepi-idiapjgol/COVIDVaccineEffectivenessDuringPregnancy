# Mother table:
cdm$mother_table_original <- tbl(db, inSchema(schema = mother_table_schema, table = mother_table_name)) %>%
  {if (grepl("CPRD", database_name)) {
    rename(., "pregnancy_outcome_id" = "original_outcome")
  } else . } %>%
  compute(name = inSchema(results_database_schema, "mother_table_original"), temporary = FALSE, overwrite = TRUE)

if (sensitvitySCIFIPEARL) {
  subjects <- cdm$person |> distinct(person_id) |> compute()
  cdm$mother_table_original <- cdm$mother_table_original |>
    inner_join(subjects, by = "person_id") |>
    compute()
}

# CLEAN MOTHER TABLE----
info(logger, "Clean mother table")
## Start with all population
cdm$mother_table <- cdm$mother_table_original %>%
  mutate(cohort_definition_id = 1,
         cohort_start_date = pregnancy_start_date,
         cohort_end_date = pregnancy_end_date) %>%
  rename("subject_id" = "person_id") %>%
  compute(name = "mother_table", temporary = FALSE, overwrite = TRUE) %>%
  newCohortTable(.softValidation = TRUE)

## In observation at pregnancy start date
cdm$mother_table <- cdm$mother_table %>%
  addInObservation() %>%
  filter(in_observation == 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "In observation at pregnancy start date")

## In observation at pregnancy end date
cdm$mother_table <- cdm$mother_table %>%
  addInObservation(indexDate = "cohort_end_date") %>%
  filter(in_observation == 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "In observation at pregnancy end date")

## end date > start_date
cdm$mother_table <- cdm$mother_table %>%
  filter(pregnancy_start_date < pregnancy_end_date) %>%
  mutate(cohort_definition_id = 1) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date")

## gestational length < 308 days
cdm$mother_table <- cdm$mother_table %>%
  filter(!!datediff("pregnancy_start_date", "pregnancy_end_date") < 308) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Gestational length < 308 days")

## gestational length  days != 0
cdm$mother_table <- cdm$mother_table %>%
  filter(gestational_length_in_day != 0) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Gestational length days != 0")

## 2 pregnancies starting in the same day
cdm$mother_table <- cdm$mother_table %>%
  addCohortIntersectCount(
    targetCohortTable = "mother_table",
    window = list(c(0, Inf)),
    indexDate = "pregnancy_start_date",
    censorDate = "pregnancy_end_date",
    targetStartDate = "pregnancy_start_date",
    targetEndDate = "pregnancy_end_date",
    nameStyle = "overlap"
  ) %>%
  filter(overlap <= 1) %>%
  left_join(
    cdm$observation_period |>
      select(subject_id = person_id, observation_period_start_date, observation_period_end_date)
  ) |>
  filter(
    cohort_start_date >= observation_period_start_date & cohort_start_date <= observation_period_end_date
  ) |>
  filter(
    cohort_end_date >= observation_period_start_date & cohort_end_date <= observation_period_end_date
  ) |>
  select(!c("observation_period_start_date", "observation_period_end_date")) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No overlapping pregnancy records")%>%
  newCohortTable()

## enrollment period: start
cdm$mother_table <- cdm$mother_table %>%
  filter(pregnancy_end_date > study.start) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Pregnancy end date > study start date")

## enrollment period: end
cdm$mother_table <- cdm$mother_table %>%
  filter(pregnancy_start_date < enrollment.end) %>%
  compute(name = "mother_table", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Pregnancy start date < enrollment end date") %>%
  newCohortTable()

# SOURCE POPULATION ----
info(logger, "Inclusion pregnant cirteria")
## 1 year of prior observation
cdm$source_pregnant <- cdm$mother_table  %>%
  addDemographics() %>%
  filter(prior_observation >= 365) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "1 year of prior observation at pregnancy start date") %>%
  newCohortTable()

## age
cdm$source_pregnant <- cdm$source_pregnant %>%
  filter(age >=12 & age <= 55) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Age at pregnancy start date in [12, 55]")

## sex
cdm$source_pregnant <- cdm$source_pregnant %>%
  filter(sex == "Female") %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Sex: female")

# eliminate subjects with multiple vax records
cdm$source_pregnant <- cdm$source_pregnant %>%
  anti_join(exclude_vax_records, by = "subject_id") %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Duplicate vaccination records")

## stop follow-up at vaccine irregularities
# save counts:
cdm$source_pregnant <- cdm$source_pregnant %>%
  left_join(censor_vaccination, by = "subject_id") %>%
  compute()
cdm$source_pregnant %>%
  filter(!is.na(censor)) %>%
  group_by(censor) %>%
  tally() %>%
  mutate(population = "pregnant") %>%
  ungroup() %>%
  union_all(
    censor_vaccination %>%
      group_by(censor) %>%
      tally() %>%
      mutate(population = "general") %>%
      ungroup()
  ) %>%
  collect() %>%
  write_csv(file = here(output_folder, paste0("vaccine_records_censor_", database_name, ".csv")))

# BY OBJECTIVE: 0-1, 2-3 ----
info(logger, "By objective inclusion criteria")

## 0-1, no vaccine before:
cdm$temp_none_first <- cdm$source_pregnant %>%
  compute(name = "temp_none_first", temporary = FALSE) %>%
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1, cohort_name = "none_first")) |>
  # no vaccine before pregnancy start
  left_join(
    cdm$vaccine_schema %>%
      filter(dose_id == 1) %>%
      select(subject_id, index_vaccine_date = vaccine_date, index_vaccine_brand = vaccine_brand),
    by = "subject_id"
  ) %>%
  filter(is.na(index_vaccine_date) | index_vaccine_date > pregnancy_start_date) %>%
  compute(name = "temp_none_first", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No vaccine before pregnancy") %>%
  anti_join(exclude_unkown_1, by = "subject_id") %>%
  compute(name = "temp_none_first", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Unkown vaccine brand")

## 2-3, no vaccine before:
cdm$temp_second_third <- cdm$source_pregnant %>%
  compute(name = "temp_second_third", temporary = FALSE) %>%
  newCohortTable(cohortSetRef = tibble(cohort_definition_id = 1, cohort_name = "complete_booster")) %>%
  # no 3rd dose before pregnancy
  left_join(
    cdm$vaccine_schema %>%
      filter(dose_id == 3) %>%
      select(subject_id, index_vaccine_date = vaccine_date, index_vaccine_brand = vaccine_brand),
    by = "subject_id"
  ) %>%
  filter(is.na(index_vaccine_date) | index_vaccine_date > pregnancy_start_date) %>%
  compute(name = "temp_second_third", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No 3rd dose before pregnancy") %>%
  # 2nd dose = days.booster days < pregnancy end date
  inner_join(
    cdm$vaccine_schema %>%
      filter(dose_id == 2) %>%
      select(subject_id, previous_vaccine_date = vaccine_date, previous_vaccine_brand = vaccine_brand),
    by = "subject_id"
  ) %>%
  filter(!!dateadd("previous_vaccine_date", days.booster) < pregnancy_end_date) %>%
  # enter the cohort when elegible for booster
  mutate(
    cohort_start_date = if_else(
      !!dateadd("previous_vaccine_date", days.booster) > pregnancy_start_date,
      !!dateadd("previous_vaccine_date", days.booster),
      pregnancy_start_date
    )
  ) %>%
  compute(name = "temp_second_third", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Elegible for 3rd dose during pregnancy") %>%
  # no vaccine irregularities before 3rd dose or on 3rd dose
  left_join(
    censor_vaccination |> select(subject_id, vaccine_censor_date)
  ) %>%
  filter(is.na(vaccine_censor_date) | vaccine_censor_date > index_vaccine_date) %>%
  compute(name = "temp_second_third", temporary = FALSE) %>%
  recordCohortAttrition(reason = "No irregular vaccine records on 3rd dose or before") %>%
  anti_join(exclude_unkown_2, by = "subject_id") %>%
  compute(name = "temp_second_third", temporary = FALSE) %>%
  recordCohortAttrition(reason = "Unkown vaccine brand")

cdm <- omopgenerics::bind(cdm$temp_none_first, cdm$temp_second_third, name = "source_pregnant")

# Set cohort dates and columns to keep:
cdm$source_pregnant <- cdm$source_pregnant %>%
  select(-cohort_end_date) %>%
  addDemographics(
    age = FALSE,
    sex = FALSE,
    priorObservation = TRUE,
    priorObservationName = "observation_period_start_date",
    priorObservationType = "date",
    futureObservation = TRUE,
    futureObservationName = "cohort_end_date",
    futureObservationType = "date"
  ) %>%
  addCohortIntersectDate(
    targetCohortTable = "mother_table",
    indexDate = "pregnancy_end_date",
    window = c(0, Inf),
    nameStyle = "next_pregnancy_date"
  ) %>%
  mutate(
    reason = if_else(
      !is.na(next_pregnancy_date) & next_pregnancy_date < cohort_end_date,
      "Next pregnancy", "End of observation"),
    cohort_end_date = if_else(
      !is.na(next_pregnancy_date) & next_pregnancy_date < cohort_end_date,
      !!dateadd("next_pregnancy_date", -1), cohort_end_date),
    cohort_end_date = as.Date(cohort_end_date),
    enrollment_end_date = !!dateadd("pregnancy_start_date", 238),
    enrollment_end_date = if_else(enrollment_end_date > pregnancy_end_date, pregnancy_end_date, enrollment_end_date)
  ) %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, pregnancy_id,
         pregnancy_start_date, pregnancy_end_date, enrollment_end_date, age, index_vaccine_date,
         index_vaccine_brand, previous_vaccine_date, previous_vaccine_brand, observation_period_start_date, reason) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  addInObservation(nameStyle = "start") %>%
  filter(start == 1) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "After date arrangements: In observation at start date") %>%
  addInObservation(indexDate = "cohort_end_date", nameStyle = "end") %>%
  filter(end == 1) %>%
  select(!c("start", "end")) %>%
  compute(name = "source_pregnant", temporary = FALSE) %>%
  recordCohortAttrition(reason = "After date arrangements: In observation at end date") %>%
  newCohortTable()
