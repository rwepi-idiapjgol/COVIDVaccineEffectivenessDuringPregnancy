output_folder <- here(results)
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# create logger ----
log_file <- here(results, paste0("log", "_", gsub("-", "", Sys.Date()), ".txt"))
if (file.exists(log_file)) {
  unlink(log_file)
}

logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0 INITIAL SETTINGS ----")

info(logger, "Load study parameters")

# Dates:
enrollment.end <- study.end - days(90)

# Instantiate cohorts:
table_stem <- tolower(table_stem)
vaccine_json_table_name    <- "vaccine_json"
medications_table_name     <- "medications"
conditions_table_name      <- "conditions"
covid_table_name           <- "covid"
other_vaccines_table_name  <- "other_vax"
ps_covariates_table_name   <- "ps_covariates"
nco_table_name             <- "nco_cohort"
source_pregnant_table_name <- "source_pregnant"
outcomes_table_name        <- "outcomes"
matched_cohort_table_name  <- "matched"
clean_mother_table_name    <- "mother_table"

# Minimum days between vaccine doses
days.booster     <- 90 # days for booster after pfizer, moderna, or astrazeneca
booster.janssen  <- 90 # days for booster after janssen
days.moderna     <- 28 # days for 2nd dose after moderna
days.astrazeneca <- 28 # days for 2nd dose after astrazeneca
days.pfizer      <- 21 # days for 2nd dose after pfizer
pfizer <- c(17, 42)
moderna <- c(24, 49)
days.badrecord <- 4

# save study settings ----
tibble(
  study.start = study.start,
  study.end = study.end,
  pfizer_window = paste0(pfizer, collapse = " to "),
  moderna_window = paste0(moderna, collapse = " to "),
  rerecording_days = days.badrecord,
  cdm_name = cdmName(cdm)
) |>
  write_csv(file = here(output_folder, paste0("study_settings_", database_name, ".csv")))

# Load study functions
info(logger, "Load study functions ")
source(here("0_SetUp", "functions.R"))

# Database snapshot:
readr::write_csv(CDMConnector::snapshot(cdm), here(output_folder, paste0("cdm_snapshot_", cdmName(cdm), ".csv")))

if (sensitvitySCIFIPEARL) {
  locations <- readr::read_csv(here::here("Data", "locations_sweden.csv"))
  cdm$person <- cdm$person |>
    inner_join(locations |> select("location_id"), by = "location_id", copy = TRUE) |>
    compute()
  subjects <- cdm$person |> distinct(person_id) |> compute()
  cdm$observation_period <- cdm$observation_period |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$observation <- cdm$observation |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$condition_occurrence <- cdm$condition_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$drug_exposure <- cdm$drug_exposure |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$measurement <- cdm$measurement |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$visit_occurrence <- cdm$visit_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
  cdm$procedure_occurrence <- cdm$procedure_occurrence |>
    inner_join(subjects, by = "person_id") |>
    compute()
}

if (runInstantiateCohorts) {
  info(logger, "STEP 1 INSTANTIATE COHORTS ----")
  source(here("1_InstantiateCohorts", "instantiate_json.R"))
  source(here("1_InstantiateCohorts", "instantiate_nco.R"))
  source(here("1_InstantiateCohorts", "instantiate_vaccination_table.R"))
  source(here("1_InstantiateCohorts", "instantiate_source_pregnant.R"))
  source(here("1_InstantiateCohorts", "instantiate_outcomes.R"))
}

if (runPSMathcing) {
  if (!runInstantiateCohorts) {
    info(logger, "Load cohorts")
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
      cdmName = database_name,
      cohortTables = c(vaccine_json_table_name, medications_table_name, conditions_table_name,
                       covid_table_name, other_vaccines_table_name, ps_covariates_table_name,
                       nco_table_name, source_pregnant_table_name, clean_mother_table_name,
                       outcomes_table_name),
      .softValidation = TRUE
    )
    cdm$vaccine_schema <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "vaccine_schema"))) %>%
      compute(name = inSchema(schema = results_database_schema, table = "vaccine_schema"), temporary = FALSE, overwrite = TRUE)
  }
  info(logger, "STEP 2 MATCHING ----")
  source(here("2_Matching", "matching.R"))
  source(here("2_Matching", "clean_matched_cohort.R"))
}

if (runCharacterisation) {
  if (! runPSMathcing) {
    info(logger, "Load cohorts")
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
      cdmName = database_name,
      cohortTables = c(
        vaccine_json_table_name, medications_table_name, conditions_table_name,
        covid_table_name, other_vaccines_table_name, nco_table_name,
        source_pregnant_table_name, outcomes_table_name,
        matched_cohort_table_name, ps_covariates_table_name,
        clean_mother_table_name),
      .softValidation = TRUE
    )
    cdm$vaccine_schema <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "vaccine_schema"))) %>%
      compute(name = "vaccine_schema", temporary = FALSE, overwrite = TRUE)
  }
  info(logger, "STEP 3 EVALUATE COHORTS ----")
  source(here("3_Characterisation", "characteristics.R"))
  source(here("3_Characterisation", "cohort_stats.R"))
}

if (runOutcomeModel) {
  if (!runPSMathcing & !runCharacterisation) {
    info(logger, "Load cohorts")
    cdm <- cdmFromCon(
      con = db,
      cdmSchema = cdm_database_schema,
      writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
      cdmName = database_name,
      cohortTables = c(nco_table_name, outcomes_table_name, matched_cohort_table_name)
    )
    cdm$vaccine_schema <- tbl(db, inSchema(schema = results_database_schema, table = paste0(table_stem, "vaccine_schema"))) %>%
      compute(name = "vaccine_schema", temporary = FALSE, overwrite = TRUE)
  }
  info(logger, "STEP 4 OUTCOME MODEL ----")
  source(here("4_OutcomeModel", "get_survival_data.R"))
  source(here("4_OutcomeModel", "estimate_survival.R"))
}

info(logger, "STEP 5 ZIP RESULTS ----")
output_folder <- basename(output_folder)
zip(
  zipfile = paste0(output_folder, "_", gsub("-", "", Sys.Date()), ".zip"),
  files = list.files(output_folder, full.names = TRUE)
)

dbDisconnect(db)

info(logger, " -- DONE! --")
