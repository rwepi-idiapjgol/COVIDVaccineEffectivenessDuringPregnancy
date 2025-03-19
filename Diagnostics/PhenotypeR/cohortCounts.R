# renv::activate()
# renv::restore()
# .rs.restartR()

# packages 
library(CDMConnector)
library(DBI)
library(dbplyr)
library(dplyr)
library(CodelistGenerator)
library(PatientProfiles)
library(here)
library(IncidencePrevalence)
library(tictoc)
library(readr)
library(stringr)
library(testthat)
library(SqlRender)
library(CirceR)
library(tidyr)
library(CohortCharacteristics)
library(visOmopResults)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "..."

# Connection details
server_dbi <- Sys.getenv("...")
user <- Sys.getenv("...")
password <- Sys.getenv("...")
port <- Sys.getenv("...")
host <- Sys.getenv("...")

db <- dbConnect(
  "...",
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm_schema <- "..."
write_schema <- "..."
achilles_schema <- "..."

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "..."

# CDM object ----
cdm <- cdm_from_con(con = db,
                    cdm_schema = c(schema = cdm_schema),
                    write_schema = c(schema= write_schema, prefix = study_prefix),
                    achilles_schema = achilles_schema, 
                    cdm_name = db_name
)

# Create Pfizer cohort manually ----
codes <- codesFromCohort(path = here("Cohorts", "pfizer.json"), cdm = cdm)

# all codes
pfizer <- cdm$drug_exposure |> filter(drug_concept_id %in% codes$pfizer) |> compute()
results <- tibble(step = "all codes", n = pfizer |> tally() |> pull("n"))

# in observation
pfizer <- pfizer |>
  left_join(cdm$observation_period |> select(
    person_id, observation_period_start_date, observation_period_end_date
  )) |>
  filter(!is.na(observation_period_start_date) & !is.na(observation_period_end_date)) |>
  compute()
results <- bind_rows(
  results,
  tibble(step = "in observation period table", n = pfizer |> tally() |> pull("n"))
)

pfizer <- pfizer |>
  filter(drug_exposure_start_date >= observation_period_start_date & drug_exposure_start_date <= observation_period_end_date) |>
  filter(drug_exposure_end_date >= observation_period_start_date & drug_exposure_end_date <= observation_period_end_date) |>
  compute()
results <- bind_rows(
  results,
  tibble(step = "drug record in observation", n = pfizer |> tally() |> pull("n"))
)

pfizer <- pfizer |>
  filter(drug_exposure_start_date <= drug_exposure_end_date) |>
  compute()
results <- bind_rows(
  results,
  tibble(step = "drug start <= end", n = pfizer |> tally() |> pull("n"))
)

write_csv(results, "pfizer_attrition.csv")

