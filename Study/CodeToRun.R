library(DBI)
library(here)
library(zip)
library(dbplyr)
library(dplyr)
library(CDMConnector)
library(tidyr)
library(readr)
library(PatientProfiles)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(glmnet)
library(log4r)
library(survival)
library(bit64)
library(CodelistGenerator)
library(DrugUtilisation)
library(MatchIt)
library(CirceR)
library(SqlRender)
library(omopgenerics)
library(visOmopResults)
library(CohortSurvival)
library(clock)
library(fmsb)

database_name <- "..."

# Connection details
server_dbi <- Sys.getenv("...")
user <- Sys.getenv("...")
password <- Sys.getenv("...")
port <- Sys.getenv("...")
host <- Sys.getenv("...")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm_database_schema <- "..."
results_database_schema <- "..."

# cohort stem where cohorts will be instantiated
table_stem <- "..."

cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = c("schema" = results_database_schema, "prefix" = tolower(table_stem)),
  cdmName = database_name
)

# Pregnancy tables details:
mother_table_schema <- results_database_schema
mother_table_name <- "..."

# minimum counts to report
minimum_counts <- 5

# output folder
results <- paste0("Results_", cdmName(cdm))

# study dates
study.start <- as.Date("...") # date of initiation of the vaccination campaing
study.end   <- as.Date("...") # end of data availability (data cut date)

# Choose code to run
runInstantiateCohorts <- TRUE
runPSMathcing         <- TRUE
runCharacterisation   <- TRUE
runOutcomeModel       <- TRUE
sensitvitySCIFIPEARL  <- FALSE

source(here("0_SetUp/RunStudy.R"))

print("Thanks for running the analysis!! :D")
