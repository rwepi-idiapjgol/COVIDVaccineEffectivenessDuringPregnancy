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

# Jobs to run ----
runGenerateCohort <- TRUE 
runCalculateOverlap <- TRUE 
runCohortTiming <- TRUE
runCountCodes <- TRUE                
runIndexEvents <- TRUE                 
runProfiling <- FALSE                   
runMatchedSampleLSC <- TRUE         
runIncidence <- FALSE                
runPrevalence <- FALSE               
sampleIncidencePrevalence <- 1000000 

exportResultsRData <- TRUE 
dropCohortTable <- TRUE

# zip file containing the csv with recommended codes
zipFileName <- "concept_recommended_20221006.zip"

# Run the study ------
source(here("RunStudy.R"))

