info(logger, "Instantiate codelists cohorts: ")
# info(logger, "  - Generate NCO codelists")
# source(here("Data", "codelistNCO.R"))

info(logger, "  - Load codelists")
load(here("Data", "nco.RData"))

# nco
info(logger, "  - NCO")
cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = nco_codelists,
  name = nco_table_name,
  limit = "all",
  requiredObservation = c(0, 0),
  end = 0,
  overwrite = TRUE
)



