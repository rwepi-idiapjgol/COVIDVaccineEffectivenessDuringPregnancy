info(logger, "Prepare data for survival")
cdm$survival_raw <- cdm$matched |>
  addCohortName() |>
  addCohortIntersectDate(
    targetCohortTable = nco_table_name,
    indexDate = "cohort_start_date",
    # censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "nco_{cohort_name}"
  ) |>
  addCohortIntersectDate(
    targetCohortTable = outcomes_table_name,
    indexDate = "cohort_start_date",
    # censorDate = "cohort_end_date",
    targetDate = "cohort_start_date",
    order = "first",
    window = c(0, Inf),
    nameStyle = "study_{cohort_name}"
  ) |>
  compute(name = "survival_raw", temporary = FALSE)
