info(logger, "Start survival analyses")
info(logger, "1) Relative risk estimates")
# For loop settings
outcomes <- colnames(cdm$survival_raw)
outcomes <- outcomes[grepl("nco_|study_", outcomes)]
study_ends <- c("cohort_end_date", "cohort_end_date_pregnancy")
windows <- list(
  c(0, 14), c(15, 28), c(15, 90), c(29, 90), c(29, 180), c(91, 180), c(181, 365),
  c(366, Inf), c(15, 180), c(15, 365), c(15, Inf), c(0, Inf) # last one only for NCO
)
analyses <- c("main")

results <- list()
k <- 1
for (analysis in analyses) {
  info(logger, paste0(" - analysis: ", analysis))
  for (studyEnd in study_ends) {
    info(logger, paste0("  - study end: ", studyEnd))
    for (window.k in 1:length(windows)) {
      # data for window
      window <- windows[[window.k]]
      info(logger, paste0("   - window: ", paste0(as.character(window), collapse = "_")))
      survival_window <- cdm$survival_raw |>
        trimDates(interval = window, outcomes = outcomes, endData = studyEnd, analysis = analysis) |>
        compute()
      # set outcomes to evaluate depending on window
      if (window[1] == 0 & is.infinite(window[2])) {
        outcomes.k <- outcomes[grepl("nco_", outcomes)]
      } else {
        outcomes.k <- outcomes[grepl("study_", outcomes)]
      }
      for (outcome in outcomes.k) {
        # survival format for outcome
        survival_data <- survivalFormat(survival_window, outcome) |>
          mutate(overall = "overall")
        # get estimates
        if(grepl("nco", outcome)) {
          results[[k]] <- estimateSurvival(
            data = survival_data,
            group = "cohort_name", c("overall", "vaccine_brand", "trimester"),
            cox = TRUE, binomial = FALSE, coxTime = FALSE, rateratio = FALSE
          ) |>
            mutate(
              cdm_name = cdmName(cdm),
              variable_name = "nco",
              variable_level = gsub("nco_", "", outcome),
              window = paste0(as.character(window), collapse = "_"),
              analysis = analysis,
              study_end = studyEnd
            )
        } else {
          results[[k]] <- estimateSurvival(
            data = survival_data,
            group = "cohort_name", strata = c("overall", "vaccine_brand", "trimester"),
            cox = TRUE, binomial = FALSE, coxTime = TRUE
          ) |>
            mutate(
              cdm_name = cdmName(cdm),
              variable_name = "study",
              variable_level = gsub("study_", "", outcome),
              window = paste0(as.character(window), collapse = "_"),
              analysis = analysis,
              study_end = studyEnd
            )
        }
        k <- k + 1
      }
    }
  }
}

info(logger, "2) Survival estimates")
# results survival
km_results_obs <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "matched",
  outcomeCohortTable = "outcomes",
  outcomeDateVariable = "cohort_start_date",
  outcomeWashout = 1,
  censorOnCohortExit = TRUE,
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
)

cdm$matched_preg <- cdm$matched %>%
  mutate(cohort_end_date = cohort_end_date_pregnancy) %>%
  compute(name = "matched_preg", temporary = FALSE, overwrite = TRUE)
km_results_pregnancy <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "matched_preg",
  outcomeCohortTable = "outcomes",
  outcomeDateVariable = "cohort_start_date",
  outcomeWashout = 1,
  censorOnCohortExit = TRUE,
  strata = list(c("vaccine_brand", "exposed"), c("trimester", "exposed"), "exposed"),
) %>%
  mutate(result_id = 2)

# export results ----
survival_results <- results |> bind_rows() |>
  uniteAdditional(c("exposed", "window", "analysis", "study_end")) |>
  mutate(
    result_id = 1,
    package_name = "StudyCode",
    package_version = today()
  )

# write
write_csv(
  survival_results,
  file = here(output_folder, paste0("relative_risk_", cdmName(cdm), ".csv"))
)

# write
write_csv(
  km_results_obs %>% bind_rows(km_results_pregnancy),
  file = here(output_folder, paste0("kaplan_meier_", cdmName(cdm), ".csv"))
)
