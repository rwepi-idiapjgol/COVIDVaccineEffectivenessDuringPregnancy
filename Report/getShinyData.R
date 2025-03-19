# Libraries ----
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(stringr)
library(visOmopResults)
library(omopgenerics)

# Load functions ----
source(here("functions.R"))

# Read results ----
result_patterns <- c(
  "cdm_snapshot", "characteristics", "cohort_stats", "cohort_counts",
  "attrition", "matching_summary", "relative_risk", "vaccine_records_censor",
  "kaplan_meier", "censoring"
)
pre_data <- readData(here("data")) |> mergeData(result_patterns)

# Shiny format ----
data <- list()
data$snapshot <- pre_data$cdm_snapshot
data$cohort_count <- pre_data$cohort_counts |>
  select("cdm_name", "cohort_group", "cohort_name", "number_records", "number_subjects") |>
  mutate(across(starts_with("number"), ~as.numeric(.x)))
data$population_attrition <- pre_data$attrition |>
  select(!"cohort_definition_id") |>
  relocate(c("cdm_name", "cohort_name")) |>
  mutate(
    reason_id = as.numeric(reason_id),
    across(starts_with("number"), ~niceNum(.x)),
    across(starts_with("excluded"), ~niceNum(.x))
  ) |>
  niceCohortName()
data$population_count <- pre_data$relative_risk |>
  filter(
    result_type == "survival_stats",
    estimate_name %in% c("num_control", "num_exposed")
  ) |>
  splitAll() |>
  filter(window == "0_Inf") |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester", "estimate_name",
    "estimate_value"
  ) |>
  distinct() |>
  mutate(estimate_value = as.numeric(estimate_value)) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  uniteStrata(c = c("vaccine_brand", "trimester")) |>
  niceCohortName()
data$weekly_counts <- pre_data$matching_summary |>
  mutate(cohort = paste0(.data$population, "_", .data$covid_cohort)) |>
  select(
    "cdm_name", "cohort_name" = "cohort", "week_start" = "matching_day",
    "elegible_exposed" = "exposed_pre", "matched_exposed" = "exposed_post",
    "elegible_unexposed" = "unexposed_pre",  "matched_unexposed" = "unexposed_post"
  ) |>
  mutate(
    across(contains("exposed"), ~ as.numeric(.x)),
    week_start = as.Date(week_start)
  ) |>
  niceCohortName() |>
  select(cdm_name, comparison, covid_definition, week_start, elegible_exposed, matched_exposed, elegible_unexposed, matched_unexposed)
data$index_date <- pre_data$cohort_stats |>
  filter(result_type == "index_date") |>
  mutate("index_date" = as.Date(variable_level)) |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester", "index_date",
    "counts" = "estimate_value"
  ) |>
  mutate(counts = as.numeric(counts)) |>
  uniteStrata(c = c("vaccine_brand", "trimester")) |>
  niceCohortName() |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, index_date, counts)
data$reenrollment <- pre_data$cohort_stats |>
  filter(result_type == "recontributions") |>
  splitAll() |>
  select(
    "cdm_name", "cohort_name", "vaccine_brand", "trimester",
    "reenrollments" = "estimate_value"
  ) |>
  uniteStrata(c = c("vaccine_brand", "trimester")) |>
  mutate(reenrollments = as.numeric(reenrollments)) |>
  niceCohortName() |>
  left_join(
    data$population_count,
    by = c("cdm_name", "comparison", "strata_name", "strata_level", "covid_definition")
  ) |>
  mutate(
    count = reenrollments,
    percentage = reenrollments/(num_control + num_exposed - reenrollments) * 100
  ) |>
  select(!c("reenrollments", "num_control", "num_exposed"))
data$vaccine_distribution <- pre_data$cohort_stats |>
  filter(result_type %in% c("vaccine_uptake")) |>
  splitGroup() |>
  splitAdditional() |>
  rename("date" = "variable_level", "vaccine_dose" = "variable_name") |>
  niceCohortName() |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    date = as.Date(date),
    vaccine_dose = gsub("dose number: ", "", vaccine_dose),
    exposed = if_else(exposed == "1", "exposed", "comparator")
  ) |>
  arrange(date) |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, exposed, vaccine_dose, date, estimate_value)
data$pregnant_vaccination <- pre_data$cohort_stats |>
  filter(result_type %in% c("pregnant_vaccination")) |>
  splitGroup() |>
  splitAdditional() |>
  rename("vaccine_dose" = "variable_name") |>
  niceCohortName() |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    vaccine_dose = gsub("dose number: ", "", vaccine_dose),
    exposed = if_else(exposed == "1", "exposed", "comparator")
  ) |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, exposed, vaccine_dose, estimate_value)
data$baseline <- pre_data$characteristics |>
  filter(!result_type %in% c("summarised_large_scale_characteristics", "large_scale_differences")) |>
  splitStrata() |>
  filter(exposed != "overall") |>
  select(-starts_with("additional")) |>
  uniteAdditional(cols = c("exposed")) |>
  uniteStrata(cols = c("vaccine_brand", "trimester")) |>
  niceCohortName(col = "group_level", removeCol = FALSE) |>
  mutate(additional_level = case_when(additional_level == "1" ~ "Exposed", additional_level == "0" ~ "Unexposed", .default = additional_level))
data$large_scale <- pre_data$characteristics |>
  filter(result_type %in% c("summarised_large_scale_characteristics")) |>
  splitGroup() |>
  splitAdditional() |>
  select(!c("result_id", "result_type", "package_name", "package_version",
            "estimate_type")) |>
  rename("window" = "variable_level", "concept_name" = "variable_name") |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    exposed = case_when(
      exposed == "0" ~ "unexposed",
      exposed == "1" ~ "exposed",
      .default = exposed
    )
  ) |>
  niceCohortName()
data$smd <- pre_data$characteristics |>
  filter(result_type %in% c("large_scale_differences")) |>
  splitGroup() |>
  splitAdditional() |>
  mutate(smd = as.numeric(estimate_value), asmd = abs(smd),
         smd = if_else(variable_level == "propensity_score", NA, smd)) |>
  rename("window" = "variable_level", "concept_name" = "variable_name") |>
  select(!c("result_id", "result_type", "package_name", "package_version",
            "estimate_type", "estimate_name", "estimate_value")) |>
  niceCohortName() |>
  relocate(c("comparison", "covid_definition"), .after = "cdm_name")
data$survival_summary <- pre_data$relative_risk |>
  filter(result_type == "survival_stats") |>
  newSummarisedResult() |>
  mutate(estimate_name = gsub("num", "count", estimate_name)) |>
  splitGroup() |>
  splitAdditional() |>
  filter(!(grepl("control", estimate_name) & exposed == 1)) |>
  filter(!(grepl("exposed", estimate_name) & exposed == 0)) |>
  mutate(estimate_type = if_else(grepl("count", estimate_name), "integer", estimate_type)) |>
  mutate(
    estimate_name = gsub("_control|_exposed", "", estimate_name),
    exposed = if_else(exposed == "1", "exposed", "unexposed")
  ) |>
  select(!c("result_id")) |>
  rename("outcome" = "variable_level") |>
  filter(estimate_name != "mean") |>
  mutate(estimate_value = as.numeric(estimate_value)) |>
  group_by(cdm_name, cohort_name, strata_name, strata_level, variable_name, outcome, window, analysis, study_end, exposed) |>
  mutate(
    suppress_group = if_else(any(grepl("count", estimate_name) & estimate_value < 5 & estimate_value > 0), TRUE, FALSE),
    suppress_count = if_else(grepl("count", estimate_name) & estimate_value < 5 & estimate_value > 0, TRUE, FALSE)
    ) |>
  ungroup() |>
  mutate(
    estimate_value = case_when(
      suppress_group & suppress_count ~ NA,
      suppress_group & !suppress_count & !grepl("count", estimate_name) ~ NA,
      .default = estimate_value
      ),
    exposed_censoring = if_else(analysis == "main", "none", "3rd/4rt dose")
  ) |>
  rename("followup_end" = "study_end") |>
  select(!c("suppress_group", "suppress_count", "result_type", "package_name", "package_version", "analysis")) |>
  niceCohortName() |>
  niceOutcomeName() |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, window, followup_end, exposed_censoring, exposed, variable_name, outcome, delivery_excluded, estimate_name, estimate_type, estimate_value) |>
  distinct()
data$risk <- pre_data$relative_risk |>
  filter(grepl("cox|irr", result_type)) |>
  splitGroup() |>
  splitAdditional() |>
  rename("outcome" = "variable_level", "regression" = "result_type") |>
  select(!c("package_name", "package_version", "result_id")) |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    exposed_censoring = if_else(analysis == "main", "none", "3rd/4rt dose")
  ) |>
  select(
    c("cdm_name", "cohort_name", "strata_name", "strata_level", "regression",
      "exposed_censoring","followup_end" = "study_end", "window", "outcome", "variable_name",
      "estimate_type", "estimate_name", "estimate_value")
  ) |>
  niceCohortName() |>
  niceOutcomeName() |>
  relocate(c("comparison", "covid_definition"), .after = "cdm_name") |>
  mutate(window = factor(window, levels = c("0_Inf", "0_14", "15_Inf", "15_28", "15_90", "15_180", "15_365", "29_90", "29_180", "91_180", "181_365", "366_Inf")))
data$population_count <- data$population_count |>
  mutate(total = num_control + num_exposed) |>
  select(cdm_name, comparison, covid_definition, strata_name, strata_level, total, num_control, num_exposed)
data$kaplan_meier <- pre_data$kaplan_meier |>
  filter(strata_name != "overall") |>
  niceCohortName(col = "group_level", removeCol = FALSE) |>
  filter(result_type == "survival_estimate") |>
  splitAdditional() |>
  splitStrata() |>
  uniteStrata(cols = c("vaccine_brand", "trimester")) |>
  niceOutcomeName(col = "variable_level") |>
  mutate(
    estimate_value = as.numeric(estimate_value),
    followup_end = if_else(result_id == 1, "cohort_end_date", "cohort_end_date_pregnancy"),
    time = as.numeric(time),
    Cohort = if_else(exposed == "0", "Unexposed", "Exposed"),
    strata_level = factor(strata_level,
                          levels = c("overall", "pfizer", "moderna", "T1", "T2", "T3"),
                          label = c("Overall", "Pfizer", "Moderna", "Trimester 1", "Trimester 2", "Trimester 3"))
  ) |>
  select(-c("result_id", "result_type", "package_name", "package_version", "group_name", "group_level", "analysis_type", "variable_name", "variable_level", "estimate_type", "exposed")) |>
  pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
data$censoring <- pre_data$censoring |>
  niceCohortName(col = "cohort_name", removeCol = TRUE) |>
  mutate(
    n = as.numeric(n),
    N = if_else(n < 5 & n != 0, "<5", niceNum(n)),
    Reason = stringr::str_to_sentence(gsub("_", " ", reason)),
    "Mean (SD)" = paste0(niceNum(mean, 2), " (", niceNum(sd, 2), ")"),
    "Median (Q25-Q75)" = paste0(niceNum(median, 2), " (", niceNum(q25, 2), " - ", niceNum(q75, 2), ")")
  ) |>
  select("CDM name" = "cdm_name", "Comparison" = "comparison",
         "Covid definition" = "covid_definition",
         "Follow-up end" = "followup_end",
         "Reason", "N", "Mean (SD)", "Median (Q25-Q75)")

# Save shiny data ----
save(data, file = here("shinyData.Rdata"))

