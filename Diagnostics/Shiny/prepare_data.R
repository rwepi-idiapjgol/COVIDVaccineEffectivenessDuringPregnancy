# Get data ----
# All potential outputs
library(dplyr)
library(here)
library(tidyr)
library(dplyr)
library(here)
library(readr)
library(fresh)
library(visOmopResults)

source(here("functions_shiny.R"))

# Define result names
result_patterns <- c("cohort_definitions", "cohort_count", "code_counts", "cohort_overlap", 
                  "age_distribution", "time_distribution", "prevalence", "incidence", 
                  "index_events", "lsc_sample", "lsc_matched", "lsc_difference", "log",
                  "snapshot")
dataFolder <- "data"
data <- readData(here(dataFolder)) %>%
  mergeData(result_patterns)


# Tranform data for shiny ----
# Orphan code counts
if (!is.null(data$code_counts)) {
data$orphan_counts <- data$code_counts %>% 
  filter(strata_name == "recomendation") %>% 
  ungroup()  %>% 
  distinct() %>% 
  mutate(standard_concept_name = substr(additional_level, 1,
                                        unlist(gregexpr(';', additional_level))-2)) %>% 
  pivot_wider(names_from = variable_name, values_from = estimate_value) %>% 
  select("cdm_name", "cohort", "relationship_id",
         "standard_concept_id", "standard_concept_name", "Record count", "Person count")
}
# Code counts
if (!is.null(data$code_counts)) {
data$code_counts <- data$code_counts %>% 
  filter(strata_name == "original_codes") %>% 
  ungroup()  %>% 
  distinct() %>% 
  mutate(standard_concept_name = substr(additional_level, 1,
                                        unlist(gregexpr(';', additional_level))-2)) %>% 
  pivot_wider(names_from = variable_name, values_from = estimate_value) %>% 
  select("cdm_name", "cohort", 
         "standard_concept_id", "standard_concept_name", "Record count", "Person count")
}
# Index events
if (!is.null(data$index_events)) {
data$index_events <- data$index_events %>% 
  pivot_wider(names_from = variable_name, values_from = estimate) %>% 
  select(cdm_name, cohort_name, codelist_name, group_name, group_level, standard_concept_id, standard_concept_name, 
         source_concept_name, source_concept_id, domain_id,  
         cdm_name, `Record count`, `Person count`)
}
# Overlap
if (!is.null(data$cohort_overlap)) {
  if (data$cohort_overlap %>% dplyr::tally() != 0) {
    data$cohort_overlap <- data$cohort_overlap %>%
      ungroup() %>%
      inner_join(data$cohort_count %>%
                   select(cdm_name,
                          cohort_definition_id_x = cohort_definition_id,
                          cohort_name_x = cohort_name,
                          subject_counts_x = number_subjects),
                 by = c("cdm_name", "cohort_definition_id_x")) %>%
      inner_join(data$cohort_count %>%
                   select(cdm_name,
                          cohort_definition_id_y = cohort_definition_id,
                          cohort_name_y = cohort_name,
                          subject_counts_y = number_subjects),
                 by = c("cdm_name", "cohort_definition_id_y")) %>%
      rename("intersect_counts" = "intersect_count") %>% 
      mutate(across(contains("counts"), ~ as.numeric(.x))) 
  }
}
# Age distribution
if (!is.null(data$age_distribution)) {
data$age_distribution <- data$age_distribution %>%
  ungroup() %>%
  inner_join(data$cohort_count %>% select(cdm_name, cohort_definition_id, cohort_name)) %>%
  select(-cohort_definition_id)
}
# Time distribution
if (!is.null(data$time_distribution)) {
to_pivot <- colnames(data$time_distribution)[!colnames(data$time_distribution) %in%
                                               c("sex", "cohort_name", "cdm_name", "cohort_definition_id")]
data$time_distribution <- tibble(covariate = c("age", "prior_observation", "future_observation")) %>%
  left_join(
    data$time_distribution %>%
      ungroup() %>%
      inner_join(data$cohort_count %>% select(cdm_name, cohort_definition_id, cohort_name)) %>%
      select(-cohort_definition_id) %>%
      mutate(across(tidyr::everything(), ~ as.character(.x))) %>%
      pivot_longer(cols = dplyr::all_of(to_pivot), values_to = "estimate_value") %>%
      mutate(
        estimate_type = case_when(
          grepl("Min", name) ~ "min",
          grepl("Max", name) ~ "max",
          grepl("Median", name) ~ "median",
          grepl("Mean", name) ~ "mean",
          grepl("Sd", name) ~ "sd",
        ),
        covariate = gsub("_Min|_Max|_Median|_Sd|_Mean", "", name),
        estimate_value = round(as.numeric(estimate_value), 3)
      ) %>%
      select(-name)
  ) %>%
  select(cdm_name, cohort_name, sex, covariate, estimate_type, estimate_value)
}

# if (!is.null(data$lsc_matched)) {
# process_columns <- function(df) {
#   df %>%
#     splitAdditional() %>%
#     distinct() %>%
#     mutate(
#       estimate_name = paste0("matched_", estimate_name),
#       estimate = as.numeric(estimate_value)
#     ) %>%
#     select(-estimate_value, -estimate_name, -estimate, -estimate_type) %>%
#     summarise(across(everything(), ~n_distinct(.) > 1)) %>%
#     select(where(~.)) %>%
#     names()
# }
# 
# non_numeric_cols <- process_columns(data$lsc_matched)
# non_numeric_cols_sample <- process_columns(data$lsc_sample)
# 
# data$lsc_table <- data$lsc_matched %>% 
#   splitStrata() %>% 
#   splitAdditional() %>%
#   distinct() %>%
#   mutate(
#     estimate_name = paste0("matched_", estimate_name),
#     estimate_value = as.numeric(estimate_value)
#   ) %>% 
#   select(!estimate_type) %>% 
#   pivot_wider(id_cols = dplyr::all_of(c(non_numeric_cols,"cdm_name")),
#     names_from = estimate_name, values_from = estimate_value) %>% 
#   left_join(
#     data$lsc_sample %>% 
#       splitStrata() %>% 
#       splitAdditional() %>%
#       distinct() %>%
#       mutate(
#         estimate_name = paste0("sample_", estimate_name),
#         estimate_value = as.numeric(estimate_value)
#       ) %>% 
#       pivot_wider(id_cols = dplyr::all_of(c(non_numeric_cols_sample,"cdm_name")),
#                   names_from = estimate_name, values_from = estimate_value)) %>% 
#   mutate(
#     difference_count = (sample_count - matched_count)/matched_count,
#     difference_percentage = (sample_percentage - matched_percentage)/matched_percentage
#   ) %>% 
#   select(
#     cdm_name, cohort_name = group_level, concept_name = variable_name, table_name, concept,
#     window = variable_level, matched_count, matched_percentage, sample_count, sample_percentage, 
#     difference_count, difference_percentage
#   )
# }

if (!is.null(data$prevalence)) {
  data$prevalence <- data$prevalence %>% 
    mutate(across(all_of(c("prevalence", "prevalence_95CI_lower", "prevalence_95CI_upper")), ~ as.numeric(.x)))
}

if (!is.null(data$incidence)) {
  data$incidence <- data$incidence %>% 
    mutate(across(all_of(c("incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper")), ~ as.numeric(.x)))
}
