getId <- function(x, name) {
  settings(x) |>
    filter(.data$cohort_name == name) |>
    pull("cohort_definition_id")
}


generateVisitRelatedOutcomes <- function(codes, window, name, attritionReason) {
  # covid visit cohort
  covid_visit <- cdm$temp_covid |>
    inner_join(
      cdm$visit_occurrence |>
        filter(visit_concept_id %in% c(codes)) |>
        filter(visit_start_date >= study.start)  |>
        select(subject_id = person_id, visit_start_date) |>
        distinct(),
      by = "subject_id"
    ) %>%
    mutate(diff_days = !!datediff("cohort_start_date", "visit_start_date")) |>
    filter(diff_days >= !!window[1] & diff_days <= !!window[2]) |>
    compute()
  cdm[[paste0("temp_", name)]] <- covid_visit |>
    select(-visit_start_date, -diff_days) |>
    distinct() |>
    compute(name = paste0("temp_", name), temporary = FALSE) |>
    recordCohortAttrition(reason = attritionReason) |>
    newCohortTable(cohortSetRef = settings(cdm$temp_covid) |>
                     mutate(cohort_name = paste0(name, "_", cohort_name)))
  # covid visit cohort - delivery date
  cdm[[paste0("temp_", name, "_delivery")]] <- covid_visit |>
    addTableIntersectFlag(
      tableName = "temp_delivery",
      window = c(-2,2),
      indexDate = "visit_start_date",
      targetStartDate = "pregnancy_end_date",
      targetEndDate = NULL,
      nameStyle = "is_delivery_date") |>
    filter(is_delivery_date == 0) |>
    select(-visit_start_date, -diff_days) |>
    distinct() |>
    compute(name = paste0("temp_", name, "_delivery"), temporary = FALSE) |>
    recordCohortAttrition(reason = paste0(attritionReason, " (not delivery)")) |>
    newCohortTable(cohortSetRef = settings(cdm$temp_covid) |>
                     mutate(cohort_name = paste0(name, "_no_delivery_", cohort_name)))
  return(cdm)
}

addMatchingAgeGroup <- function(x) {
  return(x |>
           mutate(
             maternal_age = case_when(
               .data$age >= 12 & .data$age < 14 ~ 12,
               .data$age >= 14 & .data$age < 16 ~ 14,
               .data$age >= 16 & .data$age < 18 ~ 16,
               .data$age >= 18 & .data$age < 20 ~ 18,
               .data$age >= 20 & .data$age < 22 ~ 20,
               .data$age >= 22 & .data$age < 24 ~ 22,
               .data$age >= 24 & .data$age < 26 ~ 24,
               .data$age >= 26 & .data$age < 28 ~ 26,
               .data$age >= 28 & .data$age < 30 ~ 28,
               .data$age >= 30 & .data$age < 32 ~ 30,
               .data$age >= 32 & .data$age < 34 ~ 32,
               .data$age >= 34 & .data$age < 36 ~ 34,
               .data$age >= 36 & .data$age < 38 ~ 36,
               .data$age >= 38 & .data$age < 40 ~ 38,
               .data$age >= 40 & .data$age < 42 ~ 40,
               .data$age >= 42 & .data$age < 44 ~ 42,
               .data$age >= 44 & .data$age < 46 ~ 44,
               .data$age >= 46 & .data$age < 48 ~ 46,
               .data$age >= 48 & .data$age < 50 ~ 48,
               .data$age >= 50 & .data$age < 52 ~ 50,
               .data$age >= 52 & .data$age < 54 ~ 52,
               .data$age >= 54 & .data$age < 56 ~ 54
             )
           )
  )
}

addRegion <- function(x, database_name) {
  if(database_name != "CPRD") {
    x |>
      left_join(
        cdm$person |>
          left_join(cdm$location, by = "location_id") |>
          select(subject_id = person_id, region = location_source_value),
        by = "subject_id"
      )
  } else {
    x |>
      left_join(
        cdm$person |>
          left_join(cdm$care_site, by = "care_site_id") |>
          select(person_id, location_id = location_id.y) |>
          left_join(cdm$location, by = "location_id") |>
          select(subject_id = person_id, region = location_source_value),
        by = "subject_id"
      )
  }

}

pregnantMatchingTable <- function(sourceTable, covidId, weekStart, weekEnd, excludeControls, objective_id, days.booster) {
  temp <- sourceTable |>
    mutate(week_start = as.Date(weekStart), week_end = as.Date(weekEnd)) |>
    # pregnant at week.k
    filter(
      pregnancy_start_date <= week_start &  # start before the week
        pregnancy_end_date >= week_end      # end after the week
    ) |>
    # in observation at week.k
    filter(
      cohort_start_date <= week_start &  # start before the week
        cohort_end_date >= week_end      # end after the week
    ) |>
    # elegible for enrollment
    filter(enrollment_end_date >= week_start) |> # within pregnancy exposure time
    # no covid-19 in the last three months from week start date
    addCohortIntersectFlag(
      targetCohortTable = "covid",
      targetCohortId = covidId,
      window = c(-90, -1),
      indexDate = "week_start",
      nameStyle = "covid"
    ) |>
    filter(covid == 0) |>
    select(-covid) |>
    # classify exposed - unexposed
    mutate(exposed = if_else(index_vaccine_date >= week_start & index_vaccine_date <= week_end, 1, 0),
           exposed = if_else(is.na(exposed) | is.na(index_vaccine_date), 0, exposed)) |>
    # exclude if not pfizer or moderna
    filter(!(exposed == 1 & index_vaccine_brand %in% c("janssen", "astrazeneca", "unkown"))) |>
    # within enrollment oregnant period
    filter(!(exposed == 1 & index_vaccine_date > enrollment_end_date)) |>
    # exclude if exposed before week start
    filter(index_vaccine_date >= week_start | is.na(index_vaccine_date)) |>
    # covid during the week
    addCohortIntersectDate(
      targetCohortTable = "covid",
      targetCohortId = covidId,
      window = c(0, Inf),
      order = "first",
      indexDate = "week_start",
      censorDate = "week_end",
      nameStyle = "covid_date_week"
    ) %>%
    # Exclude previous controls if their exposure did not change
    {if (!is.null(excludeControls))
      anti_join(., excludeControls, by = c("subject_id", "exposed"), copy = TRUE)
      else . } %>%
    compute()
  # exclude covid before exposure
  temp <- temp |>
    anti_join(temp |> filter(exposed == 1, covid_date_week < index_vaccine_date), by = colnames(temp)) |>
    compute()
  return(temp)
}

matchItDataset <- function(x, objective_id, asmd = FALSE) {
  disselect <- c(
    "cohort_start_date", "cohort_end_date", "observation_period_start_date",
    "covid_date_week", "week_start", "week_end", "index_vaccine_date",
    "index_vaccine_brand", "pregnancy_start_date", "pregnancy_end_date",
    "enrollment_end_date", "days_previous_vaccine", "previous_vaccine_date"
  )
  if (objective_id == 1) {
    disselect <- c(disselect, "previous_vaccine_brand", "days_previous_vaccine_band")
  }
  x <- x %>%
    mutate(
      trimester = cut(
        as.numeric(!!datediff("pregnancy_start_date", "week_start")),
        c(0, 90, 180, 330),
        include.lowest = TRUE
      ),
      # gestational_age = cut(
      #   as.numeric(!!datediff("pregnancy_start_date", "week_start")),
      #   !!c(0, 90, 180, 330),
      #   include.lowest = TRUE)
      gestational_age = cut(
        as.numeric(!!datediff("pregnancy_start_date", "week_start")),
        !!c(seq(0, 30*9, 9), 303),
        include.lowest = TRUE)
    ) |>
    addTableIntersectCount(
      tableName = "visit_occurrence",
      indexDate = "pregnancy_start_date",
      window = list("m365_m1" = c(-365, 0)),
      targetStartDate = "visit_start_date",
      targetEndDate = NULL,
      nameStyle = "visits_year_before_pregnancy"
    ) |>
    compute(name = "temp", temporary = FALSE, overwrite = TRUE) |>
    addCohortIntersectCount(
      targetCohortTable = "covid",
      targetCohortId = covid_id,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-365, -1)),
      nameStyle = "{cohort_name}"
    ) |>
    addCohortIntersectCount(
      targetCohortTable = other_vaccines_table_name,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-365, -1)),
      nameStyle = "{cohort_name}"
    ) |>
    addCohortIntersectFlag(
      targetCohortTable = ps_covariates_table_name,
      indexDate = "week_start",
      targetStartDate = "cohort_start_date",
      targetEndDate = "cohort_end_date",
      window = list(c(-Inf, -1)),
      nameStyle = "{cohort_name}"
    ) |>
    compute(name = "temp", temporary = FALSE, overwrite = TRUE) |>
    addCohortIntersectCount(
      targetCohortTable = "mother_table",
      indexDate = "pregnancy_start_date",
      window = list(c(-Inf, -1)),
      nameStyle = "previous_pregnancies"
    ) %>%
    mutate(
      previous_observation = as.numeric(!!datediff("observation_period_start_date", "week_start")),
      days_previous_vaccine = as.numeric(!!datediff("previous_vaccine_date", "week_start")),
      days_previous_vaccine_band = cut(
        days_previous_vaccine,
        !!seq(0, 10000, 30),
        include.lowest = TRUE
      )
    ) |>
    collect()

  if (!asmd) {
    x <- x |> select(!any_of(disselect))
  }

  return(x)
}

addAttritionReason <- function(attrition_table, reason, x) {
  if (length(x) == 0) {
    n_records <- 0
    n_subjects <- 0
  } else {
    n_records <- nrow(x)
    n_subjects <-  nrow(x |> distinct(subject_id))
  }

  new_attrition <- attrition_table |>
    bind_rows(
      tibble(
        cohort_definition_id = unique(attrition_table$cohort_definition_id),
        number_records = n_records,
        number_subjects = n_subjects,
        reason_id = max(attrition_table$reason_id) + 1,
        reason = reason,
        excluded_records = attrition_table$number_records[attrition_table$reason_id == max(attrition_table$reason_id)] - n_records,
        excluded_subjects = attrition_table$number_subjects[attrition_table$reason_id == max(attrition_table$reason_id)] - n_subjects
      )
    )
  return(new_attrition)
}


censorExposedPair <- function(x) {
  x |>
    left_join(
      x |>
        filter(control_censored) |>
        select(cohort_definition_id, match_id, new_cohort_end_date = cohort_end_date),
      by = c("cohort_definition_id", "match_id")
    ) |>
    mutate(cohort_end_date = if_else(
      new_cohort_end_date > cohort_end_date,
      cohort_end_date,
      new_cohort_end_date
    )) |>
    select(-new_cohort_end_date)
}

trimDates <- function(x, interval, outcomes, endData, analysis) {
  if (analysis == "sensitivity") {
    # end data adjusted to sensitivity
    x <- x |>
      mutate(!!endData := if_else(censor_date < .data[[endData]] & !is.na(censor_date), censor_date, .data[[endData]]))
  }
  # set end date
  if (is.infinite(interval[2])) {
    x <- x |> mutate("end_date" = .data[[endData]])
  } else {
    x <- x %>%
      mutate("end_date" = !!dateadd(date = "cohort_start_date", number = interval[2]))
  }

  # set start date
  x <- x |>
    rename("end_data" := !!endData) %>%
    mutate(
      "start_date" = !!dateadd(date = "cohort_start_date", number = interval[1]),
      "start_date" = as.Date(start_date)
    ) |>
    select(all_of(c(
      "cohort_name", "subject_id", "match_id", "exposed", "trimester",
      "vaccine_brand", "end_date", "start_date", "end_data", "age",
      outcomes
    )))

  # match to exclude (not in observation)
  x <- x |>
    anti_join(
      x |>
        filter(start_date > end_data) |> # window starts out of observation
        distinct(cohort_name, match_id),
      by = c("cohort_name", "match_id")
    ) |>
    # reset end date
    mutate(end_date = if_else(end_date > end_data, end_data, end_date))

  return(x)
}

survivalFormat <- function(x, out) {
  # exclusion if match outcome before
  x <- x |>
    anti_join(
      x |>
        filter(start_date > .data[[out]]) |> # outcome before
        distinct(cohort_name, match_id),
      by = c("cohort_name", "match_id")
    )
  x <- x %>%
    mutate(
      # status 1 if outcome in window
      status = if_else(.data[[out]] >= start_date & .data[[out]] <= end_date, 1, 0),
      status = if_else(is.na(status), 0, status),
      time = if_else(status == 1, !!datediff("start_date", out), !!datediff("start_date", "end_date")),
    ) |>
    select(all_of(c(
      "cohort_name", "subject_id", "match_id", "exposed", "trimester", "vaccine_brand",
      "status", "time", "start_date", "age"
    ))) |>
    compute()
  return(x)
}

estimateSurvival <- function(data, group, strata,
                             cox = TRUE, binomial = FALSE, rateratio = TRUE,
                             coxTime = TRUE, coxSandwich = TRUE) {
  results <- list()
  groupLevel = unique(data |> pull(.data[[group]]))
  k <- 1
  for (group.k in groupLevel) { # group level
    for (strata.k in strata) { # strata name
      strataLevels <- unique(data |> pull(strata.k))
      for(strataLevel.k in strataLevels) { # strata level
        # # for double robust estimation
        # covariates <-  smd |>
        #   filter(estimate_name == "smd", as.numeric(estimate_value) > 0.1 | as.numeric(estimate_value) < -0.1) |>
        #   rename("concept_id" = "additional_level") |>
        #   inner_join(cdm$concept, by = "concept_id", copy = TRUE) |>
        #   filter(domain_id %in% c("Drug", "Condition")) |>
        #   pull(concept_id) |>
        #   unique()
        # names(covariates) <- covariates

        # data
        data.k <- data |>
          filter(cohort_name == group.k, .data[[strata.k]] == strataLevel.k) |>
          collect() |>
          mutate(start_date = month(.data$start_date))

        # formula
        # if (length(covariates) == 0) {
        covariates_formula <- "exposed"
        # # } else {
        #
        #   windowFlag <- unique(smd |> pull("variable_level"))
        #   cdm$matched_asmd <- cdm$matched |>
        #     addConceptIntersectFlag(
        #       conceptSet = as.list(covariates),
        #       window = gsub("i", "I", windowFlag) |> strsplit(" to ") |> lapply(as.numeric)
        #     ) |>
        #     compute(name = "matched_asmd", temporary = FALSE)
        #   covariates_formula <- c("exposed", covariates)
        #   covariates_formula <- paste0(covariates_formula, collapse = " + ")
        # }

        # cox ----
        if (cox) {
          tryCatch({
            formula <- as.formula(paste0("Surv(time, status) ~ ", covariates_formula))
            coxRegression <- coxph(formula,  data = data.k)
            results[[k]] <-  summary(coxRegression)$coefficients |>
              as_tibble(rownames = "variable") |>
              select(
                "variable", "coef", "exp_coef" = "exp(coef)",
                "se_coef" = "se(coef)", "z", "p" = "Pr(>|z|)"
              ) |>
              filter(variable == "exposed") |>
              left_join(
                summary(coxRegression)$conf.int |>
                  as_tibble(rownames = "variable") |>
                  select("variable", "lower_ci" = "lower .95", "upper_ci" = "upper .95"),
                by = "variable"
              ) |>
              regressionToSummarised(
                type = "cox", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # cox sandwitch ----
        if (coxSandwich) {
          tryCatch({
            formula <- as.formula(paste0("Surv(time, status) ~ ", covariates_formula))
            coxRegression <- coxph(formula,  data = data.k, robust = TRUE)
            results[[k]] <-  summary(coxRegression)$coefficients |>
              as_tibble(rownames = "variable") |>
              select(
                "variable", "coef", "exp_coef" = "exp(coef)",
                "se_coef" = "se(coef)", "z", "p" = "Pr(>|z|)"
              ) |>
              filter(variable == "exposed") |>
              left_join(
                summary(coxRegression)$conf.int |>
                  as_tibble(rownames = "variable") |>
                  select("variable", "lower_ci" = "lower .95", "upper_ci" = "upper .95"),
                by = "variable"
              ) |>
              regressionToSummarised(
                type = "cox-sandwich", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # cox time ----
        if (coxTime) {
          tryCatch({
            formula <- as.formula(paste0("Surv(time, status) ~ ", paste0(covariates_formula, " + start_date")))
            coxRegression <- coxph(formula,  data = data.k)
            results[[k]] <-  summary(coxRegression)$coefficients |>
              as_tibble(rownames = "variable") |>
              select(
                "variable", "coef", "exp_coef" = "exp(coef)",
                "se_coef" = "se(coef)", "z", "p" = "Pr(>|z|)"
              ) |>
              filter(variable == "exposed") |>
              left_join(
                summary(coxRegression)$conf.int |>
                  as_tibble(rownames = "variable") |>
                  select("variable", "lower_ci" = "lower .95", "upper_ci" = "upper .95"),
                by = "variable"
              ) |>
              regressionToSummarised(
                type = "cox-time", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # cox sandwich + time ----
        if (coxSandwich & coxTime) {
          tryCatch({
            formula <- as.formula(paste0("Surv(time, status) ~ ", paste0(covariates_formula, " + start_date")))
            coxRegression <- coxph(formula,  data = data.k, robust = TRUE)
            results[[k]] <-  summary(coxRegression)$coefficients |>
              as_tibble(rownames = "variable") |>
              select(
                "variable", "coef", "exp_coef" = "exp(coef)",
                "se_coef" = "se(coef)", "z", "p" = "Pr(>|z|)"
              ) |>
              filter(variable == "exposed") |>
              left_join(
                summary(coxRegression)$conf.int |>
                  as_tibble(rownames = "variable") |>
                  select("variable", "lower_ci" = "lower .95", "upper_ci" = "upper .95"),
                by = "variable"
              ) |>
              regressionToSummarised(
                type = "cox-sandwich-time", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # rateratio ----
        if (rateratio) {
          tryCatch({
            dataIRR <- data.k |>
              group_by(exposed) |>
              summarise(person_days = sum(time), cases = sum(status))
            res <- rateratio(
              dataIRR$cases[dataIRR$exposed == 1],
              dataIRR$cases[dataIRR$exposed == 0],
              dataIRR$person_days[dataIRR$exposed == 1],
              dataIRR$person_days[dataIRR$exposed == 1]
            )

            results[[k]] <- tibble(
              exp_coef = res$estimate,
              lower_ci = res$conf.int[1],
              upper_ci = res$conf.int[2]
            ) |>
              mutate(variable = "exposed") |>
              regressionToSummarised(
                type = "irr", groupLevel = group.k, cols = c("exp_coef", "lower_ci", "upper_ci"),
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # binomial ----
        if (binomial) {
          # regression
          tryCatch({
            log <- glm(as.formula(paste0("status ~ ", covariates_formula)),
                       data = data.k, family = binomial(link = log))
            results[[k]] <- summary(log)$coefficients |>
              as_tibble(rownames = "variable") |>
              filter(variable == "exposed") |>
              rename("coef" = "Estimate", "se_coef" = "Std. Error",
                     "z" = "z value", "p" = "Pr(>|z|)") |>
              mutate("exp_coef" = exp(coef)) |>
              inner_join(
                confint(log) |>
                  as_tibble(rownames = "variable") |>
                  mutate(lower_ci = exp(`2.5 %`), upper_ci = exp(`97.5 %`)) |>
                  select(variable, lower_ci, upper_ci),
                by = "variable") |>
              regressionToSummarised(
                type = "binomial", groupLevel = group.k,
                strataName = strata.k, strataLevel = strataLevel.k
              ) |>
              mutate(exposed = NA) # for KM and followup
            k <- k + 1
          },
          error = function(e) {
          })
        }

        # follow-up stats ----
        results[[k]] <- data.k |>
          group_by(exposed) |>
          summarise(
            followup_mean = mean(time),
            followup_sd = sd(time),
            followup_median = median(time),
            followup_q25 = quantile(time, 0.25),
            followup_q75 = quantile(time, 0.75),
            followup_min = min(time),
            followup_max = max(time),
            age_mean = mean(age),
            age_sd = sd(age),
            age_median = median(age),
            age_q25 = quantile(age, 0.25),
            age_q75 = quantile(age, 0.75),
            age_min = min(age),
            age_max = max(age)
          ) |>
          mutate(
            group_name = "cohort_name",
            group_level = group.k,
            strata_name = strata.k,
            strata_level = strataLevel.k,
            result_type = "survival_stats",
            estimate_type = "numeric",
            num_control = sum(data.k$exposed == 0),
            num_exposed = sum(data.k$exposed == 1),
            num_events_control = sum(data.k$exposed == 0 & data.k$status == 1),
            num_events_exposed = sum(data.k$exposed == 1 & data.k$status == 1)
          ) |>
          pivot_longer(
            cols = c("followup_mean", "followup_median", "followup_q25", "followup_q75",
                     "followup_min", "followup_max","followup_sd",  "num_control",
                     "num_exposed", "num_events_control", "num_events_exposed",
                     "age_mean", "age_median", "age_q25", "age_q75",
                     "age_min", "age_max", "age_sd"),
            names_to = "estimate_name", values_to = "estimate_value"
          ) |>
          mutate(estimate_value = as.character(estimate_value))
        k <- k + 1
      }
    }
  }
  return(results |> bind_rows())
}

regressionToSummarised <- function(
    x, type, groupLevel, strataName, strataLevel,
    cols = c("coef", "se_coef", "exp_coef", "z", "p", "lower_ci", "upper_ci"),
    estimate = "numeric") {
  x |>
    mutate(
      estimate_type = estimate,
      group_name = "cohort_name",
      group_level = groupLevel,
      strata_name = strataName,
      strata_level = strataLevel,
      result_type = type
    ) |>
    select(-variable) |>
    pivot_longer(
      cols = all_of(cols),
      names_to = "estimate_name", values_to = "estimate_value"
    ) |>
    mutate(estimate_value = as.character(estimate_value))
}

asmdBinary <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% mutate(weight = 1)
  } else {
    x <- x %>% rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>% rename("group" = dplyr::all_of(groupName))
  for (variable in variables) {
    lab <- unique(x[[variable]])
    if (!all(lab %in% c(0, 1))) {
      x <- dplyr::mutate(x, !!variable := dplyr::if_else(.data[[variable]] == .env$lab[1], 0, 1))
    }
  }
  lab <- unique(x$group)
  x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 2)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(variables),
      list(
        mean = function(x) {Hmisc::wtd.mean(x, .data$weight)},
        var = function(x) {Hmisc::wtd.var(x, .data$weight)}
      ),
      .names = "{.col} {.fn}"
    )) %>%
    tidyr::pivot_longer(!"group", names_to = "variable.func") %>%
    tidyr::separate("variable.func", c("variable", "func"), " ") %>%
    tidyr::pivot_wider(names_from = c("func", "group"), values_from = "value") %>%
    dplyr::mutate(smd = (.data$mean_1-.data$mean_2)/sqrt((.data$var_1+.data$var_2)/2), asmd = abs(smd)) %>%
    dplyr::select("variable", "asmd", "smd") %>%
    mutate(asmd_type = "binary")

}

asmdContinuous <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% mutate(weight = 1)
  } else {
    x <- x %>% rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>% rename("group" = dplyr::all_of(groupName))
  lab <- unique(x$group)
  x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 2)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(variables),
      list(
        mean = function(x) {Hmisc::wtd.mean(x, .data$weight)},
        var = function(x) {Hmisc::wtd.var(x, .data$weight)}
      ),
      .names = "{.col} {.fn}"
    )) %>%
    tidyr::pivot_longer(!"group", names_to = "variable.func") %>%
    tidyr::separate("variable.func", c("variable", "func"), " ") %>%
    tidyr::pivot_wider(names_from = c("func", "group"), values_from = "value") %>%
    dplyr::mutate(smd = (.data$mean_1-.data$mean_2)/sqrt(.data$var_1+.data$var_2), asmd = abs(smd)) %>%
    dplyr::select("variable", "asmd", "smd") %>%
    mutate(asmd_type = "continuous")

}

asmdCategorical <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% dplyr::mutate(weight = 1)
  } else {
    x <- x %>% dplyr::rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>%
    dplyr::rename("group" = dplyr::all_of(groupName)) %>%
    dplyr::select("group", "weight", dplyr::all_of(variables))
  lab <- unique(x$group)
  if (length(lab) != 2) {
    stop("Number of labels in group column different from 2.")
  }
  x <- x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 0))
  denominator <- x %>%
    dplyr::group_by(.data$group) %>%
    dplyr::tally(wt = .data$weight, name = "denominator")
  result <- NULL
  for (k in 1:length(variables)) {
    y <- x %>%
      dplyr::rename("label" = dplyr::all_of(variables[k])) %>%
      dplyr::group_by(.data$group,.data$label) %>%
      dplyr::tally(wt = .data$weight) %>%
      dplyr::right_join(denominator, by = "group") %>%
      dplyr::mutate(percentage = dplyr::if_else(
        is.na(.data$n), 0, .data$n/.data$denominator
      )) %>%
      dplyr::select("label", "group", "percentage") %>%
      tidyr::pivot_wider(names_from = "group", values_from = "percentage", values_fill = 0)
    TT <- y[["1"]]
    CC <- y[["0"]]
    result <- result %>% dplyr::union_all(dplyr::tibble(
      variable = variables[k],
      asmd = asmdFromPercentage(TT, CC)
    ))
  }
  result <- result %>% mutate(asmd_type = "categorical")
  return(result)
}

asmdFromPercentage <- function(TT, CC) {
  if (length(TT) == 1) {
    return(NA)
  } else {
    TT <- TT[-1]
    CC <- CC[-1]
    n <- length(TT)
    vect <- TT - CC
    TT1 <- matrix(rep(TT, n), nrow = n, byrow = TRUE)
    TT2 <- matrix(rep(TT, n), nrow = n, byrow = FALSE)
    CC1 <- matrix(rep(CC, n), nrow = n, byrow = TRUE)
    CC2 <- matrix(rep(CC, n), nrow = n, byrow = FALSE)
    S <- (TT1*TT2 + CC1*CC2) / 2
    diag(S) <- (TT*(1-TT) + CC*(1-CC)) / 2
    asmd <- as.numeric(sqrt(vect %*% solve(S) %*% vect))
    return(asmd)
  }
}
