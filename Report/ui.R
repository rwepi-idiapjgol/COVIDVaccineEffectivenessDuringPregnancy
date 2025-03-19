ui <- dashboardPage(
  dashboardHeader(title = "Pregnant COVID-19 vaccine effectiveness"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Database details",
        tabName = "database_details",
        menuSubItem(
          text = "CDM snapshot",
          tabName = "cdm_snapshot"
        )
      ),
      menuItem(
        text = "Cohorts",
        tabName = "cohort_details",
        menuSubItem(
          text = "Cohort count",
          tabName = "cohort_count"
        )
      ),
      menuItem(
        text = "Matching summary",
        tabName = "matching",
        menuSubItem(
          text = "Weekly counts",
          tabName = "weekly_counts"
        ),
        menuSubItem(
          text = "Re-enrollment",
          tabName = "reenrollment"
        )
      ),
      menuItem(
        text = "Popultation",
        tabName = "population",
        menuSubItem(
          text = "Attrition",
          tabName = "attrition"
        ),
        menuSubItem(
          text = "Counts",
          tabName = "count"
        ),
        menuSubItem(
          text = "Index date",
          tabName = "index_date"
        ),
        menuSubItem(
          text = "Follow-up time",
          tabName = "followup"
        ),
        menuSubItem(
          text = "Vaccine uptake",
          tabName = "vaccination"
        ),
        menuSubItem(
          text = "Pregnant vaccination",
          tabName = "pregnant_vaccination"
        ),
        menuSubItem(
          text = "Baseline characteristics",
          tabName = "baseline_characteristics"
        ),
        menuSubItem(
          text = "Large Scale characteristics",
          tabName = "large_scale_characteristics"
        ),
        menuSubItem(
          text = "Standardised mean differences",
          tabName = "smd"
        )
      ),
      menuItem(
        text = "Negative Control Outcomes",
        tabName = "nco",
        menuSubItem(
          text = "Summary",
          tabName = "nco_summary"
        ),
        menuSubItem(
          text = "Forest plot",
          tabName = "nco_forest_plot"
        )
      ),
      menuItem(
        text = "Vaccine effectiveness",
        tabName = "outcomes",
        menuSubItem(
          text = "Summary",
          tabName = "study_summary"
        ),
        menuSubItem(
          text = "Kaplan-Meier",
          tabName = "kaplan_meier"
        ),
        menuSubItem(
          text = "Effectiveness estimates",
          tabName = "study_forest_plot"
        )
      )
    )
  ),
  ## body ----
  dashboardBody(
    tabItems(
      ### cdm_snapshot ----
      tabItem(
        tabName = "cdm_snapshot",
        h3("Database details"),
        p("See details of CDM snapshot for each database:"),
        DTOutput("cdm_snapshot_table")
      ),
      ### cohort_count ----
      tabItem(
        tabName = "cohort_count",
        h3("Cohort counts"),
        p("Counts for each JSON cohort instantiated in the study"),
        selectors(data = data$cohort_count, prefix = "cohort_count",
                  columns = c("cdm_name", "cohort_group"), multiple = TRUE,
                  default = list("cdm_name" = data$cohort_count$cdm_name[1],
                                 "cohort_group" = data$cohort_count$cohort_group[1])),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("cohort_count_cohort_name_picker")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            div(
              style = "margin-bottom: 20px;",
              downloadButton("cohort_count_download_table", "Download current table")
            ),
            DTOutput("cohort_count_table") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("weekly_counts_table_download_word", "Download table in word"),
            gt_output('cohort_count_table_formatted') %>% withSpinner()
          )
        )
      ),
      ### matching counts ----
      tabItem(
        tabName = "weekly_counts",
        h3("Sequential matching counts"),
        p("Counts of elegible and matched individuals for each calendar week during sequential matching."),
        selectors(
          data$weekly_counts, prefix = "weekly_cnts", columns = c("cdm_name", "comparison", "covid_definition"),
          default = list(
            "cdm_name" = data$weekly_counts$cdm_name[1],
            "comparison" = data$weekly_counts$comparison[1],
            "covid_definition" = data$weekly_counts$covid_definition[1]
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("weekly_counts_table_download", "Download table as csv"),
            DTOutput('weekly_counts_table') %>% withSpinner()
          ),
          tabPanel(
            "Summary table",
            h5(),
            downloadButton("weekly_counts_summary_download", "Download table in word"),
            gt_output('weekly_counts_summary') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            div(
              pickerInput(
                inputId = "weekly_plot",
                label = "Matching status",
                choices = c("elegible_exposed", "matched_exposed", "elegible_unexposed", "matched_unexposed"),
                selected = c("elegible_exposed", "matched_exposed"),
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = TRUE,
                inline = TRUE
              )
            ),
            plotSelectors(prefix = "plt_wcounts", choices = c("cdm_name", "comparison", "covid_definition", "matching_status"),
                          default = list("color" = "matching_status", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "wcounts"),
            downloadButton("weekly_counts_plot_download", "Download figure"),
            plotlyOutput('weekly_counts_plot') %>% withSpinner()
          )
        )
      ),
      ## index date ----
      tabItem(
        tabName = "index_date",
        h3("Index date"),
        p("Distribution of index date (patient follow-up start date)."),
        selectors(
          data$index_date, prefix = "index_dates",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$index_date$cdm_name[1],
            "comparison" = data$index_date$comparison[1],
            "covid_definition" = data$index_date$covid_definition[1],
            "strata_name" = data$index_date$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("index_date_strata_level")
        ),
        div(
          pickerInput(
            inputId = "index_date_group",
            label = "Group by",
            choices = c("days", "weeks", "months", "years"),
            selected = c("months"),
            options = list(`actions-box` = FALSE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE,
            inline = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw daya",
            h5(),
            downloadButton("index_date_table_download", "Download table as csv"),
            DTOutput('index_date_table') %>% withSpinner()
          ),
          tabPanel(
            "Summary table",
            h5(),
            downloadButton("index_date_summary_download", "Download table in word"),
            gt_output('index_date_summary') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(prefix = "plt_index", choices = c("cdm_name", "comparison", "covid_definition", "strata_name", "strata_level"),
                          default = list("color" = NULL, "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_index"),
            downloadButton("index_date_plot_download", "Download figure"),
            plotlyOutput('index_date_plot') %>% withSpinner()
          )
        )
      ),
      ## future observation ----
      ## re-enrollment ----
      tabItem(
        tabName = "reenrollment",
        h3("Re-enrollments"),
        p("Number of subjects firstly enrolled as controls which later became
          vaccinated an are re-enrolled in the exposure cohort."),
        selectors(
          data$index_date, prefix = "reenrolment",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$index_date$cdm_name[1],
            "comparison" = data$index_date$comparison[1],
            "covid_definition" = "diagnostic_test",
            "strata_name" = data$index_date$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("reenrolment_strata_level")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("reenrolment_table_download", "Download table as csv"),
            h5(),
            DTOutput('reenrolment_table') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("reenrolment_summary_download", "Download table in word"),
            gt_output('reenrolment_summary') %>% withSpinner()
          )
        )
      ),
      ## vaccination ----
      tabItem(
        tabName = "vaccination",
        h3("Vaccine uptake"),
        p("Uptake of 1st, 2nd, 3rd and 4th COVID-19 vaccine in the matched study population. Uptake refers to COVID-19 vaccination anytime during observation time in the database."),
        selectors(
          data$vaccine_distribution, prefix = "vaccination",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$vaccine_distribution$cdm_name[1],
            "comparison" = data$vaccine_distribution$comparison[1],
            "covid_definition" = data$vaccine_distribution$covid_definition[1],
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("vaccination_strata_level")
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "vaccination_exposed",
            label = "Exposure",
            choices = c("exposed", "comparator"),
            selected = c("exposed"),
            options = list(`actions-box` = FALSE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("vaccination_vaccine_dose")
        ),
        div(
          pickerInput(
            inputId = "vaccination_group",
            label = "Group by",
            choices = c("days", "weeks", "months", "years"),
            selected = c("weeks"),
            options = list(`actions-box` = FALSE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE,
            inline = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("vaccination_table_download", "Download table as csv"),
            DTOutput('vaccination_table') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("vaccination_summary_download", "Download table in word"),
            gt_output('vaccination_summary') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(prefix = "plt_vax", choices = c("cdm_name", "comparison", "covid_definition", "strata_name", "strata_level", "exposed", "vaccine_dose"),
                          default = list("color" = "vaccine_dose", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_vax"),
            downloadButton("vaccination_plot_download", "Download figure"),
            plotlyOutput('vaccination_plot') %>% withSpinner()
          )
        )
      ),
      ## pregnant vaccination ----
      tabItem(
        tabName = "pregnant_vaccination",
        h3("Vaccination during pregnancy"),
        p("Uptake of 1st, 2nd, 3rd and 4th COVID-19 vaccine while pregnant in the macthed study population."),
        selectors(
          data$vaccine_distribution, prefix = "pregnant_vax",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$vaccine_distribution$cdm_name[1],
            "comparison" = data$vaccine_distribution$comparison[1],
            "covid_definition" = data$vaccine_distribution$covid_definition[1],
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("pregnant_vax_strata_level")
        ),
        div(
          pickerInput(
            inputId = "pregnant_vax_exposed",
            label = "Exposure",
            choices = c("exposed", "comparator"),
            selected = c("exposed"),
            options = list(`actions-box` = FALSE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("pregnant_vax_vaccine_dose")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw",
            h5(),
            downloadButton("pregnant_vax_table_download", "Download table as csv"),
            DTOutput('pregnant_vax_table') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("pregnant_vax_summary_download", "Download table in word"),
            gt_output('pregnant_vax_summary') %>% withSpinner()
          )
        )
      ),
      ## population attrition ----
      tabItem(
        tabName = "attrition",
        h3("Population attrition"),
        p("Study population attrition. Reason refers to inclusion criteria."),
        selectors(
          data$population_attrition, prefix = "attrition",
          columns = c("cdm_name", "comparison", "covid_definition"),
          default = list(
            "cdm_name" = data$population_attrition$cdm_name[1],
            "comparison" = data$population_attrition$comparison[1],
            "covid_definition" = data$population_attrition$covid_definition[1]
          ),
          multiple = FALSE
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("attrition_table_download", "Download table as csv")
        ),
        DTOutput('attrition_table') %>% withSpinner()
      ),
      tabItem(
        tabName = "count",
        h3("Population counts"),
        p("Number of patients for each database, analysis, and cohort."),
        selectors(
          data$population_count, prefix = "pop_count",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$population_count$cdm_name[1],
            "comparison" = data$population_count$comparison[1],
            "covid_definition" = data$population_count$covid_definition[1],
            "strata_name" = data$population_count$strata_name[1]
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("population_count_strata_level")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            div(
              style = "display: inline-block;vertical-align:center; width: 150px;",
              downloadButton("population_count_table_download", "Download table as csv")
            ),
            DTOutput('population_count_table') %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("population_count_summary_download", "Download table in word"),
            gt_output('population_count_summary') %>% withSpinner()
          )
        )
      ),
      ## baseline ----
      tabItem(
        tabName = "baseline_characteristics",
        h3("Baseline characteristics"),
        p("Characterisation of the macthed study population at baseline"),
        selectors(
          data$baseline, prefix = "baseline",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$baseline$cdm_name[1],
            "comparison" = data$baseline$comparison[1],
            "covid_definition" = data$baseline$covid_definition[1],
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("baseline_strata_level")
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("baseline_table_download", "Download table as word")
        ),
        gt_output('baseline_table') %>% withSpinner()
      ),
      ## large scale characteristics ----
      tabItem(
        tabName = "large_scale_characteristics",
        h3("Large scale characteristics"),
        p("Large scale characeristics for each cohort and strata in the study, in diferent time windows relative to index date"),
        selectors(
          data = data$large_scale,
          prefix = "large",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          multiple = FALSE
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("large_scale_strata_level")
        ),
        selectors(
          data = data$large_scale,
          prefix = "large",
          columns = c("exposed", "window", "estimate_name"),
          default = list(
            "window" = unique(data$large_scale$window)[1],
            "exposed" = "overall",
            "estimate_name" = c("count", "percentage")
          )
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("large_scale_download_table", "Download current table as csv")
        ),
        DTOutput("large_scale_table") %>% withSpinner()
      ),
      # SMD ----
      tabItem(
        tabName = "smd",
        h3("Standardised mean differences"),
        p("Standardised mean differences between macthed exposed and unexposed cohorts in diferent time windows relative to index date"),
        selectors(
          data = data$smd,
          prefix = "smd",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          multiple = FALSE
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("smd_strata_level")
        ),
        selectors(
          data = data$smd,
          prefix = "smd",
          columns = c("window"),
          default = list("window" = unique(data$large_scale$window)[1])
        ),
        div(
          style = "display: inline-block;vertical-align:center; width: 150px;",
          downloadButton("smd_download_table", "Download current table as csv")
        ),
        DTOutput("smd_table") %>% withSpinner()
      ),
      ## NCO summary ----
      tabItem(
        tabName = "nco_summary",
        h3("Summary"),
        p("For each analysis and negative control outcome we present subject and outcome counts, and descriptive statistics of participants age and follow-up time."),
        selectors(
          data = data$survival_summary |> filter(variable_name == "nco"),
          prefix = "nco_summ",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list("cdm_name" = data$survival_summary$cdm_name[1],
                         "comparison" = "none_first",
                         "covid_definition" = "diagnostic_test",
                         "strata_name" = "overall")
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("nco_summary_strata_level")
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "nco"),
          prefix = "nco_summ",
          columns = c("outcome"),
          default = list(
            "outcome" = data$survival_summary |> filter(variable_name == "nco") |> pull(outcome) |> unique()
          )
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "nco"),
          prefix = "nco_summ",
          columns = c("followup_end"),
          default = list(
            "followup_end" = "cohort_end_date_pregnancy"
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("nco_summary_download_raw", "Download current table as csv"),
            DTOutput("nco_summary_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("nco_summary_download_table", "Download table in word"),
            gt_output('nco_summary_table') %>% withSpinner()
          )
        )
      ),
      ## STUDY summary ----
      tabItem(
        tabName = "study_summary",
        h3("Summary"),
        p("For each analysis and study outcome we present subject and outcome counts, and descriptive statistics of participants age and follow-up time."),
        selectors(
          data = data$survival_summary |> filter(variable_name == "study"),
          prefix = "study_summ",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$survival_summary$cdm_name[1],
            "comparison" = "none_first",
            "covid_definition" = "diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("study_summary_strata_level")
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "study"),
          prefix = "study_summ",
          columns = c("outcome"),
          default = list(
            "outcome" = data$survival_summary |> filter(variable_name == "study") |> pull(outcome) |> unique()
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "delivery_sum",
            label = "delivery excluded",
            choices = c("yes", "no"),
            selected = "yes",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        selectors(
          data = data$survival_summary |> filter(variable_name == "study"),
          prefix = "study_summ",
          columns = c("followup_end"),
          default = list( "followup_end" = "cohort_end_date_pregnancy")
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "study_summ_window",
            label = "Window",
            choices = c("0_14", "15_Inf", "15_28", "15_90", "15_180", "15_365", "29_90", "29_180", "91_180", "181_365", "366_Inf"),
            selected = "15_Inf",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("study_summary_download_raw", "Download current as csv"),
            DTOutput("study_summary_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("study_summary_download_table", "Download table in word"),
            gt_output('study_summary_table') %>% withSpinner()
          )
        )
      ),
      ## NCO forest ----
      tabItem(
        tabName = "nco_forest_plot",
        h3("Forest plots"),
        p("Negative control outcomes risk estimates for all populations."),
        selectors(
          data = data$risk |> filter(variable_name == "nco"),
          prefix = "nco_risk",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$survival_summary$cdm_name[1],
            "comparison" = "none_first",
            "covid_definition" = "diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("nco_risk_strata_level")
        ),
        selectors(
          data = data$risk |> filter(variable_name == "nco"),
          prefix = "nco_risk",
          columns = c("regression", "outcome", "followup_end"),
          default = list(
            "regression" = "cox",
            "outcome" = data$risk |> filter(variable_name == "nco") |> pull(outcome) |> unique(),
            "followup_end" = "cohort_end_date_pregnancy"
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            downloadButton("nco_risk_download_raw", "Download current as csv"),
            DTOutput("nco_risk_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            downloadButton("nco_risk_download_table", "Download table in word"),
            gt_output('nco_risk_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(
              prefix = "plt_nco_risk",
              choices = c("cdm_name", "comparison", "covid_definition", "strata_name", "strata_level",
                          "regression", "followup_end", "window", "outcome",
                          "association"),
              default = list("color" = "association", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_nco_risk"),
            downloadButton("nco_risk_download_plot", "Download plot"),
            plotlyOutput('nco_risk_plot', height = "1400px") %>% withSpinner()
          )
        )
      ),
      # STUDY FOREST ----
      tabItem(
        tabName = "study_forest_plot",
        h3("Vaccine effectiveness"),
        p("Hazard ratios for each analysis and study outcome in the study."),
        selectors(
          data = data$risk |> filter(variable_name == "study"),
          prefix = "study_risk",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$survival_summary$cdm_name[1],
            "comparison" = "none_first",
            "covid_definition" = "diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("study_risk_strata_level")
        ),
        selectors(
          data = data$risk |> filter(variable_name == "study"),
          prefix = "study_risk",
          columns = c("regression", "outcome"),
          default = list(
            "regression" = "cox",
            "outcome" = data$risk |> filter(variable_name == "study") |> pull(outcome) |> unique()
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "delivery_risk",
            label = "delivery excluded",
            choices = c("yes", "no"),
            selected = "yes",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        selectors(
          data = data$risk |> filter(variable_name == "study"),
          prefix = "study_risk",
          columns = c("followup_end"),
          default = list("followup_end" = "cohort_end_date_pregnancy")
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "study_risk_window",
            label = "Window",
            choices = c("0_14", "15_Inf", "15_28", "15_90", "15_180", "15_365", "29_90", "29_180", "91_180", "181_365", "366_Inf"),
            selected = "15_Inf",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            h5(),
            pickerInput(
              inputId = "study_risk_raw_hr",
              label = "Estimate",
              choices = c("Hazard Ratio", "Vaccine effectiveness"),
              selected = "Hazard Ratio",
              options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
              multiple = FALSE,
              inline = TRUE
            ),
            downloadButton("study_risk_download_raw", "Download current as csv"),
            h5(),
            DTOutput("study_risk_raw") %>% withSpinner()
          ),
          tabPanel(
            "Table",
            h5(),
            pickerInput(
              inputId = "study_risk_format_hr",
              label = "Estimate",
              choices = c("Hazard Ratio", "Vaccine effectiveness"),
              selected = "Hazard Ratio",
              options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
              multiple = FALSE,
              inline = TRUE
            ),
            downloadButton("study_risk_download_table", "Download table in word"),
            gt_output('study_risk_table') %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            h5(),
            plotSelectors(
              prefix = "plt_study_risk",
              choices = c("cdm_name", "comparison", "covid_definition", "strata_name", "strata_level",
                          "regression", "followup_end", "window", "outcome", "delivery_excluded",
                          "association"),
              default = list("color" = "outcome", "facet_by" = "cdm_name")),
            plotDownloadSelectors(prefix = "dwn_study_risk"),
            downloadButton("study_risk_download_plot", "Download plot"),
            h5(),
            plotlyOutput('study_risk_plot', height = "800px") %>% withSpinner()
          )
        )
      ),
      # KAPLAN MEIER ----
      tabItem(
        tabName = "kaplan_meier",
        h3("Kaplan-Meier"),
        p("Kaplan-Meier curves for each population cohort and outcome in the study."),
        selectors(
          data = data$kaplan_meier,
          prefix = "km",
          columns = c("cdm_name", "comparison", "covid_definition", "strata_name"),
          default = list(
            "cdm_name" = data$survival_summary$cdm_name[1],
            "comparison" = "none_first",
            "covid_definition" = "diagnostic_test",
            "strata_name" = "overall"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          uiOutput("km_strata_level")
        ),
        selectors(
          data = data$kaplan_meier,
          prefix = "km",
          columns = "outcome",
          default = list( "outcome" = "covid")
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "delivery_km",
            label = "Delivery excluded",
            choices = c("yes", "no"),
            selected = "yes",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE,
            inline = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "km_followup_end",
            label = "Followup end",
            choices = c("cohort_end_date", "cohort_end_date_pregnancy"),
            selected = "cohort_end_date_pregnancy",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE,
            inline = TRUE
          )
        ),
        h5(),
        plotSelectors(
          selectors = "facet_by",
          prefix = "plt_km",
          choices = c("cdm_name", "comparison", "covid_definition", "strata_name",
                      "strata_level", "outcome", "followup_end"),
          default = list("facet_by" = "cdm_name")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Kaplan-Meier",
            h5(),
            plotDownloadSelectors(prefix = "dwn_km"),
            downloadButton("km_download_plot", "Download plot"),
            h5(),
            plotlyOutput('km_plot') %>% withSpinner()
          ),
          tabPanel(
            "Log-Log plot",
            h5(),
            plotDownloadSelectors(prefix = "dwn_loglog"),
            downloadButton("loglog_download_plot", "Download plot"),
            h5(),
            plotlyOutput('loglog_plot') %>% withSpinner()
          )
        )
      ),
      # Followup ----
      tabItem(
        tabName = "followup",
        h3("Available follow-up time"),
        p("Distribution of available patient follow-up time for each analysis population."),
        selectors(
          data = data$censoring,
          prefix = "followup",
          columns = c("CDM name", "Comparison", "Covid definition", "Follow-up end", "Reason"),
          default = list(
            "cdm_name" = data$censoring$`CDM name`[1],
            "Comparison" = data$censoring$Comparison[1],
            "Covid definition" = data$censoring$`Covid definition`[1],
            "Follow-up end" = data$censoring$`Follow-up end`[1],
            "Reason" = data$censoring$Reason
          )
        ),
        downloadButton("followup_summary_download_table", "Download table in word"),
        gt_output('followup_summary_table') %>% withSpinner()
      )
    )
  )
)
