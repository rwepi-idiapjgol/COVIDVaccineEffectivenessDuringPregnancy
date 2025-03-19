# server shiny ----
server <- function(input, output, session) {
  # cdm_snapshot ----
  output$cdm_snapshot_table <- renderDataTable({
    datatable(
      data$snapshot,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # cohort_count picker ----
  output$cohort_count_cohort_name_picker <-  reactiveSelectors(
    data = data$cohort_count, prefix = "cohort_count", columns = "cohort_name",
    restrictions = "cohort_group", input = input, multiple = TRUE
  )
  # cohort_count -----
  getCohortCount <- reactive({
    return(
      data$cohort_count |>
        filterData(prefix = "cohort_count", input = input) %>%
        niceChar()
    )
  })
  output$cohort_count_table <- renderDataTable({
    datatable(
      getCohortCount(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25)
    )
  })
  output$cohort_count_download_table <- downloadHandler(
    filename = function() {
      "cohortCountTable.csv"
    },
    content = function(file) {
      write_csv(getCohortCount(), file)
    }
  )
  output$cohort_count_table_formatted <- render_gt({
    getCohortCount() %>%
      mutate("Number records" = niceNum(`Number records`), "Number subjects" = niceNum(`Number subjects`)) |>
      gtTable(groupNameCol = c("CDM name"), groupNameAsColumn = TRUE, colsToMergeRows = "all_columns")
  })
  output$weekly_counts_table_download_word <- downloadHandler(
    filename = function() {
      "cohortCountTable.docx"
    },
    content = function(file) {
      gtsave(data =  getCohortCount() %>%
               gtTable(groupNameCol = "cdm_name", groupNameAsColumn = TRUE),
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
  # Weekly counts ----
  getWeeklyCounts <- reactive({
    data$weekly_counts %>%
      filterData(prefix = "weekly_cnts", input = input) %>%
      arrange(.data$week_start)
  })
  output$weekly_counts_summary <- render_gt({
    getWeeklyCounts() %>%
      group_by(cdm_name, comparison, covid_definition) %>%
      summarise(
        "Exposed elegible" = sum(elegible_exposed, na.rm = TRUE),
        "Matched, N(%)" = sum(matched_exposed, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        "Not matched, N(%)" = paste0(
          niceNum(`Exposed elegible` - `Matched, N(%)`), " (",
          round((`Exposed elegible` - `Matched, N(%)`)/`Exposed elegible` * 100, 2), " %)"),
        "Matched, N(%)" = paste0(
          niceNum(`Matched, N(%)`), " (", round((`Matched, N(%)`)/`Exposed elegible` * 100, 2), " %)"),
        "Exposed elegible" = niceNum(`Exposed elegible`)
      ) |>
      rename("Comparison" = "comparison", "Covid definition" = "covid_definition") |>
      gtTable(groupNameCol = "cdm_name", groupNameAsColumn = TRUE)
  })
  output$weekly_counts_summary_download <- downloadHandler(
    filename = function() {
      "weeklyCountsSummary.docx"
    },
    content = function(file) {
      gtsave(data = getWeeklyCounts() %>%
               group_by(cdm_name, comparison, covid_definition) %>%
               summarise(
                 "Exposed elegible" = sum(elegible_exposed, na.rm = TRUE),
                 "Matched, N(%)" = sum(matched_exposed, na.rm = TRUE),
                 .groups = "drop"
               ) %>%
               mutate(
                 "Matched, N(%)" = paste0(
                   `Matched`, " (", round((`Matched`)/`Exposed elegible`, 3), " %)"),
                 "Not matched, N(%)" = paste0(
                   `Exposed elegible` - `Matched`, " (",
                   round((`Exposed elegible` - `Matched`)/`Exposed elegible`, 3), " %)")
               ) %>%
               rename("Comparison" = "comparison", "Covid definition" = "covid_definition") %>%
               gtTable(groupNameCol = "cdm_name"),
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
  output$weekly_counts_table <- renderDataTable({
    datatable(
      getWeeklyCounts() %>%
        mutate(
          across(contains("exposed"), ~ if_else(.x < 5 & .x > 0, NA, .x))
        ) %>%
        select("CDM name" = "cdm_name",
               "Comparison" = "comparison",
               "Covid definition" = "covid_definition",
               "Week start" = "week_start",
               "Exposed PRE" = "elegible_exposed",
               "Unexposed PRE" = "elegible_unexposed",
               "Exposed POST" = "matched_exposed",
               "Unexposed POST" = "matched_unexposed"),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25)
    )
  })
  output$weekly_counts_table_download <- downloadHandler(
    filename = function() {
      "weeklyCounts.csv"
    },
    content = function(file) {
      write_csv(getWeeklyCounts() %>%
                  mutate(
                    across(contains("exposed"), ~ if_else(.x < 5 & .x > 0, NA, .x))
                  ) %>%
                  select("CDM name" = "cdm_name",
                         "Comparison" = "comparison",
                         "Covid definition" = "covid_definition",
                         "Week start" = "week_start",
                         "Exposed PRE" = "elegible_exposed",
                         "Unexposed PRE" = "elegible_unexposed",
                         "Exposed POST" = "matched_exposed",
                         "Unexposed POST" = "matched_unexposed"),
                file)
    }
  )
  getWeeklyCountsPlot <- reactive({
    table <- getWeeklyCounts() %>%
      pivot_longer(cols = c("elegible_exposed", "matched_exposed", "elegible_unexposed", "matched_unexposed"), names_to = "matching_status", values_to = "n") |>
      filter(matching_status %in% input$weekly_plot) %>%
      mutate(n = if_else(n<5 & n>0, NA, n))

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    if(is.null(input$plt_wcounts_color)){
      if(!is.null(input$plt_wcounts_facet_by)){
        p<-table %>%
          filter(!is.na(n)) %>%
          unite("facet_var",
                c(all_of(input$plt_wcounts_facet_by)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "week_start", y = "n")) +
          geom_point() +
          geom_line() +
          scale_x_date(date_breaks = "months", date_labels = "%b%y") +
          facet_wrap(vars(facet_var),nrow = 2) +
          theme_bw()
      } else{
        p <- table %>%
          filter(!is.na(n)) %>%
          ggplot(aes_string(x = "week_start", y = "n")) +
          geom_point() +
          geom_line() +
          scale_x_date(date_breaks = "months", date_labels = "%b%y") +
          theme_bw()
      }
    }
    if(!is.null(input$plt_wcounts_color) ){
      if(is.null(input$plt_wcounts_facet_by) ){
        p <- table %>%
          filter(!is.na(n)) %>%
          unite("Group",
                c(all_of(input$plt_wcounts_color)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x = "week_start", y = "n",
                            group = "Group",
                            fill = "Group",
                            colour = "Group")) +
          geom_point() +
          geom_line() +
          scale_x_date(date_breaks = "months", date_labels = "%b%y") +
          theme_bw()
      }

      if(!is.null(input$plt_wcounts_facet_by) ){
        if(!is.null(input$plt_wcounts_color) ){
          p<-table %>%
            filter(!is.na(n)) %>%
            unite("Group",
                  c(all_of(input$plt_wcounts_color)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$plt_wcounts_facet_by)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = "week_start", y = "n",
                              group = "Group",
                              fill = "Group",
                              colour = "Group")) +
            geom_point() +
            geom_line() +
            scale_x_date(date_breaks = "months", date_labels = "%b%y") +
            facet_wrap(vars(facet_var),ncol = 2)+
            theme_bw()
        }
      }
    }

    p +
      xlab("Week start") +
      ylab("N")
  })
  output$weekly_counts_plot <- renderPlotly({
    getWeeklyCountsPlot()
  })
  output$weekly_counts_plot_download <- downloadHandler(
    filename = function() {
      paste0("weeklyCountsPlot.", input$wcounts_device)
    },
    content = function(file) {
      ggsave(file, getWeeklyCountsPlot(),
             width = as.numeric(input$wcounts_width),
             height = as.numeric(input$wcounts_height))
    }
  )
  # index date ----
  output$index_date_strata_level <-  reactiveSelectors(
    data = data$index_date, prefix = "index_dates", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
    # default = list("strata_level" = data$index_date$strata_level[data$index_date$strata_name %in% input$index_date_strata_name])
  )
  getIndexDate <- reactive({
    data$index_date %>%
      filterData(prefix = "index_dates", input = input) %>%
      arrange(.data$index_date) %>%
      {if (input$index_date_group == "years") {
        mutate(., index_date = lubridate::floor_date(index_date, unit = "years"))
      } else if (input$index_date_group == "weeks") {
        mutate(., index_date = lubridate::floor_date(index_date, unit = "weeks"))
      } else if (input$index_date_group == "months") {
        mutate(., index_date = lubridate::floor_date(index_date, unit = "months"))
      } else .} %>%
      group_by(cdm_name, comparison, covid_definition, strata_name, strata_level, index_date) %>%
      summarise(counts = sum(counts), .groups = "drop")
  })
  output$index_date_summary <- render_gt({
    getIndexDate() %>%
      mutate(
        counts = if_else(counts>0 & counts<5, "<5", niceNum(counts)),
        index_date = as.character(index_date)
      ) |>
      niceChar() |>
      rename("estimate_value" = "Counts") |>
      formatHeader(header = c("Strata name", "Strata level"), includeHeaderName = FALSE) |>
      gtTable(groupNameCol = "CDM name", groupNameAsColumn = TRUE)
  })
  output$index_date_summary_download <- downloadHandler(
    filename = function() {
      "indexDateSummary.docx"
    },
    content = function(file) {
      gtsave(data = getIndexDate() %>%
               mutate(
                 counts = if_else(counts>0 & counts<5, "<5", niceNum(counts)),
                 index_date = as.character(index_date)
               ) |>
               niceChar() |>
               rename("estimate_value" = "Counts") |>
               formatHeader(header = c("Strata name", "Strata level"), includeHeaderName = FALSE) |>
               gtTable(groupNameCol = "CDM name", groupNameAsColumn = TRUE),
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
  output$index_date_table <- renderDataTable({
    datatable(
      getIndexDate() %>%
        mutate(counts = if_else(counts < 5 & counts > 0, NA, counts)) %>%
        niceChar(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25)
    )
  })
  output$index_date_table_download <- downloadHandler(
    filename = function() {
      "indexDateCounts.csv"
    },
    content = function(file) {
      write_csv(getIndexDate() %>%
                  mutate(counts = if_else(counts < 5 & counts > 0, NA, counts)) %>%
                  niceChar(),
                file)
    }
  )
  getIndexDatePlot <- reactive({
    table <- getIndexDate() |>
      mutate(counts = if_else(counts < 5 & counts > 0, NA, counts)) |>
      filter(!is.na(counts))

    validate(need(ncol(table)>1, "Provide a valid table"))

    if (!is.null(input$plt_index_facet_by)) {
      table <- table %>%
        unite("facet_var",
              c(all_of(input$plt_index_facet_by)), remove = FALSE, sep = "; ")
    }
    if (!is.null(input$plt_index_color)) {
      table <- table %>%
        unite("Group",
              c(all_of(input$plt_index_color)), remove = FALSE, sep = "; ")
      p <- table %>%
        ggplot(aes_string(
          x = "index_date",
          y = "counts",
          color = "Group",
          fill = "Group"
        ))
    } else {
      p <- table %>%
        ggplot(aes_string(
          x = "index_date",
          y = "counts"
        ))
    }
    p <- p + geom_line() + geom_point()
    if (!is.null(input$plt_index_facet_by)) {
      p <- p +
        facet_wrap(vars(facet_var), ncol = 2)
    }
    p + ylab("Counts") + xlab("Index date")
  })
  output$index_date_plot <- renderPlotly({
    getIndexDatePlot()
  })
  output$index_date_plot_download <- serverPlotDownload(
    prefix = "dwn_index", name = "indexDatePlot", plot = getIndexDatePlot(), input = input
  )
  # re-enrollment ----
  output$reenrolment_strata_level <-  reactiveSelectors(
    data = data$reenrollment, prefix = "reenrolment", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
    # default = list("strata_level" = data$index_date$strata_level[data$index_date$strata_name %in% input$index_date_strata_name])
  )
  getReenrollment <- reactive({
    data$reenrollment %>%
      filterData(prefix = "reenrolment", input = input) %>%
      splitStrata()
  })
  output$reenrolment_table <- renderDataTable({
    datatable(
      getReenrollment(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25)
    )
  })
  output$reenrolment_table_download <- serverCSVDownload("reenrollments", getReenrollment())
  getReenrollmentGT <- reactive({
    data$reenrollment %>%
      filterData(prefix = "reenrolment", input = input) %>%
      mutate(
        estimate_name = "N (%)",
        estimate_value = paste0(count, " (", round(percentage, 2), " %)")
      ) %>%
      select(-c("count", "percentage")) %>%
      formatHeader(
        header = c("strata_name", "strata_level"),
        includeHeaderName = FALSE
      ) |>
      gtTable(
        groupNameCol = "cdm_name",
        groupNameAsColumn = TRUE,
        colsToMergeRows = "all_columns"
      )
  })
  output$reenrolment_summary <- render_gt({
    getReenrollmentGT()
  })
  output$reenrolment_summary_download <- serverGTDownload(
    name = "reenrollemnts", gt = getReenrollmentGT()
  )
  # vaccination ----
  output$vaccination_strata_level <-  reactiveSelectors(
    data = data$vaccine_distribution, prefix = "vaccination", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  output$vaccination_vaccine_dose <- reactiveSelectors(
    data = data$vaccine_distribution, prefix = "vaccination", columns = "vaccine_dose",
    restrictions = "comparison", input = input, multiple = TRUE
  )
  getVaccination <- reactive({
    data$vaccine_distribution %>%
      filterData(prefix = "vaccination", input = input) %>%
      arrange(date, comparison, covid_definition, vaccine_dose) %>%
      {if (input$vaccination_group == "years") {
        mutate(., date = lubridate::floor_date(date, unit = "years"))
      } else if (input$vaccination_group == "weeks") {
        mutate(., date = lubridate::floor_date(date, unit = "weeks"))
      } else if (input$vaccination_group == "months") {
        mutate(., date = lubridate::floor_date(date, unit = "months"))
      } else .} %>%
      group_by(cdm_name, comparison, covid_definition, strata_name, strata_level, exposed, vaccine_dose, date) %>%
      summarise(estimate_value = sum(estimate_value), .groups = "drop") %>%
      ungroup() %>%
      arrange(comparison, covid_definition, vaccine_dose, date)
  })
  output$vaccination_summary <- render_gt({
    getVaccination() %>%
      mutate(
        estimate_value = if_else(estimate_value>0 & estimate_value<5, "<5", niceNum(estimate_value)),
        date = as.character(date)
      ) |>
      niceChar() |>
      rename("estimate_value" = "Estimate value") |>
      formatHeader(header = c("Strata name", "Strata level", "Exposed"), includeHeaderName = FALSE) |>
      gtTable(groupNameCol = "CDM name", groupNameAsColumn = TRUE, colsToMergeRows = "all_columns")
  })
  output$vaccination_summary_download <- downloadHandler(
    filename = function() {
      "vaccinationSummary.docx"
    },
    content = function(file) {
      gtsave(data = getVaccination() %>%
               mutate(
                 estimate_value = if_else(estimate_value>0 & estimate_value<5, "<5", niceNum(estimate_value)),
                 date = as.character(date)
               ) |>
               niceChar() |>
               rename("estimate_value" = "Estimate value") |>
               formatHeader(header = c("Strata name", "Strata level", "Exposed"), includeHeaderName = FALSE) |>
               gtTable(groupNameCol = "CDM name", groupNameAsColumn = TRUE),
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
  output$vaccination_table <- renderDataTable({
    datatable(
      getVaccination() %>%
        mutate(estimate_value = if_else(estimate_value < 5 & estimate_value > 0, NA, estimate_value)) %>%
        niceChar() |>
        rename("Counts" = "Estimate value"),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25)
    )
  })
  output$vaccination_table_download <- downloadHandler(
    filename = function() {
      "vaccinationCounts.csv"
    },
    content = function(file) {
      write_csv(getVaccination() %>%
                  mutate(estimate_value = if_else(estimate_value < 5 & estimate_value > 0, NA, estimate_value)) %>%
                  niceChar() |>
                  rename("Counts" = "Estimate value"),
                file)
    }
  )
  getVaccinationPlot <- reactive({
    table <- getVaccination() |>
      mutate(counts = if_else(estimate_value < 5 & estimate_value > 0, NA, estimate_value)) |>
      filter(!is.na(counts) & !is.na(date))

    validate(need(ncol(table)>1, "Provide a valid selection of parameters"))

    if (!is.null(input$plt_vax_facet_by)) {
      table <- table %>%
        unite("facet_var",
              c(all_of(input$plt_vax_facet_by)), remove = FALSE, sep = "; ")
    }
    if (!is.null(input$plt_vax_color)) {
      table <- table %>%
        unite("Group",
              c(all_of(input$plt_vax_color)), remove = FALSE, sep = "; ")
      p <- table %>%
        ggplot(aes_string(
          x = "date",
          y = "counts",
          color = "Group",
          fill = "Group"
        ))
    } else {
      p <- table %>%
        ggplot(aes_string(
          x = "date",
          y = "counts"
        ))
    }
    max <- max(table$counts)
    p <- p + geom_line() + geom_point() + scale_y_continuous(limits = c(0, max))
    if (!is.null(input$plt_vax_facet_by)) {
      p <- p +
        facet_wrap(vars(facet_var), ncol = 2)
    }
    p + ylab("Counts") + xlab("Date")
  })
  output$vaccination_plot <- renderPlotly({
    getVaccinationPlot()
  })
  output$vaccination_plot_download <- serverPlotDownload(
    prefix = "dwn_vax", name = "vaccinationPlot", plot = getVaccinationPlot(), input = input
  )
  # pregnant vaccination ----
  output$pregnant_vax_strata_level <-  reactiveSelectors(
    data = data$vaccine_distribution, prefix = "pregnant_vax", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  output$pregnant_vax_vaccine_dose <- reactiveSelectors(
    data = data$vaccine_distribution, prefix = "pregnant_vax", columns = "vaccine_dose",
    restrictions = "comparison", input = input, multiple = TRUE
  )
  getVaccinationPregnant <- reactive({
    data$pregnant_vaccination %>%
      filterData(prefix = "pregnant_vax", input = input) %>%
      mutate(
        estimate_value = if_else(estimate_value>0 & estimate_value<5, "<5", niceNum(estimate_value)),
      ) |>
      arrange(comparison, covid_definition, vaccine_dose) |>
      niceChar() |>
      rename("estimate_value" = "Estimate value") |>
      formatHeader(header = c("Strata name", "Strata level", "Exposed"), includeHeaderName = FALSE) |>
      gtTable(groupNameCol = "CDM name", groupNameAsColumn = TRUE, colsToMergeRows = "all_columns")
  })
  output$pregnant_vax_summary <- render_gt({
    getVaccinationPregnant()
  })
  output$pregnant_vax_summary_download <- downloadHandler(
    filename = function() {
      "pregnantVaccinationSummary.docx"
    },
    content = function(file) {
      gtsave(data = getVaccinationPregnant(),
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
  output$pregnant_vax_table <- renderDataTable({
    datatable(
      data$pregnant_vaccination %>%
        filterData(prefix = "pregnant_vax", input = input) %>%
        mutate(estimate_value = if_else(estimate_value < 5 & estimate_value > 0, NA, estimate_value)) %>%
        niceChar() |>
        rename("Counts" = "Estimate value"),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25)
    )
  })
  output$pregnant_vax_table_download <- downloadHandler(
    filename = function() {
      "pregnantVaccinationCounts.csv"
    },
    content = function(file) {
      write_csv( data$pregnant_vaccination %>%
                   filterData(prefix = "pregnant_vax", input = input) %>%
                   mutate(estimate_value = if_else(estimate_value < 5 & estimate_value > 0, NA, estimate_value)) %>%
                   niceChar() |>
                   rename("Counts" = "Estimate value"),
                 file)
    }
  )
  # attrition ----
  getAttrition <- reactive({
    data$population_attrition |>
      filterData(prefix = "attrition", input = input) |>
      select(!"cdm_name", !"comparison", !"covid_definition") |>
      arrange(reason_id)
  })
  output$attrition_table <- renderDataTable({
    datatable(
      getAttrition(),
      rownames = FALSE,
      extensions = "Scroller",
      options = list(pageLength = 25)
    )
  })
  output$attrition_table_download <-  serverCSVDownload(
    name = "populationAttrition", getAttrition()
  )
  # count ----
  getCount <- reactive({
    data$population_count |>
      filterData(prefix = "pop_count", input = input)
  })
  output$population_count_strata_level <- reactiveSelectors(
    data = data$population_count, prefix = "pop_count", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  output$population_count_table <- renderDataTable({
    datatable(
      getCount(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$population_count_table_download <-  serverCSVDownload(
    name = "populationCount", getCount()
  )
  getCountGT <- reactive({
    getCount() |>
      select(!starts_with("num")) |>
      mutate(estimate_name = "Number individuals") |>
      rename("estimate_value" = "total") |>
      formatHeader(
        header = c("strata_name", "strata_level"),
        includeHeaderName = FALSE
      ) |>
      gtTable(groupNameCol = "cdm_name", groupNameAsColumn = TRUE)
  })
  output$population_count_summary <- render_gt({
    getCountGT()
  })
  output$population_count_summary_download <- serverGTDownload(
    name = "populationCount", gt = getCountGT()
  )
  # baseline ----
  output$baseline_strata_level <- reactiveSelectors(
    data = data$baseline, prefix = "baseline", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getBaseline <- reactive({
    data$baseline |>
      filterData(prefix = "baseline", input = input) |>
      filter(variable_name != "Number subjects") |>
      select(!starts_with("group")) |>
      uniteGroup(cols = c("comparison", "covid_definition")) |>
      tableCharacteristics(
        header = c("cdm_name", "additional"),
        excludeColumns = c(
          "result_id", "result_type", "package_name","package_version", "estimate_type"
        )
      )
  })
  output$baseline_table <- render_gt({
    getBaseline()
  })
  output$baseline_table_download <- serverGTDownload(
    name = "baseline", gt = getBaseline()
  )
  # large scale ----
  output$large_scale_strata_level <- reactiveSelectors(
    data = data$large_scale, prefix = "large", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getLargeScale <- reactive({
    data$large_scale |>
      filterData(prefix = "large", input = input) |>
      pivot_wider(
        names_from = c("estimate_name", "exposed"),
        values_from = "estimate_value"
      )
  })
  output$large_scale_table <- renderDataTable({
    datatable(getLargeScale(), options = list(pageLength = 25))
  })
  output$large_scale_download_table <- serverCSVDownload(
    name = "largeScale", table = getLargeScale()
  )
  # SMD ----
  output$smd_strata_level <- reactiveSelectors(
    data = data$smd, prefix = "smd", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getSMD <- reactive({
    data$smd |>
      filterData(prefix = "smd", input = input) |>
      distinct()
  })
  output$smd_table <- renderDataTable({
    datatable(getSMD(), options = list(pageLength = 25))
  })
  output$smd_download_table <- serverCSVDownload(
    name = "SMD", table = getSMD()
  )
  # NCO ----
  output$nco_summary_strata_level <- reactiveSelectors(
    data = data$survival_summary |> filter(variable_name == "nco"),
    prefix = "nco_summ", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getNCOSummaryRaw <- reactive({
    data$survival_summary |>
      filterData(prefix = "nco_summ", input = input) |>
      filter(variable_name == "nco") |>
      select(!"estimate_type") |>
      pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
      select(!c("variable_name", "window", "exposed_censoring", "delivery_excluded"))
  })
  output$nco_summary_raw <- renderDataTable({
    datatable(getNCOSummaryRaw(),
              rownames = FALSE,
              extensions = "Buttons",
              options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25))
  })
  output$nco_summary_download_raw <- serverCSVDownload(
    name = "summaryNCO", table = getNCOSummaryRaw()
  )
  getGTNCOSummary <- reactive({
    data$survival_summary |>
      filterData(prefix = "nco_summ", input = input) |>
      filter(variable_name == "nco") |>
      formatEstimateValue() |>
      formatEstimateName(
        estimateNameFormat = c(
          "Subjects (N)" = "<count>",
          "Events (N)" = "<count_events>",
          "Follow-up, Median [Q25-Q75]"  = "<followup_median> [<followup_q25>-<followup_q75>]",
          "Follow-up, Mean (SD)"  = "<followup_mean> [(<followup_sd>)",
          "Follow-up, [Min-Max]"  = "[<followup_min>-<followup_max>]",
          "Age, Median [Q25-Q75]"  = "<age_median> [<age_q25>-<age_q75>]",
          "Age, Mean (SD)"  = "<age_mean> [(<age_sd>)",
          "Age, [Min-Max]"  = "[<age_min>-<age_max>]"
        ),
        keepNotFormatted = FALSE
      ) |>
      formatHeader(
        header = c("strata_name", "strata_level", "exposed"),
        includeHeaderName = FALSE,
      ) |>
      arrange(cdm_name, comparison, covid_definition) |>
      select(!c("estimate_type")) |>
      relocate(c("window", "followup_end"), .before = "outcome") |>
      select(!c("variable_name", "window", "exposed_censoring", "delivery_excluded")) |>
      gtTable(groupNameCol = "cdm_name", groupNameAsColumn = TRUE, colsToMergeRows = "all_columns")
  })
  output$nco_summary_table <- render_gt({
    getGTNCOSummary()
  })
  output$nco_summary_download_table <- serverGTDownload(name = "summaryNCO", gt = getGTNCOSummary())
  # STUDY ----
  output$study_summary_strata_level <- reactiveSelectors(
    data = data$survival_summary |> filter(variable_name == "study"),
    prefix = "study_summ", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getOutcomesSummaryRaw <- reactive({
    data$survival_summary |>
      filterData(prefix = "study_summ", input = input) |>
      filter(
        variable_name == "study",
        delivery_excluded %in% input$delivery_sum | delivery_excluded == "-"
      ) |>
      select(!c("estimate_type")) |>
      pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
      select(!c("variable_name", "window", "exposed_censoring"))
  })
  output$study_summary_raw <- renderDataTable({
    datatable(getOutcomesSummaryRaw(),
              rownames = FALSE,
              extensions = "Buttons",
              options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25))
  })
  output$study_summary_download_raw <- serverCSVDownload(
    name = "summaryOutcomes", table = getOutcomesSummaryRaw()
  )
  getGTOutcomesSummary <- reactive({
    data$survival_summary |>
      filterData(prefix = "study_summ", input = input) |>
      filter(
        variable_name == "study",
        delivery_excluded %in% input$delivery_sum | delivery_excluded == "-"
      ) |>
      formatEstimateValue() |>
      formatEstimateName(
        estimateNameFormat = c(
          "Subjects (N)" = "<count>",
          "Events (N)" = "<count_events>",
          "Follow-up, Median [Q25-Q75]"  = "<followup_median> [<followup_q25>-<followup_q75>]",
          "Follow-up, Mean (SD)"  = "<followup_mean> [(<followup_sd>)",
          "Follow-up, [Min-Max]"  = "[<followup_min>-<followup_max>]",
          "Age, Median [Q25-Q75]"  = "<age_median> [<age_q25>-<age_q75>]",
          "Age, Mean (SD)"  = "<age_mean> [(<age_sd>)",
          "Age, [Min-Max]"  = "[<age_min>-<age_max>]"
        ),
        keepNotFormatted = FALSE
      ) |>
      formatHeader(
        header = c("strata_name", "strata_level", "exposed"),
        includeHeaderName = FALSE,
      ) |>
      arrange(cdm_name, comparison, covid_definition, window) |>
      select(!c("estimate_type", "exposed_censoring")) |>
      relocate(c("window", "followup_end"), .before = "outcome") |>
      select(!c("variable_name")) |>
      gtTable(groupNameCol = "cdm_name", groupNameAsColumn = TRUE, colsToMergeRows = "all_columns")
  })
  output$study_summary_table <- render_gt({
    getGTOutcomesSummary()
  })
  output$study_summary_download_table <- serverGTDownload(name = "summaryOutcomes", gt = getGTOutcomesSummary())
  # NCO FOREST ----
  output$nco_risk_strata_level <- reactiveSelectors(
    data = data$risk |> filter(variable_name == "nco"),
    prefix = "nco_risk", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getNCOForestRaw <- reactive({
    data$risk |>
      filterData(prefix = "nco_risk", input = input) |>
      filter(variable_name == "nco") |>
      select(!"estimate_type") |>
      pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
      select(!c("variable_name", "delivery_excluded"))
  })
  output$nco_risk_raw <- renderDataTable({
    datatable(getNCOForestRaw(),
              rownames = FALSE,
              extensions = "Buttons",
              options = list(scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25))
  })
  output$nco_risk_download_raw <- serverCSVDownload(
    name = "estimatesNCO", table = getNCOForestRaw()
  )
  getNCOForestTable <- reactive({
    data$risk |>
      filterData(prefix = "nco_risk", input = input) |>
      filter(variable_name == "nco") |>
      formatEstimateValue() |>
      formatEstimateName(
        estimateNameFormat = c("Point estimate [95% CI]" = "<exp_coef> [<lower_ci>, <upper_ci>]"),
        keepNotFormatted = FALSE
      ) |>
      formatHeader(
        header = c("estimate_name", "cdm_name", "strata_name", "strata_level"),
        includeHeaderName = FALSE,
      ) |>
      arrange(comparison, covid_definition) |>
      select(!c("estimate_type", "variable_name", "delivery_excluded")) |>
      gtTable(colsToMergeRows = "all_columns")
  })
  output$nco_risk_table <- render_gt({
    getNCOForestTable()
  })
  output$nco_risk_download_table <- serverGTDownload(name = "summaryNCO", gt = getNCOForestTable())
  getNCOForestPlot <- reactive({
    format <- c("cdm_name", "strata_level", "regression", "window", "outcome")
    format <- format[!format %in% input$plt_nco_risk_facet_by]

    table <- data$risk |>
      filterData(prefix = "nco_risk", input = input) |>
      filter(variable_name == "nco") |>
      select(!"estimate_type") |>
      pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
      mutate(
        outcome_plot = glue::glue(paste0("{", paste0(format, collapse = "}; {"), "}")),
        association = case_when(
          lower_ci > 1 ~ "positive association",
          upper_ci < 1 ~ "negative association",
          lower_ci <= 1 & upper_ci >= 1 ~ "no association",
          .default = "no association"
        )
      ) |>
      left_join(
        data$survival_summary |>
          filter(grepl("count", estimate_name)) |>
          pivot_wider(names_from = c("estimate_name", "exposed"), values_from = "estimate_value")
      )

    if (!is.null(input[[paste0("plt_nco_risk_facet_by")]])) {
      table <- table %>%
        unite("facet_var",
              c(all_of(input[[paste0("plt_nco_risk_facet_by")]])), remove = FALSE, sep = "; ")
    }

    if (!is.null(input[[paste0("plt_nco_risk_color")]])) {
      table <- table %>%
        unite("Group",
              c(all_of(input[[paste0("plt_nco_risk_color")]])), remove = FALSE, sep = "; ")
      p <- table %>%
        ggplot(aes(x = exp_coef, y = outcome, color = Group, label1 = lower_ci, label2 = upper_ci,
                   label3 = count_unexposed, label4 = count_exposed, label5 = count_events_unexposed, label6 = count_events_exposed))
    } else {
      p <- table %>%
        ggplot(aes(x = exp_coef, y = outcome, label1 = lower_ci, label2 = upper_ci,
                   label3 = count_unexposed, label4 = count_exposed, label5 = count_events_unexposed, label6 = count_events_exposed))
    }
    p <- p +
      geom_vline(xintercept = 1) +
      geom_point() +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), linewidth = 0.8) +
      scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
                         labels = c(0.1, 0.25, 0.5, 1, 2, 4),
                         limits = c(0.1, 4),
                         trans = "log10",
                         oob = scales::rescale_none) +
      ylab("") +
      xlab("")

    if (!is.null(input[[paste0("plt_nco_risk_facet_by")]])) {
      p <- p + facet_wrap(vars(facet_var), ncol = 2)
    }
    p
  })
  output$nco_risk_plot <- renderPlotly({
    getNCOForestPlot()
  })
  output$nco_risk_download_plot <- serverPlotDownload(
    prefix = "dwn_nco_risk", name = "forestNCO", plot = getNCOForestPlot(), input = input
  )
  # STUDY FOREST ----
  output$study_risk_strata_level <- reactiveSelectors(
    data = data$risk |> filter(variable_name == "study"),
    prefix = "study_risk", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  getStudyForestRaw <- reactive({
    data$risk |>
      filterData(prefix = "study_risk", input = input) |>
      filter(
        variable_name == "study",
        delivery_excluded %in% input$delivery_risk | delivery_excluded == "-"
      ) |>
      select(!c("estimate_type", "exposed_censoring")) |>
      pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
      select(!c("variable_name")) |>
      arrange(cdm_name, comparison, covid_definition, window) %>%
      {if (input$study_risk_raw_hr == "Vaccine effectiveness") {
        mutate(., exp_coef = (1-exp_coef)*100, lower_ci_1 = (1-lower_ci)*100,
               lower_ci = (1-upper_ci)*100, upper_ci = lower_ci_1, se_coef = se_coef*100) |>
          select(!c("coef", "lower_ci_1"))
      } else .}
  })
  output$study_risk_raw <- renderDataTable({
    datatable(getStudyForestRaw(),
              rownames = FALSE,
              extensions = "Buttons",
              options = list
              (scrollX = TRUE, scrollCollapse = TRUE, pageLength = 25))
  })
  output$study_risk_download_raw <- serverCSVDownload(
    name = "estimatesStudy", table = getStudyForestRaw()
  )
  getStudyForestTable <- reactive({
    data$risk |>
      filterData(prefix = "study_risk", input = input) |>
      filter(
        variable_name == "study",
        delivery_excluded %in% input$delivery_risk | delivery_excluded == "-"
      ) %>%
      {if (input$study_risk_format_hr == "Vaccine effectiveness") {
        mutate(.,
          estimate_value = as.character((1-as.numeric(estimate_value))*100)
          ) |>
          formatEstimateValue() |>
          formatEstimateName(
            estimateNameFormat = c("Point estimate [95% CI]" = "<exp_coef>% [<upper_ci>, <lower_ci>]"),
            keepNotFormatted = FALSE
          )
      } else {
        formatEstimateValue(.) |>
          formatEstimateName(
            estimateNameFormat = c("Point estimate [95% CI]" = "<exp_coef> [<lower_ci>, <upper_ci>]"),
            keepNotFormatted = FALSE
          )
      }} %>%
      arrange(cdm_name, comparison, covid_definition, window) |>
      formatHeader(
        header = c("estimate_name", "cdm_name", "strata_name", "strata_level"),
        includeHeaderName = FALSE,
      ) |>
      select(!c("estimate_type", "variable_name", "exposed_censoring")) |>
      gtTable(colsToMergeRows = "all_columns")
  })
  output$study_risk_table <- render_gt({
    getStudyForestTable()
  })
  output$study_risk_download_table <- serverGTDownload(name = "summaryStudy", gt = getStudyForestTable())
  getStudyForestPlot <- reactive({
    format <- c("cdm_name", "strata_level", "window", "outcome")
    format <- format[!format %in% input$plt_study_risk_facet_by]

    table <- data$risk |>
      filterData(prefix = "study_risk", input = input) |>
      filter(
        variable_name == "study",
        delivery_excluded %in% input$delivery_risk | delivery_excluded == "-"
      ) |>
      select(!"estimate_type") |>
      pivot_wider(names_from = "estimate_name", values_from = "estimate_value")  %>%
      # {if (input$study_risk_hr == "Vaccine effectiveness") {
      #   mutate(., coef = 1-coef, exp_coef = 1-exp_coef, lower_ci = 1-lower_ci, upper_ci = 1-upper_ci)
      # } else .} %>%
      mutate(
        outcome_plot = glue::glue(paste0("{", paste0(format, collapse = "}; {"), "}")),
        outcome_plot = if_else(.data$delivery_excluded == "no", paste0(outcome_plot, "; delivery_not_excluded"), outcome_plot),
        association = case_when(
          lower_ci > 1 ~ "positive association",
          upper_ci < 1 ~ "negative association",
          lower_ci <= 1 & upper_ci >= 1 ~ "no association",
          .default = "no association"
        )
      ) |>
      left_join(
        data$survival_summary |>
          filter(grepl("count", estimate_name)) |>
          pivot_wider(names_from = c("estimate_name", "exposed"), values_from = "estimate_value")
      ) |>
      mutate(window = factor(window, levels = c("0_14", "15_Inf", "15_28", "15_90", "15_180", "15_365", "29_90", "29_180", "91_180", "181_365", "366_Inf"))) |>
      arrange(cdm_name, comparison, covid_definition, window)

    orderY <- unique(table$outcome_plot)

    table <- table |> mutate(outcome_plot = factor(outcome_plot, levels = orderY))

    if (!"i2" %in% colnames(table)) {
      table <- table |> mutate(i2 = NA)
    }

    if (!is.null(input[[paste0("plt_study_risk_facet_by")]])) {
      table <- table %>%
        unite("facet_var",
              c(all_of(input[[paste0("plt_study_risk_facet_by")]])), remove = FALSE, sep = "; ")
    }

    if (!is.null(input[[paste0("plt_study_risk_color")]])) {
      table <- table %>%
        unite("Group",
              c(all_of(input[[paste0("plt_study_risk_color")]])), remove = FALSE, sep = "; ")
      p <- table %>%
        ggplot(aes(x = exp_coef, y = outcome_plot, color = Group, label = outcome, label1 = lower_ci, label2 = upper_ci,
                   label3 = count_unexposed, label4 = count_exposed, label5 = count_events_unexposed,
                   label6 = count_events_exposed, label7 = i2))
    } else {
      p <- table %>%
        ggplot(aes(x = exp_coef, y = outcome_plot, label = outcome, label1 = lower_ci, label2 = upper_ci,
                   label3 = count_unexposed, label4 = count_exposed, label5 = count_events_unexposed,
                   label6 = count_events_exposed, label7 = i2))
    }
    p <- p +
      geom_vline(xintercept = 1) +
      geom_point() +
      geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), linewidth = 0.8) +
      scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2),
                         labels = c(0.1, 0.25, 0.5, 1, 2),
                         limits = c(0.1, 2),
                         trans = "log10",
                         oob = scales::rescale_none) +
      ylab("") +
      xlab("")

    if (!is.null(input[[paste0("plt_study_risk_facet_by")]])) {
      p <- p + facet_wrap(vars(facet_var), ncol = 2)
    }
    p
  })
  output$study_risk_plot <- renderPlotly({
    getStudyForestPlot()
  })
  output$study_risk_download_plot <- serverPlotDownload(
    prefix = "dwn_study_risk", name = "forestStudy", plot = getStudyForestPlot(), input = input
  )
  # KAPLAN MEIER ----
  output$km_strata_level <- reactiveSelectors(
    data = data$kaplan_meier,
    prefix = "km", columns = "strata_level",
    restrictions = "strata_name", input = input, multiple = TRUE
  )
  plotKM <- reactive({
    table <- data$kaplan_meier |>
      filterData("km", input = input) |>
      filter(delivery_excluded %in% input$delivery_km | delivery_excluded == "-")
    if (!is.null(input$plt_km_facet_by)) {
      table <- table |>
        unite("facet_by", input$plt_km_facet_by, sep = "; ", remove = FALSE)
    }
    gg <- table |>
      ggplot(aes(x = time, y = estimate, color = Cohort, fill = Cohort, ymin = estimate_95CI_lower, ymax = estimate_95CI_upper)) +
      geom_step(size = 1) +
      geom_ribbon(alpha = 0.3, colour = NA) +
      scale_color_manual(values = c("#87b38d", "#0d3b66")) +
      scale_fill_manual(values = c("#87b38d", "#0d3b66")) +
      theme_bw() +
      xlab("Time (days)") +
      ylab("Survival probability")
    if (!is.null(input$plt_km_facet_by)) {
      gg <- gg +
        facet_wrap(vars(facet_by))
    }
    gg
  })
  output$km_plot <- renderPlotly({
    plotKM()
  })
  output$km_download_plot <- serverPlotDownload(
    prefix = "dwn_km", name = "kaplanMeier", plot = plotKM(), input = input
  )
  # log log
  plotLogLog <- reactive({
    table <- data$kaplan_meier |>
      filterData("km", input = input) |>
      filter(delivery_excluded %in% input$delivery_km | delivery_excluded == "-") |>
      mutate(estimate = log(-log(estimate)))
    if (!is.null(input$plt_km_facet_by)) {
      table <- table |>
        unite("facet_by", input$plt_km_facet_by, sep = "; ", remove = FALSE)
    }
    gg <- table |>
      ggplot(aes(x = time, y = estimate, color = Cohort, fill = Cohort)) +
      geom_step(size = 1) +
      scale_color_manual(values = c("#87b38d", "#0d3b66")) +
      scale_fill_manual(values = c("#87b38d", "#0d3b66")) +
      scale_x_continuous(trans='log10') +
      theme_bw() +
      xlab("Time (days)") +
      ylab("log(-log(S(t)))")
    if (!is.null(input$plt_km_facet_by)) {
      gg <- gg +
        facet_wrap(vars(facet_by))
    }
    gg
  })
  output$loglog_plot <- renderPlotly({
    plotLogLog()
  })
  output$loglog_download_plot <- serverPlotDownload(
    prefix = "dwn_loglog", name = "logLog", plot = plotLogLog(), input = input
  )
  # FOLLOW-UP ----
  output$followup_summary_table <- render_gt({
    data$censoring |>
      filterData("followup", input) |>
      gtTable(colsToMergeRows = "all_columns", groupNameCol = "CDM name")
  })
}
