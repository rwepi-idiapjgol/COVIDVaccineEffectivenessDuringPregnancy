## Read data ----
readData <- function(path) {
  x <- list.files(path = path)
  csvFiles <- x[tools::file_ext(x) == "csv"]
  zipFiles <- x[tools::file_ext(x) == "zip"]
  tempfolder <- tempdir()
  data <- readFiles(file.path(path, csvFiles))
  for (file in zipFiles) {
    file <- file.path(path, file)
    fname = unzip(file, list=TRUE)$Name
    fname <- fname[tools::file_ext(fname) == "csv"]
    unzip(file, files=fname, exdir=tempfolder, overwrite=TRUE)
    files <- file.path(tempfolder, fname)
    data <- c(data, readFiles(files))
  }
  return(data)
}

readFiles <- function(files) {
  data <- list()
  for (file in files) {
    data[[file]] <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()))
  }
  names(data) <- basename(tools::file_path_sans_ext(names(data)))
  return(data)
}

mergeData <- function(data, patterns) {
  x <- list()
  for (pat in patterns) {
    x[[pat]] <- data[grepl(pat, names(data))] %>% dplyr::bind_rows() %>% distinct()
  }
  return(x)
}

## Shiny creation ----
selectors <- function(data, prefix, columns, multiple = TRUE, default = list()) {
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col) {
    data[[col]] %>% unique() %>% sort()
  }
  purrr::map(columns, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choic(.),
    selected = def(.),
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

plotSelectors <- function(prefix, choices, multiple = TRUE, default = list(), selectors = c("color", "facet_by")) {
  purrr::map(selectors, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choices,
    selected = default[[.]],
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

reactiveSelectors <- function(data, prefix, columns, restrictions, input,
                              multiple = TRUE, default = list()) {
  if (length(restrictions) != length(columns)) {
    if (length(restrictions) == 1) {
      restrictions <- rep(restrictions, length(columns))
    } else {
      cli::cli_abort("Revise columns and restrictions arguments.")
    }
  }

  names(columns) <- restrictions

  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col, dict = columns, input = input)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col, dict, input) {
    filterCol <- names(dict)[dict == col]
    return(sort(unique(data[[col]][data[[filterCol]] %in% input[[paste0(prefix, "_", filterCol)]]])))
  }
  renderUI({
    purrr::map(columns, ~ pickerInput(
      inputId = paste0(prefix, "_", .),
      label = stringr::str_to_sentence(gsub("_", " ", .)),
      choices = choic(., dict = columns, input = input),
      selected = def(.),
      options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
      multiple = multiple,
      inline = TRUE
    ))
  })
}

plotDownloadSelectors <- function(prefix,
                                  choices = list("device" = c("png", "jpeg", "tiff", "pdf"),
                                                 "width" = 1:50,
                                                 "height"= 1:50),
                                  multiple = FALSE,
                                  default = list("device" = "png", "width" = 10, "height" = 10),
                                  types = c("device", "width", "height")) {
  purrr::map(types, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choices[[.]],
    selected = default[[.]],
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

serverPlotDownload <- function(prefix, name, plot, input) {
  downloadHandler(
    filename = function() {
      paste0(name, ".", input[[paste0(prefix, "_device")]])
    },
    content = function(file) {
      ggsave(file, plot,
             width = as.numeric(input[[paste0(prefix, "_width")]]),
             height = as.numeric(input[[paste0(prefix, "_height")]]))
    }
  )
}

serverGTDownload <- function(name, gt) {
  downloadHandler(
    filename = function() {
      paste0(name, ".docx")
    },
    content = function(file) {
      gtsave(data = gt,
             filename = file,
             vwidth = 400,
             vheight = 300)
    },
    contentType = "docx"
  )
}

serverCSVDownload <- function(name, table) {
  downloadHandler(
    filename = function() {
      paste0(name, ".csv")
    },
    content = function(file) {
      write_csv(table, file)
    }
  )
}

filterData <- function(data, prefix, input) {
  cols <- colnames(data)
  cols <- cols[paste0(prefix, "_", cols) %in% names(input)]
  for (col in cols) {
    data <- data %>%
      dplyr::filter(.data[[col]] %in% .env$input[[paste0(prefix, "_", col)]])
  }
  return(data)
}

niceChar <- function(x, cols = everything()) {
  x %>%
    rename_with(.fn = ~ stringr::str_to_sentence(gsub("_", " ", .x)), .cols = cols) %>%
    rename("CDM name" = "Cdm name")
}

niceNum <- function(x, dec = 0) {
  trimws(format(round(as.numeric(x), dec), big.mark = ",", nsmall = dec, scientific = FALSE))
}

niceCohortName <- function(x, col = "cohort_name", removeCol = TRUE) {
  x <- x |>
    mutate(
      covid_definition = if_else(
        grepl("covid_diagnostic_test", .data[[col]]), "diagnostic_test", "test"),
      comparison = gsub("_covid_diagnostic_test|_covid_test", "", .data[[col]])
    ) |>
    relocate("covid_definition", .after = "comparison")
  if (removeCol) {
    x <- x |> select(!all_of(col))
  }
  return(x)
}

niceOutcomeName <- function(x, col  = "outcome") {
  x |>
    mutate(
      covid_definition_out = if_else(
        grepl("covid_diagnostic_test", .data[[col]]), "diagnostic_test", "test"),
      delivery_excluded = case_when(
        variable_name == "nco" ~ NA,
        grepl("no_delivery", .data[[col]]) ~ "yes",
        !grepl("no_delivery", .data[[col]]) & grepl("inpatient|icu", .data[[col]]) ~ "no",
        !grepl("no_delivery", .data[[col]]) & !grepl("inpatient|icu", .data[[col]]) ~ "-"
      ),
      outcome = gsub("_diagnostic_test|_test|_no_delivery", "", .data[[col]])
    ) |>
    filter(covid_definition == covid_definition_out | variable_name == "nco")  |>
    relocate("delivery_excluded", .after = !!col) |>
    select(!covid_definition_out)
}

## Figures ----
# logRrtoSE + plotCiCalibrationEffect_NMB,  used to plot NCO
logRrtoSE <- function(logRr, alpha, mu, sigma) {
  phi <- (mu - logRr)^2 / qnorm(alpha / 2)^2 - sigma^2
  phi[phi < 0] <- 0
  se <- sqrt(phi)
  return(se)
}
plotCiCalibrationEffect_NMB <- function(logRr,
                                        seLogRr,
                                        trueLogRr,
                                        legacy = FALSE,
                                        model = NULL,
                                        xLabel = "Relative risk",
                                        title,
                                        fileName = NULL) {
  alpha <- 0.05
  if (is.null(model)) {
    model <- fitSystematicErrorModel(
      logRr = logRr,
      seLogRr = seLogRr,
      trueLogRr = trueLogRr,
      estimateCovarianceMatrix = FALSE,
      legacy = legacy
    )
  } else {
    legacy <- (names(model)[3] == "logSdIntercept")
  }
  d <- data.frame(
    logRr = logRr,
    seLogRr = seLogRr,
    trueLogRr = trueLogRr,
    trueRr = exp(trueLogRr),
    logCi95lb = logRr + qnorm(0.025) * seLogRr,
    logCi95ub = logRr + qnorm(0.975) * seLogRr
  )
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$seLogRr), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$logCi95lb > d$trueLogRr | d$logCi95ub < d$trueLogRr

  temp1 <- aggregate(Significant ~ trueRr, data = d, length)
  temp2 <- aggregate(Significant ~ trueRr, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  temp2$meanLabel <- paste0(
    formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
    "% of CIs includes ",
    temp2$trueRr
  )
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 10)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 10, hjust = 1)

  d$Group <- ""
  dd$Group <- ""

  x <- seq(log(0.1), log(10), by = 0.01)
  calBounds <- data.frame()
  for (i in 1:nrow(dd)) {
    mu <- model[1] + model[2] * log(dd$trueRr[i])
    if (legacy) {
      sigma <- exp(model[3] + model[4] * log(dd$trueRr[i]))
    } else {
      sigma <- model[3] + model[4] * abs(log(dd$trueRr[i]))
    }
    calBounds <- rbind(
      calBounds,
      data.frame(
        logRr = x,
        seLogRr = logRrtoSE(x, alpha, mu, sigma),
        Group = dd$Group[i]
      )
    )
  }
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$logRr, y = .data$seLogRr)) +
    ggplot2::geom_area(
      fill = "#cce3de",
      color = "#cce3de",
      size = 1,
      alpha = 0.5,
      data = tibble(logRr = 0:1000, seLogRr =  -(0:1000)/qnorm(0.025))) +
    ggplot2::geom_area(
      fill = "#cce3de",
      color = "#cce3de",
      size = 1,
      alpha = 0.5,
      data = tibble(logRr = -(0.001:100), seLogRr =  -(0.001:100)/qnorm(0.025))) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.025), slope = 1 / qnorm(0.025)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$trueRr)) / qnorm(0.975), slope = 1 / qnorm(0.975)), colour = rgb(0, 0, 0), linetype = "dashed", size = 1, alpha = 0.5, data = dd) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    # ggplot2::geom_area(
    #   fill = rgb(1, 0.5, 0, alpha = 0.5),
    #   color = rgb(1, 0.5, 0),
    #   size = 1,
    #   alpha = 0.5, data = calBounds
    # ) +
    ggplot2::geom_point(
      shape = 16,
      size = 2,
      alpha = 0.5,
      color = rgb(0, 0, 0.8)
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    # ggplot2::geom_label(x = log(0.15), y = 0.95, alpha = 1, hjust = "left", ggplot2::aes(label = .data$nLabel), size = 3.5, data = dd) +
    # ggplot2::geom_label(x = log(0.15), y = 0.8, alpha = 1, hjust = "left", ggplot2::aes(label = .data$meanLabel), size = 3.5, data = dd) +
    ggplot2::scale_x_continuous(xLabel, limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::facet_grid(. ~ Group) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      # axis.title = theme,
      legend.key = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      strip.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      legend.position = "none"
    )

  if (!is.null(fileName)) {
    ggsave(width = 7, height = 4, dpi = 600,
           filename = fileName)
  }
  return(plot)
}
