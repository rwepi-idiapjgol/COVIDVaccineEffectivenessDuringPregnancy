# Shiny utils ----
selectors <- function(data, prefix, columns, multiple = TRUE, default = list()) {
  if(!is.null(data)) {
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col)[1]
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
}

plotSelectors <- function(prefix, choices, multiple = TRUE, default = list(), type = c("color", "facet_by")) {
  purrr::map(type, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choices,
    selected = default[[.]],
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
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

niceColumnNames <- function(x, cols = everything()) {
  x %>% rename_with(.fn = ~ stringr::str_to_sentence(gsub("_", " ", .x)), .cols = cols)
}

niceNum <- function(x, dec = 0) {
  trimws(format(round(as.numeric(x), dec), big.mark = ",", nsmall = dec, scientific = FALSE))
}

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

# PhenotyperR utils ----
formatMarkdown <- function(x) {
  lines <- strsplit(x, "\r\n\r\n") |> unlist()
  getFormat <- function(line) {
    if (grepl("###", line)) {return(h3(gsub("###", "", line)))} 
    else {h4(line)} 
  }
  purrr::map(lines, ~ getFormat(.))
}
formatLog <- function(x) {
  lines <- strsplit(x |>  str_replace_all(": :", ":") , "\n") |> unlist() 
  getFormat <- function(line) {
    line <- strsplit(line, ":") |> unlist()
    return(list(h4(line[1]), h5(paste0(gsub(" elapsed", "", line[2])))))
  }
  purrr::map(lines, ~ getFormat(.))
}

lscToSummarisedResult <- function(lsc) {
  lsc %>%
    mutate(
      "result_type" = "lsc",
      "package_name" = "PatientProfiles",
      "package_version" = "0.5.2",
      "variable_name" = variable,
      "estimate_name" = estimate_type,
      "estimate_type" = if_else(
        estimate_name == "count", "integer", "percentage"
      ),
      "estimate_value" = estimate,
      "group_name" = snakecase::to_snake_case(group_name),
      "strata_name" = tolower(strata_name),
      "strata_level" = tolower(strata_level)
    ) %>%
    uniteAdditional(
      cols = c("table_name", "type", "concept")
    ) %>%
    select(
      "cdm_name", "result_type", "package_name", "package_version",
      "group_name", "group_level", "strata_name", "strata_level",
      "variable_name", "variable_level", "estimate_name", "estimate_type",
      "estimate_value", "additional_name", "additional_level"
    )
  
}
