# restore library:
# renv::activate()
# renv::restore()

# libraries ----
library(dplyr)
library(readr)
library(here)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(ggplot2)
library(stringr)
library(gt)
library(omopgenerics)
library(visOmopResults)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(glue)
library(stats)
library(PatientProfiles)
library(CohortSurvival)

# load functions
source(here("functions.R"))

# load data
load(here("shinyData-old.Rdata"))

# run shiny
source(here("server.R"))
source(here("ui.R"))
shinyApp(ui, server)
