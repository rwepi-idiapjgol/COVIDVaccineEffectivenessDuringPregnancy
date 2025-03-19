# Renv
# renv::activate()
# renv::restore()
# .rs.restartR()

# Libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(gt)
library(ggplot2)
library(plotly)
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(fresh)
library(rclipboard)
library(bit)
library(bit64)
library(rsconnect)
library(packrat)
library(visOmopResults)
library(shiny)
library(googledrive)

# Google drive authenticator
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

# Variables
dataFolder <- "data"

# Functions
source(here("functions_shiny.R"))

# Data
source(here("prepare_data.R"))


# Shiny theme ----
DUtheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#78B7C5",
    dark_hover_bg = "#3B9AB2",
    dark_color = "white"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)


# App
source(here("ui.R"))
source(here("server.R"))
shinyApp(ui, server)
