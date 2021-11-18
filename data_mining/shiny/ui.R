library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)

ui <- fluidPage(
  fileInput("dataFile", "Sélectionner le fichier RData (rds)",
            accept = c(".rds"),
            buttonLabel = "Cliquer ici",
            placeholder = "Aucun fichier sélectionné"),
  pickerInput("select_vars", "Sélectionner les variables à explorer",
              choices = NULL,
              multiple = T,
              options = list(`actions-box` = T)),
  textInput("path", "Path", getwd(), width = 1000),
  fluidRow(actionButton("dl_corrs", "Downloader nuages de points"),
           actionButton("dl_histo", "Downloader histogrammes"),
           actionButton("dl_densite", "Downloader densité"),
           actionButton("dl_bars", "Downloader bar-graphs"))
)