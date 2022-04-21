library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinymanager)
library(tidyverse)
library(shinythemes)
library(dashboardthemes)

Data <- readRDS("ShinyData.rds") %>%
  filter(!grepl('voteIntent', axisX) &
         !grepl('educ', axisX) &
         !grepl('turnout', axisX) &
         !grepl("potential", axisX) &
         !grepl("income", axisX) &
         !grepl("quebec", axisX) &
         !grepl("ontario", axisX) &
         !grepl("maritimes", axisX) &
         !grepl("west", axisX) &
         !grepl("male", axisX) &
         !grepl("female", axisX) &
         !grepl("age", axisX) &
         !grepl("lang", axisX))
source(paste0(getwd(), "/clessnTheme.R"))

choicesGroup <- list(group_age = c("age34m", "age3554", "age55p"),
                     group_educ = c("educBHS", "educUniv"),
                     group_income = c("incomeLow", "incomeMid", "incomeHigh"),
                     group_region = c("ontario", "quebec", "maritimes", "west"),
                     group_sexOri = c("ses_hetero", "ses_sexOri_other"),
                     group_immigrant = c("immigrant"),
                     group_habit = c("ses_dwelling_loft", "ses_dwelling_detachedHouse", "ses_dwelling_townHouse",
                                     "ses_dwelling_semiDetached", "ses_dwelling_HLM", "ses_dwelling_mobile", "ses_dwelling_cityAppMerged",
                                     "ses_dwelling_other"),
                     group_sex = c("male", "female"))

choicesAxisX <- unique(Data$axisX)
choicesDependent <- unique(Data$dependent)

css <- "
.selectize-input {
  background-color: rgb(210, 210, 210);
}

.selectize-item {
  color: rgb(247, 245, 245);
}
.selectize-option {
  color: rgb(247, 245, 245);
}
.selectize-option:hover {
  background-color: rgb(254, 236, 32);
  color: black;
}
.selectize-oheader {
  color: green !important;
  font-size: 18px !important;
}
input[type='text'], textarea {
  background-color : rgb(211,211,211);
  color : black;
}
"

ui <- shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  tags$head(tags$style(HTML(css))),
  dashboardPage(
    dashboardHeader(title = "Lifestyle - voteIntent"),
    dashboardSidebar(
      numericInput("seuil_sign", "Seuil de significance",
                   value = 0.05, min = 0, max = 1,
                   step = 0.01)),
    dashboardBody(
      clessnTheme,
      fluidRow(
        column(3),
        column(2, pickerInput("party", "Intention de vote\n",
                              choices = choicesDependent,
                              selected = choicesDependent[sample(1:length(choicesDependent), 1)],
                              multiple = T,
                              options = pickerOptions(`actions-box` = TRUE,
                                                      `liveSearch` = TRUE))),
        column(2, pickerInput("axisX", "Axe des x\n",
                              choices = choicesAxisX,
                              selected = choicesAxisX[sample(1:length(choicesAxisX), 1)],
                              multiple = T,
                              options = pickerOptions(`actions-box` = TRUE,
                                                      `liveSearch` = TRUE))),
        column(2, pickerInput("group", "Groupe\n",
                              choices = choicesGroup,
                              multiple = T,
                              options = pickerOptions(`actions-box` = TRUE,
                                                      `liveSearch` = TRUE))),
        column(3)
      ),
      fluidRow(
        column(1),
        column(10,
               plotOutput("plot", width = "100%",
                          height = "600px"))
    )
    )
  )
  )
)