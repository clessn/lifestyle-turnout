library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinymanager)
library(tidyverse)
library(shinythemes)
library(dashboardthemes)

Data <- readRDS("")
source(paste0(getwd(), "/clessnTheme.R"))
source(paste0(getwd(), "/auths.R"))


choicesAxisXVote <- unique(DataVote$cleanAxisX)
choicesDependentVote <- unique(DataVote$cleanDependent)
choicesGroupVote <- unique(DataVote$cleanGroup)

choicesAxisXAttach <- unique(DataAttach$cleanAxisX)
choicesDependentAttach <- unique(DataAttach$cleanDependent)
choicesGroupAttach <- unique(DataAttach$cleanGroup)


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
  auth_ui("auth",
          lan = use_language("fr"),
          background = "linear-gradient(rgb(64,64,64),
                       rgb(64,64,64))"),
  dashboardPage(
    dashboardHeader(title = "Segmentation"),
    dashboardSidebar(
      numericInput("seuil_sign", "Seuil de significance",
                   value = 0.05, min = 0, max = 1,
                   step = 0.01),
      sidebarMenu(
        menuItem("Vote", tabName = "vote",
                 startExpanded = T,
                 menuSubItem("CAQ", tabName = "caq_vote"),
                 menuSubItem("Tous les partis", tabName = "others_vote")
      ),
        menuItem("Attachement partisan", tabName = "attach",
                 startExpanded = T,
                 menuSubItem("CAQ", tabName = "caq_attach"),
                 menuSubItem("Tous les partis", tabName = "others_attach")
    )
    )),
    dashboardBody(
      clessnTheme,
      tabItems(
        tabItem("caq_vote",
                fluidRow(
                  column(2),
                  column(4, pickerInput("axisX_caq_vote", "Enjeu(x)\n",
                                        choices = choicesAxisXVote,
                                        selected = choicesAxisXVote[sample(1:length(choicesAxisXVote), 1)],
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE))),
                  column(4, pickerInput("cluster_caq_vote", "Segment(s)\n",
                                        choices = choicesGroupVote,
                                        selected = choicesGroupVote,
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE))),
                  column(2)
                ),
                fluidRow(
                  column(1),
                  column(10,
                         plotOutput("plot_caq_vote", width = "100%",
                                    height = "600px")),
                ),
        ),
        tabItem("others_vote",
                fluidRow(
                  column(4, pickerInput("axisX_others_vote", "Enjeu(x)\n",
                                            choices = choicesAxisXVote,
                                            selected = choicesAxisXVote[sample(1:length(choicesAxisXVote), 1)],
                                            multiple = T,
                                            options = pickerOptions(`actions-box` = TRUE,
                                                           `liveSearch` = TRUE))),
                  column(4, pickerInput("dependent_others_vote", "Parti(s)\n",
                                            choices = choicesDependentVote,
                                            selected = "CAQ",
                                            multiple = T,
                                            options = pickerOptions(`actions-box` = TRUE,
                                                           `liveSearch` = TRUE))),
                  column(4, pickerInput("cluster_others_vote", "Segment(s)\n",
                                           choices = choicesGroupVote,
                                           selected = choicesGroupVote[sample(1:length(choicesGroupVote), 1)],
                                           multiple = T,
                                           options = pickerOptions(`actions-box` = TRUE,
                                                          `liveSearch` = TRUE)))
                ),
                fluidRow(
                column(1),
                column(10,
                plotOutput("plot_others_vote", width = "100%",
                           height = "600px")),
                )
        ),
        tabItem("caq_attach",
                fluidRow(
                  column(2),
                  column(4, pickerInput("axisX_caq_attach", "Enjeu(x)\n",
                                        choices = choicesAxisXAttach,
                                        selected = choicesAxisXAttach[sample(1:length(choicesAxisXAttach), 1)],
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE))),
                  column(4, pickerInput("cluster_caq_attach", "Segment(s)\n",
                                        choices = choicesGroupAttach,
                                        selected = choicesGroupAttach,
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE))),
                  column(2)
                ),
                fluidRow(
                  column(1),
                  column(10,
                         plotOutput("plot_caq_attach", width = "100%",
                                    height = "600px")),
                ),
        ),
        tabItem("others_attach",
                fluidRow(
                  column(4, pickerInput("axisX_others_attach", "Enjeu(x)\n",
                                        choices = choicesAxisXAttach,
                                        selected = choicesAxisXAttach[sample(1:length(choicesAxisXAttach), 1)],
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE))),
                  column(4, pickerInput("dependent_others_attach", "Parti(s)\n",
                                        choices = choicesDependentAttach,
                                        selected = "CAQ",
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE))),
                  column(4, pickerInput("cluster_others_attach", "Segment(s)\n",
                                        choices = choicesGroupAttach,
                                        selected = choicesGroupAttach[sample(1:length(choicesGroupAttach), 1)],
                                        multiple = T,
                                        options = pickerOptions(`actions-box` = TRUE,
                                                                `liveSearch` = TRUE)))
                ),
                fluidRow(
                  column(1),
                  column(10,
                         plotOutput("plot_others_attach", width = "100%",
                                    height = "600px")),
                )
        )
      ),
      br(),
      fluidRow(
        column(2,
               numericInput("strip_text_size", "Taille des titres\n", value = 12.5)),
        column(2,
               numericInput("width", "Largeur de l'image (pixels)", value = 4500)),
        column(2,
               numericInput("height", "Hauteur de l'image (pixels)", value = 3000))
      ),
      shiny::textInput(inputId = "filename", label = "Nom du fichier"),
      shiny::downloadButton(outputId = "downloadPlot", label = "Télécharger le graphique")
      )
    )
  )
)

ui <- secure_app(ui)