library(shiny)
library(shinyWidgets)
library(tidyverse)
library(utils)

server <- function(session, input, output){
  Data <- reactive({
    req(input$dataFile, file.exists(input$dataFile$datapath))
    readRDS(input$dataFile$datapath)
  })
  observe({
    updatePickerInput(session = session,
                      "select_vars", "Sélectionner les variables à explorer",
                      choices = names(Data()))
  })
  observe({
    updatePickerInput(session = session,
                      "select_vars_fill", "Sélectionner les variables à explorer (fill)",
                      choices = names(Data()))
  })
  output$wd <- renderText(getwd())
  # corrs ####
  observeEvent(input$dl_corrs, {
    vars <- input$select_vars
    comb <- combn(vars, 2, simplify = F)
    for (i in 1:length(comb)){
      varx <- comb[[i]][1]
      vary <- comb[[i]][2]
      plot <- ggplot(Data(), aes_string(x = varx, y = vary)) +
                geom_point() +
                geom_smooth()
      ggsave(paste0(getwd(), input$path, "/", varx, "-", vary, ".png"),
             plot = plot)
    }
  }, ignoreInit = TRUE)
  # histo ####
  observeEvent(input$dl_histo, {
    vars <- input$select_vars
    for (i in 1:length(vars)){
      var <- vars[i]
      plot <- ggplot(Data(), aes_string(x = var)) +
        geom_histogram()
      ggsave(paste0(getwd(), input$path, "/", var, ".png"),
             plot = plot)
    }
  }, ignoreInit = TRUE)
  # densite ####
  observeEvent(input$dl_densite, {
    vars <- input$select_vars
    for (i in 1:length(vars)){
      var <- vars[i]
      plot <- ggplot(Data(), aes_string(x = var)) +
        geom_density()
      ggsave(paste0(getwd(), input$path, "/", var, ".png"),
             plot = plot)
    }
  }, ignoreInit = TRUE)
  # bars ####
  observeEvent(input$dl_bars, {
    vars <- input$select_vars
    comb <- combn(vars, 2, simplify = F)
    for (i in 1:length(comb)){
      varx <- comb[[i]][1]
      vary <- comb[[i]][2]
      plot <- ggplot(Data(), aes(x = factor(.data[[varx]]))) +
        geom_bar(stat="count", aes(fill = factor(.data[[vary]]),
                                          group = factor(.data[[vary]])),
                 position = position_dodge())
      ggsave(paste0(input$path, "/", varx, "-", vary, ".png"),
             plot = plot)
    }
  }, ignoreInit = TRUE)
}
