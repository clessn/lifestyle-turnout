library(shiny)
library(shinydashboard)
library(shinymanager)
library(tidyverse)

source(paste0(getwd(), "/data.R"), encoding = "UTF-8")
source(paste0(getwd(), "/clessnTheme.R"))


#rsconnect::deployApp("C:/Users/huber/Dropbox/CLESSN/segmentation-synopsis/shiny-segm", account = "clessn-host")



server <- function(session, input, output) {
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  # Vote - CAQ ####
  output$plot_caq_vote <- renderPlot({
    colorsClusters <- c("Oiseau en cage" = "#5f6e7d",
                        "iel offensé" = "#0078FF",
                        "Canadian club" = "#40bfb4",
                        "Jeune déclassé" = "#fdb827",
                        "Boomer nostalgique" = "#ff6b6b",
                        "Réformiste tranquille" = "#9b45bc")
    GraphData <- DataVote %>%
      filter(pValAxisX <= input$seuil_sign &
             cleanDependent == "CAQ" &
             cleanAxisX %in% input$axisX_caq_vote &
             cleanGroup %in% input$cluster_caq_vote) %>%
      mutate(cleanValDependent = valDependent*100,
             se = se*100,
             ciLow = ciLow*100,
             ciHigh = ciHigh*100)
    validate(
      need(nrow(GraphData) > 0, paste0("Aucun enjeu sélectionné n'est significatif (au seuil de ", input$seuil_sign, ") pour les combinaisons parti-segment sélectionnées."))
    )
    plot <- ggplot(GraphData, aes(x = valAxisX, y = cleanValDependent,
                                    group = cleanGroup)) +
        geom_line(aes(color = cleanGroup),
                  size = 1.5) +
        geom_ribbon(aes(ymin = ciLow,
                        ymax = ciHigh,
                        fill = cleanGroup), alpha = 0.15) +
        facet_wrap(~cleanAxisX, ncol = 4) +
        scale_x_continuous(breaks = c(0.15, 0.85), labels = c("-", "+")) +
        coord_cartesian(ylim=c(0,100)) +
        scale_fill_manual(values = colorsClusters) +
        scale_color_manual(values = colorsClusters) +
        ylab("\nProb. voter CAQ (%)\n") +
        xlab("\n") +
        theme_bw() +
        theme(legend.position = "top",
              legend.title=element_blank(),
              strip.background = element_rect(fill = "#CCCCCC", colour = "#CCCCCC"),
              strip.text.x = element_text(colour = "#181818", face = "bold", size = input$strip_text_size),
              strip.text.y = element_text(colour = "#181818", face = "bold", size = 12.5),
              panel.border = element_rect(colour = "#CCCCCC"),
              axis.text.x = element_text(size = 30, colour = "#181818"),
              axis.text.y = element_text(size = 11, colour = "#181818",
                                         margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
              axis.title.y = element_text(size = 17.5),
              legend.text=element_text(size=12)) +
        guides(color = guide_legend(nrow = 1),
               fill = guide_legend(nrow = 1))
    plot
  })
  
  # Vote - Tous les partis ####
  output$plot_others_vote <- renderPlot({
    colorsClusters <- c("Oiseau en cage" = "#5f6e7d",
                        "iel offensé" = "#0078FF",
                        "Canadian club" = "#40bfb4",
                        "Jeune déclassé" = "#fdb827",
                        "Boomer nostalgique" = "#ff6b6b",
                        "Réformiste tranquille" = "#9b45bc")
    GraphData <- DataVote %>%
      filter(pValAxisX <= input$seuil_sign &
             cleanDependent %in% input$dependent_others_vote &
             cleanAxisX %in% input$axisX_others_vote &
             cleanGroup %in% input$cluster_others_vote
             ) %>%
      mutate(cleanValDependent = valDependent*100,
             se = se*100,
             ciLow = ciLow*100,
             ciHigh = ciHigh*100)
    validate(
      need(nrow(GraphData) > 0, paste0("Aucun enjeu sélectionné n'est significatif (au seuil de ", input$seuil_sign, ") pour les combinaisons parti-segment sélectionnées."))
    )
    plot <- ggplot(GraphData, aes(x = valAxisX, y = cleanValDependent,
                                       group = cleanGroup)) +
                      geom_line(aes(color = cleanGroup),
                                size = 1.5) +
                      geom_ribbon(aes(ymin = ciLow,
                                      ymax = ciHigh,
                                      fill = cleanGroup), alpha = 0.15) +
                      facet_grid(cleanDependent~cleanAxisX, switch = "y") +  
                      scale_x_continuous(breaks = c(0.15, 0.85), labels = c("-", "+")) +
                      coord_cartesian(ylim=c(0,100)) +
                      scale_fill_manual(values = colorsClusters) +
                      scale_color_manual(values = colorsClusters) +
                      ylab("Probabilité de voter (%)") +
                      xlab("") +
                      theme_bw() +
                      theme(legend.position = "top",
                            legend.title=element_blank(),
                            strip.background = element_rect(fill = "#CCCCCC", colour = "#CCCCCC"),
                            strip.text.x = element_text(colour = "#181818", face = "bold", size = input$strip_text_size),
                            strip.text.y = element_text(colour = "#181818", face = "bold", size = 12.5),
                            panel.border = element_rect(colour = "#CCCCCC"),
                            axis.text.x = element_text(size = 30, colour = "#181818"),
                            axis.text.y = element_text(size = 11, colour = "#181818",
                                                       margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
                            axis.title.y = element_text(size = 17.5),
                            legend.text=element_text(size=12)) +
                      guides(color = guide_legend(nrow = 1),
                             fill = guide_legend(nrow = 1))
    plot
  })
  # Attach - CAQ ####
  output$plot_caq_attach <- renderPlot({
    colorsClusters <- c("Oiseau en cage" = "#5f6e7d",
                        "iel offensé" = "#0078FF",
                        "Canadian club" = "#40bfb4",
                        "Jeune déclassé" = "#fdb827",
                        "Boomer nostalgique" = "#ff6b6b",
                        "Réformiste tranquille" = "#9b45bc")
    GraphData <- DataAttach %>%
      filter(pValAxisX <= input$seuil_sign &
             cleanDependent == "CAQ" &
             cleanAxisX %in% input$axisX_caq_attach &
             cleanGroup %in% input$cluster_caq_attach) %>%
      mutate(cleanValDependent = valDependent*100,
             se = se*100,
             ciLow = ciLow*100,
             ciHigh = ciHigh*100)
    validate(
      need(nrow(GraphData) > 0, paste0("Aucun enjeu sélectionné n'est significatif (au seuil de ", input$seuil_sign, ") pour les combinaisons parti-segment sélectionnées."))
    )
    plot <- ggplot(GraphData, aes(x = valAxisX, y = cleanValDependent,
                                  group = cleanGroup)) +
      geom_line(aes(color = cleanGroup),
                size = 1.5) +
      geom_ribbon(aes(ymin = ciLow,
                      ymax = ciHigh,
                      fill = cleanGroup), alpha = 0.15) +
      facet_wrap(~cleanAxisX, ncol = 4) +
      scale_x_continuous(breaks = c(0.15, 0.85), labels = c("-", "+")) +
      coord_cartesian(ylim=c(0,100)) +
      scale_fill_manual(values = colorsClusters) +
      scale_color_manual(values = colorsClusters) +
      ylab("\nAttachement à la CAQ\n") +
      xlab("\n") +
      theme_bw() +
      theme(legend.position = "top",
            legend.title=element_blank(),
            strip.background = element_rect(fill = "#CCCCCC", colour = "#CCCCCC"),
            strip.text.x = element_text(colour = "#181818", face = "bold", size = input$strip_text_size),
            strip.text.y = element_text(colour = "#181818", face = "bold", size = 12.5),
            panel.border = element_rect(colour = "#CCCCCC"),
            axis.text.x = element_text(size = 30, colour = "#181818"),
            axis.text.y = element_text(size = 11, colour = "#181818",
                                       margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
            axis.title.y = element_text(size = 17.5),
            legend.text=element_text(size=12)) +
      guides(color = guide_legend(nrow = 1),
             fill = guide_legend(nrow = 1))
    plot
  })
  # Attach - Tous les partis ####
  output$plot_others_attach <- renderPlot({
    colorsClusters <- c("Oiseau en cage" = "#5f6e7d",
                        "iel offensé" = "#0078FF",
                        "Canadian club" = "#40bfb4",
                        "Jeune déclassé" = "#fdb827",
                        "Boomer nostalgique" = "#ff6b6b",
                        "Réformiste tranquille" = "#9b45bc")
    GraphData <- DataAttach %>%
      filter(pValAxisX <= input$seuil_sign &
             cleanDependent %in% input$dependent_others_attach &
             cleanAxisX %in% input$axisX_others_attach &
             cleanGroup %in% input$cluster_others_attach) %>%
      mutate(cleanValDependent = valDependent*100,
             se = se*100,
             ciLow = ciLow*100,
             ciHigh = ciHigh*100)
    validate(
      need(nrow(GraphData) > 0, paste0("Aucun enjeu sélectionné n'est significatif (au seuil de ", input$seuil_sign, ") pour les combinaisons parti-segment sélectionnées."))
    )
    plot <- ggplot(GraphData, aes(x = valAxisX, y = cleanValDependent,
                                  group = cleanGroup)) +
      geom_line(aes(color = cleanGroup),
                size = 1.5) +
      geom_ribbon(aes(ymin = ciLow,
                      ymax = ciHigh,
                      fill = cleanGroup), alpha = 0.15) +
      facet_grid(cleanDependent~cleanAxisX, switch = "y") +  
      scale_x_continuous(breaks = c(0.15, 0.85), labels = c("-", "+")) +
      coord_cartesian(ylim=c(0,100)) +
      scale_fill_manual(values = colorsClusters) +
      scale_color_manual(values = colorsClusters) +
      ylab("Attachement partisan") +
      xlab("") +
      theme_bw() +
      theme(legend.position = "top",
            legend.title=element_blank(),
            strip.background = element_rect(fill = "#CCCCCC", colour = "#CCCCCC"),
            strip.text.x = element_text(colour = "#181818", face = "bold", size = input$strip_text_size),
            strip.text.y = element_text(colour = "#181818", face = "bold", size = 12.5),
            panel.border = element_rect(colour = "#CCCCCC"),
            axis.text.x = element_text(size = 30, colour = "#181818"),
            axis.text.y = element_text(size = 11, colour = "#181818",
                                       margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
            axis.title.y = element_text(size = 17.5),
            legend.text=element_text(size=12)) +
      guides(color = guide_legend(nrow = 1),
             fill = guide_legend(nrow = 1))
    plot
  })
  
  # Download ####
  output$downloadPlot <- shiny::downloadHandler(
    filename = function(){paste0(input$filename, ".png")},
    content = function(file){
      ggsave(file, device = function(..., width, height){
        grDevices::png(..., width = input$width, height = input$height, res = 300, units = "px")})
    }
  )
}
