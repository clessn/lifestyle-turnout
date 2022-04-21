library(shiny)
library(shinydashboard)
library(shinymanager)
library(tidyverse)

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

#rsconnect::deployApp("C:/Users/huber/Dropbox/CLESSN/segmentation-synopsis/shiny-segm", account = "clessn-host")

server <- function(session, input, output) {
  output$plot <- renderPlot({
    Data %>% filter(pValAxisX <= input$seuil_sign) %>%
      filter(cate %in% input$group &
             axisX %in% input$axisX) %>%
      ggplot(aes(x = valAxisX, y = valDependent, group=cate)) +
      geom_line(aes(color = cate)) +
      geom_ribbon(aes(ymin = ciLow,
                      ymax = ciHigh, fill = cate), alpha = 0.2) +
      facet_wrap(~axisX) +
      ylab("Probabilité de voter en 2019") +
      xlab("") +
      coord_cartesian(ylim=c(0,1)) +
      theme(legend.position = "top",
            axis.text.x = element_blank())
  })
}
  