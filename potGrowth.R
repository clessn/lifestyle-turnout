library(tidyverse)

data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds") %>%
  select(c(CASEID, starts_with("op_potentialG"))) %>%
  rename_all(~stringr::str_replace(.,"^op_potentialG_",""))


make_graph <- function(parti1, parti2){
  ns <- data %>%
    group_by(.data[[parti1]], .data[[parti2]]) %>%
    summarise(n = n())
  
  GraphData <- data %>%
    select(c(.data[[parti1]], .data[[parti2]])) %>%
    left_join(., ns, by = c(.data[[parti1]], .data[[parti2]]))
  
  plot <- ggplot(GraphData, aes(x = .data[[parti1]], y = .data[[parti2]])) +
    geom_point(aes(color = n), shape = 15, size = 10) +
    geom_smooth() +
    scale_color_gradient2(low = "#EFEFEF",
                          mid = "#ED9689",
                          high = "#EA1B00",
                          limits = c(0, 500))
  return(plot)
}

make_graph("QS", "CAQ")


data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")

data$ses_age_cat[data$ses_age_cat == 1] <- "18-34"
data$ses_age_cat[data$ses_age_cat == 2] <- "35-54"
data$ses_age_cat[data$ses_age_cat == 3] <- "55+"
data$op_turnout2019[data$op_turnout2019 == 0] <- "N'a pas voté"
data$op_turnout2019[data$op_turnout2019 == 1] <- "A voté"


Agg <- data.frame(table(data$ses_age_cat, data$op_turnout2019))
Sums <- Agg %>%
    group_by(Var1) %>%
    summarise(sum = sum(Freq))

GraphData <- Agg %>%
    left_join(., Sums, by = "Var1") %>%
    mutate(perc = Freq/sum)


ggplot(GraphData, aes(x = factor(Var1),
                          y = perc)) +
    geom_bar(stat="identity", aes(fill = factor(Var2),
                                  group = factor(Var2)),
             position = position_dodge()) +
  xlab("Groupe d'âge") +
  ylab("Proportion du groupe d'âge\n") +
  theme_bw() +
  guides(fill=guide_legend(title="Vote en 2019"))

ggsave("_SharedFolder_article-turnout-lifestyles/dataMining_results/age-vote.png")

