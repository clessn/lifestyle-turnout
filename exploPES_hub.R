# 0.1 - Packages ####
library(tidyverse)

# 0.2 - Data ####
Data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/VPL-data-19_01_2019/lifestyle_coding/data/DataBaV_31-05-2020.rds")
Data$age_cat <- NA
Data$age_cat[Data$age_under25 == 1] <- 1
Data$age_cat[Data$age_25to45 == 1] <- 2
Data$age_cat[Data$age_45to60 == 1] <- 3
Data$age_cat[Data$age_over60 == 1] <- 4


sum_nas <- function(df){
  #df <- Ses_CA
  cols <- names(df)
  ret <- data.frame(cols = cols,
                    nas = c(rep(0, length(cols))))
  vec_nas <- c()
  for (i in 1:length(cols)){
    col <- names(df)[i]
    xi <- sum(is.na(df[[col]])) 
    vec_nas[i] <- xi
  }
  ret$nas <- vec_nas
  ret <- ret %>%
    arrange(., -nas)
  print(head(ret, 10))
  return(ret)
}

varAge <- "age_cat"
var <- "drink_wine"

make_graph <- function(Data, varAge, var){
  Freq <- data.frame(table(Data[[varAge]], Data[[var]])) %>%
    setNames(c(varAge, var, "Freq"))
  
  Agg <- Data %>%
    group_by(.data[[varAge]], .data[[var]]) %>%
    summarise(vote2019 = sum(voted))
  
  Agg[[varAge]] <- as.factor(Agg[[varAge]])
  Agg[[var]] <- as.factor(Agg[[var]])
  
  ByAge <- Data %>%
    group_by(.data[[varAge]]) %>%
    summarise(sum = sum(voted),
              n = n(),
              percAgeCat = sum/n) %>%
    select(c(1, 4))
  
  ByAge[[varAge]] <- as.factor(ByAge[[varAge]])
  
  GraphData <- left_join(Agg, Freq, by = c(varAge, var)) %>%
    mutate(percVote2019 = vote2019/Freq) %>%
    left_join(., ByAge, by = varAge)
  
  levelsAge <- c(1, 2, 3, 4) 
  
  levels(GraphData[[varAge]]) <- levelsAge
  
  plot <- ggplot(GraphData, aes(x = .data[[var]], y = percVote2019)) +
    geom_bar(stat = "identity", aes(alpha = log(Freq)), fill = "dodgerblue4") +
    facet_wrap(~.data[[varAge]]) +
    geom_hline(aes(yintercept = percAgeCat,
                   linetype = "Proportion de la\ncatégorie d'âge qui a voté"), color = "indianred2", size = 1.5) +
    geom_text(aes(y = percVote2019 + 0.03,
                  label = paste0("Freq = ", Freq)),
              fontface = "bold",
              size = 1.5) +
    scale_alpha_continuous("Fréquence du\ngroupe (log)", range = c(0.1, 1)) +
    theme_bw() +
    ylab("Proportion du groupe qui a voté en 2019\n")
  
  return(plot)
}

make_graph(Data,
           "age_cat",
           "drink_wine")

ggsave("_SharedFolder_article-turnout-lifestyles/dataMining_results/exploPes/effet_age/test.png")

save_graphs <- function(Data, vars_age, vars,
                        path) {
  for (i in 1:length(vars_age)){
    varAge <- vars_age[i]
    for (i in 1:length(vars)){
      var <- vars[i]
      plot <- make_graph(Data, varAge, var)
      ggsave(filename = paste0(path, "/", varAge, "-", var, ".png"),
             plot = plot)
    }
  }
}


vars <- c("act_Books",
          "act_DoingTeamSport",
          "act_DoingYoga",
          "act_exercice",
          "act_holidays",
          "act_MotorizedOutdoorActivities",
          "act_occupField",
          "act_PerformingArts",
          "act_subject_reading",
          "act_transport",
          "act_Travel",
          "act_VideoGames",
          "act_Volunteering",
          "act_work",
          "act_Worship",
          "cons_brand",
          "cons_coffee_place",
          "cons_coffee_type",
          "cons_favorite_drink",
          "cons_Meat",
          "cons_regime",
          "cons_smoke_status",
          "cons_socmedia",
          "cons_Vegan",
          "cons_weed_freq2",
          "op_intent",
          "op_peoplePred_PCC",
          "op_peoplePred_PLC",
          "op_voteCertainty",
          "ses_dwelling",
          "ses_dwelling_app",
          "ses_educ",
          "ses_income",
          "ses_language",
          "ses_matrimonial_status",
          "ses_parentsBornCanada",
          "ses_relationship",
          "ses_sexualorientation",
          "val_excitementAll",
          "val_kidsConformity",
          "val_kidsFreethink",
          "val_securityAll",
          "val_selfRespectAll")



save_graphs(Data,
            c("ses_age_cat", "ses_age_cat_v2"),
            vars,
            "_SharedFolder_article-turnout-lifestyles/dataMining_results/Var-AgeCat")

