library(tidyverse)

Data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")

make_graph <- function(Data, varAge, var){
  Freq <- data.frame(table(Data[[varAge]], Data[[var]])) %>%
    setNames(c(varAge, var, "Freq"))
  
  Agg <- Data %>%
    group_by(.data[[varAge]], .data[[var]]) %>%
    summarise(vote2019 = sum(op_turnout2019))
  
  Agg[[varAge]] <- as.factor(Agg[[varAge]])
  Agg[[var]] <- as.factor(Agg[[var]])
  
  ByAge <- Data %>%
    group_by(.data[[varAge]]) %>%
    summarise(sum = sum(op_turnout2019),
              n = n(),
              percAgeCat = sum/n) %>%
    select(c(1, 4))
  
  ByAge[[varAge]] <- as.factor(ByAge[[varAge]])
  
  GraphData <- left_join(Agg, Freq, by = c(varAge, var)) %>%
    mutate(percVote2019 = vote2019/Freq) %>%
    left_join(., ByAge, by = varAge)
  
  levelsAge <- if (varAge == "ses_age_cat_v2") {
    c("18-24", "25-34", "35-54", "55+")
  } else {
    c("18-34", "35-54", "55+") 
  }
  
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
           "ses_age_cat_v2",
           "ses_dwelling_app")

ggsave("_SharedFolder_article-turnout-lifestyles/dataMining_results/test.png")

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
