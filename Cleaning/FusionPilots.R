library(tidyverse)

nos_vars <- c("act_Books",
              "act_DoingTeamSport",
              "act_DoingYoga",
              "act_MotorizedOutdoorActivities",
              "act_PerformingArts",
              "act_Worship",
              "act_Travel",
              "act_VideoGames",
              "act_Volunteering",
              "cons_Meat",
              "cons_Vegan",
              "cons_Vege",
              "ses_dwelling_app",
              "langFr",
              "langEn",
              "ses_languageOther",
              "ses_hetero",
              "ses_bisex",
              "ses_gai",
              "ses_sexOri_other",
              "val_excitementAll",
              "val_kidsConformity",
              "val_securityAll",
              "val_selfRespectAll",
              "op_intent")

Pilot1 <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds") #%>%
  mutate(cons_coffee_type_NoCoffee = cons_coffee_place_noCoffee) %>%
  select(c(nos_vars,
           starts_with("cons_brand"),
           starts_with("cons_coffee"),
           starts_with("act_transport"),
           starts_with("cons_Smoke"),
           starts_with("age"),
           starts_with("ses_educ"),
           starts_with("ses_income"),
           ends_with("Drink")))

Pilot2 <- read.csv("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle2.csv") %>%
  mutate(act_DoingTeamSport = act_TeamSport,
         act_DoingYoga = act_Yoga,
         cons_microDrink = cons_microBeers,
         cons_beerDrink = cons_regBeers,
         cons_bubbleDrink = cons_sparklingDrink) %>%
  select(c(nos_vars,
           starts_with("cons_brand"),
           starts_with("cons_coffee"),
           starts_with("act_transport"),
           starts_with("cons_Smoke"),
           starts_with("age"),
           starts_with("ses_educ"),
           starts_with("ses_income"),
           ends_with("Drink")))

pilot1 <- names(Pilot1)
pilot2 <- names(Pilot2)
to_remove <- pilot1[!(pilot1 %in% pilot2)]
Pilot1 <- Pilot1 %>% select(-c(all_of(to_remove)))
cols_order <- names(Pilot1)
Pilot2 <- Pilot2[, cols_order]
Data <- rbind(Pilot1, Pilot2)

saveRDS(Data, "_SharedFolder_article-turnout-lifestyles/data/PilotsFusion.rds")  
  
  