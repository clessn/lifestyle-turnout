# Packages ####
library(tidyverse)

# Fonctions ####
regress <-
  function(data,
           y,
           x,
           reg_type,
           controls = as.character(),
           family = NA) {
    x_express <- paste0(x, collapse = " + ")
    vars_express <- paste0(y, " ~ ", x_express)
    controls_express <- paste0(controls, collapse = " + ")
    varsWcont_express <- if (length(controls) == 0) {
      vars_express
    }
    else {
      paste0(vars_express, " + ", controls_express)
    }
    express <-
      paste0(reg_type, "(", varsWcont_express, ", data = ", data)
    express <- if (is.na(family)) {
      paste0(express, ")")
    }
    else {
      paste0(express, ", family = ", family, ")")
    }
    model <- eval(parse(text = express))
    return(model)
  }

# Graphs descriptifs sur le subset des non-voters ####


# Binom tests ####
Data <- readRDS("shiny-turnout/ShinyData.rds") %>%
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

Agg <- Data %>%
  mutate(modelId = paste0(axisX, " ~ ", cate)) %>%
  group_by(modelId) %>%
  summarise(pval = unique(pValAxisX))

Agg2 <- Data %>%
  group_by(axisX) %>%
  summarise(pval = mean(pValAxisX))


Lifestyle_Ses <- Data %>%
  filter(pValAxisX <= 0.05 &
           (valAxisX == 0.1 |
              valAxisX == 1)) %>%
  select(c(axisX, cate, valDependent, valAxisX, pValAxisX)) %>%
  pivot_wider(.,
              names_from = valAxisX,
              names_prefix = "val_",
              values_from = valDependent) %>%
  mutate(effect_turnout = val_1 - val_0.1) %>%
  select(c(lifestyle = axisX, ses = cate, pValue = pValAxisX, lifestyle0 = val_0.1,
           lifestyle1 = val_1, effect_turnout))

LifestyleOnly <- Lifestyle_Ses %>%
  group_by(lifestyle) %>%
  summarise(mean_pValue = mean(pValue),
            mean_lifestyle0 = mean(lifestyle0),
            mean_lifestyle1 = mean(lifestyle1),
            mean_effect_turnout = mean(effect_turnout))

## Negative effects on turnout ####

# LifestyleOnly nous montre quelques variables avec un effet négatif fort
## médias sociaux, taxi, public transport, motorized outdoor activities, doing team sport,
## holidays relax, cons_vegan, cons_noDrink, brand_onlineOnly, videoGames, coffeeInstant

### Cleaning vars ####

### Social media ###

###### régression
modelSocMedia <- glm(op_turnout2019 ~ cons_socmedia_Other + cons_socmedia_Snap + cons_socmedia_Tiktok +
               cons_socmedia_Facebook + cons_socmedia_Insta + cons_socmedia_YT + cons_socmedia_Twitter,
             family = "binomial", data = DataPilot)

###### merger les médias sociaux avec un effet négatif significatif
DataPilot$socmedia_badTurnout <- NA
DataPilot$socmedia_badTurnout <- DataPilot$cons_socmedia_Insta + DataPilot$cons_socmedia_Snap +
  DataPilot$cons_socmedia_Tiktok + DataPilot$cons_socmedia_Other


### Transport ###

###### régression
modelTransport <- glm(op_turnout2019 ~ act_transport_Taxi + act_transport_PublicTransportation +
                        act_transport_Car + act_transport_SUV + act_transport_Moto + act_transport_Walk +
                        act_transport_Bicycle,
                     family = "binomial", data = DataPilot)

###### merger les transports avec un effet négatif significatif
DataPilot$transport_badTurnout <- NA
DataPilot$transport_badTurnout <- DataPilot$act_transport_Taxi + DataPilot$act_transport_PublicTransportation

### Others ###

table(DataPilot$act_MotorizedOutdoorActivities)
DataPilot$motorizedAct_badTurnout <- 0
DataPilot$motorizedAct_badTurnout[DataPilot$act_MotorizedOutdoorActivities > 0] <- 1 # tout le monde en haut de 0 est codé comme "en fait"
table(DataPilot$motorizedAct_badTurnout)

table(DataPilot$act_DoingTeamSport)
DataPilot$teamSport_badTurnout <- 0
DataPilot$teamSport_badTurnout[DataPilot$act_DoingTeamSport > 0] <- 1 # tout le monde en haut de 0 est codé comme "en fait"
table(DataPilot$teamSport_badTurnout)

table(DataPilot$act_holidays_Relax)
DataPilot$holidaysRelax_badTurnout <- DataPilot$act_holidays_Relax
table(DataPilot$holidaysRelax_badTurnout)

table(DataPilot$cons_Vegan)
DataPilot$vegan_badTurnout <- DataPilot$cons_Vegan
table(DataPilot$vegan_badTurnout)

table(DataPilot$cons_noDrink)
DataPilot$noDrink_badTurnout <- DataPilot$cons_noDrink
table(DataPilot$noDrink_badTurnout)

table(DataPilot$cons_brand_OnlineOnly)
DataPilot$brand_OnlineOnly_badTurnout <- DataPilot$cons_brand_OnlineOnly
table(DataPilot$brand_OnlineOnly_badTurnout)

# 1 = gamer, 0 = non-gamer. On met 1 à ceux qui ont dit souvent ou très souvent
table(DataPilot$act_VideoGames)
DataPilot$videoGames_badTurnout <- 0
DataPilot$videoGames_badTurnout[DataPilot$act_VideoGames > 0.5] <- 1
table(DataPilot$videoGames_badTurnout)

table(DataPilot$cons_coffee_type_Instant)
DataPilot$instantCoffee_badTurnout <- DataPilot$cons_coffee_type_Instant
table(DataPilot$instantCoffee_badTurnout)

vars_to_test <- names(DataPilot %>% select(contains("badTurnout")))

Results <- data.frame(var = as.character(),
                      varVoted = as.numeric(),
                      varTotal = as.numeric(),
                      varPropVoted = as.numeric(),
                      varEffect = as.numeric(),
                      pValue = as.numeric()
)

for (i in 1:length(vars_to_test)){
  #i <- 1
  #var <- "videoGames_badTurnout"
  var <- vars_to_test[i]
  Desc <- as.data.frame(DataPilot %>%
                          group_by(.data[[var]]) %>%
                          summarise(voted2019 = sum(op_turnout2019),
                                    n = as.numeric(n())) %>%
                          mutate(prop = voted2019/n))
  
  test <- binom.test(x = Desc[2, 2],
                     n = Desc[2, 3],
                     p = sum(DataPilot$op_turnout2019)/nrow(DataPilot),
                     alternative = "less",
                     conf.level = 0.95)
  
  eff <- test$estimate - test$null.value
  
  Results[i, 1] <- var
  Results[i, 2] <- test$statistic
  Results[i, 3] <- test$parameter
  Results[i, 4] <- test$estimate
  Results[i, 5] <- eff
  Results[i, 6] <- round(test$p.value, digits = 5)
}

modelBadTurnout <- regress(data = "DataPilot",
                           y = "op_turnout2019",
                           x = vars_to_test,
                           reg_type = "glm",
                           family = "binomial"
                           )


modelBadTurnoutControls <- regress(data = "DataPilot",
                           y = "op_turnout2019",
                           x = vars_to_test[!(vars_to_test %in% c("teamSport_badTurnout", "holidaysRelax_badTurnout",
                                                                  "videoGames_badTurnout"))],
                           controls = c("educBHS", "incomeLow", "ses_dwelling_app", "ses_age24m"),
                           reg_type = "glm",
                           family = "binomial"
)

modelBadTurnoutControls <- regress(data = "DataPilot",
                                   y = "op_turnout2019",
                                   x = vars_to_test[!(vars_to_test %in% c("teamSport_badTurnout", "holidaysRelax_badTurnout",
                                                                          "videoGames_badTurnout", "vegan_badTurnout"))],
                                   controls = c("incomeLow", "ses_age24m"),
                                   reg_type = "glm",
                                   family = "binomial"
)


# predict

PredictDf <- DataPilot %>%
  select(all.vars(modelBadTurnoutControls$formula))

PredictDf$pred <- predict.glm(modelBadTurnoutControls, newdata = PredictDf, type = "response")

hist(PredictDf$pred)

ggplot(PredictDf, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~op_turnout2019)

Agg <- PredictDf %>%
  mutate(nVI = rowSums(.[2:7])) %>%
  select(c(op_turnout2019, nVI))# %>%
  group_by(nVI) %>%
  summarise(voted = )

ggplot(Agg, aes(x = nVI)) +
  geom_histogram() +
  facet_wrap(~op_turnout2019)

# William: c'est dla marde
# Je dois faire un modèle avec toutes nos vars d'intérêt (même celles avec un effet positif)
# Garder même celles qui ont pas d'effet significatif (wtf???)
# Mettre plus de controls (educ)
# ce qu'on fait dans shiny (en chiffres pour interpréter, en graphs pour la publication)
# faire des interactions
# Faire des loops pour des gens qui ont des combinaisons de vars

# Odds ratio ####

# Tableaux de régression ####



