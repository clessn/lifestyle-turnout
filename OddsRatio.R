#### BEGINNING OF THE DOCUMENT ####

library(tidyverse)
library(ggthemes)
library(stargazer) 
library(questionr)

#### Ouvrir la base de données ####

# Pilot
DataPilot <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")

## Insérer modèle #####

RegAll <- glm(op_turnout2019 ~ 
                cons_socmedia_Snap +
                cons_socmedia_Insta +
                #cons_socmedia_Facebook +
                act_transport_Car +
                act_transport_PublicTransportation +
                act_MotorizedOutdoorActivities +
                act_VideoGames +
                act_PerformingArts +
                cons_noDrink +
                cons_redWineDrink +
                cons_whiteWineDrink +
                cons_Smoke +
                app_swag_VintageHippBoheme +
                ses_age24m +
                ses_age2534 +
                ses_age3554 +
                ses_age55p, 
                    data = DataPilot, family = binomial(link='logit'))
summary(RegAll)

### Create dataframe ###
TurnoutOddRatioData <- odds.ratio(RegAll, level = 0.95) %>%
  tibble::rownames_to_column() %>%
  rename(varName = rowname, lowBound = "2.5 %", highBound = "97.5 %",
         odds = OR) %>%
  filter(varName != "(Intercept)") %>%
  mutate(varName = recode(varName, op_turnout2019 = "2019 turnout", 
                          cons_socmedia_Snap = "Using Snapchat most often",
                          cons_socmedia_Insta = "Using Instagram most often",
                          act_transport_Car = "Car",
                          act_transport_PublicTransportation = "Public transportation", 
                          act_MotorizedOutdoorActivities = "Motorized outdoor activities",
                          act_VideoGames = "Video games", 
                          act_PerformingArts = "Performing arts",
                          cons_noDrink = "Do not drink alcohol",
                          cons_redWineDrink = "Drinking red wine",
                          cons_whiteWineDrink = "Drinking white wine", 
                          cons_Smoke = "Smoking", 
                          app_swag_VintageHippBoheme = "Hippie", 
                          ses_age24m = "18 to 24",
                          ses_age2534 = "25 to 34",
                          ses_age3554 = "35 to 54",
                          ses_age55p = "55 and up")) 

TurnoutOddRatioData$rowId <- NA
TurnoutOddRatioData$rowId <- 1:nrow(TurnoutOddRatioData)

### Graph ###
ggplot(TurnoutOddRatioData, aes(x = odds, y = rowId)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = highBound, xmin = lowBound), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous("\nOdds ratio (log scale)", breaks = seq(0,7,1)) +
  coord_trans(x = "log10") +
  scale_y_continuous("", labels = TurnoutOddRatioData$varName, 
                     breaks = seq(1,nrow(TurnoutOddRatioData),1)) +
  theme(axis.title.x = element_text(size=18, face="bold", colour = "black"),
        axis.title.y = element_text(size=18, face="bold", colour = "black"),
        #  legend.key.size = unit(3.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, colour = "black", face="bold"),
        axis.text = element_text(size = 15, face = "bold", colour = "black"),
        legend.position = "bottom")

