#### DÉBUT DU DOCUMENT ####

library(tidyverse)

#### 1. Ouvrir la base de données ####

Data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")

agegrp18_34 <- subset(Data, ses_age24m == 1 | ses_age2534 == 1,  
                      select=c(1:404))
agegrp35_54 <- subset(Data, ses_age3554 == 1, 
                      select=c(1:404))
agegrp55p <- subset(Data, ses_age55p == 1, 
                    select=c(1:404))


#### 2. Insérer un modèle #### -> Peut-être tester pour chaque subset un même ensemble de variables?

model_agegrp18_34 <- glm(op_turnout2019 ~ 
                      ...+
                      ...+, 
                    data = agegrp18_34, family = binomial(link='logit'))
summary(model_agegrp18_34)


model_agegrp35_54 <- glm(op_turnout2019 ~ 
                           ...+
                           ...+, 
                         data = agegrp35_54, family = binomial(link='logit'))
summary(model_agegrp35_54)


model_agegrp55p <- glm(op_turnout2019 ~ 
                           ...+
                           ...+, 
                         data = agegrp55p, family = binomial(link='logit'))
summary(model_agegrp55p)


#### 3. Création d'un nouveau dataframe ####

TurnoutOddRatioData <- odds.ratio(model_agegrp18_34, level = 0.95) %>%
  tibble::rownames_to_column() %>%
  rename(varName = rowname, lowBound = "2.5 %", highBound = "97.5 %",
         odds = OR) %>%
  filter(varName != "(Intercept)") %>%
  mutate(varName = recode(varName, op_turnout2019 = "2019 turnout", # renommmer correctement les variables utilisées
                          )) 

TurnoutOddRatioData$rowId <- NA
TurnoutOddRatioData$rowId <- 1:23
# ValidationAustOdds <- exp(cbind(Odds=coef(RegAboriginalAusAllMerge), confint(RegAboriginalAusAllMerge))) # Test


#### 4. Graphique ####

ggplot(TurnoutOddRatioData, aes(x = odds, y = rowId)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = highBound, xmin = lowBound), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "red") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous("\nOdds ratio (log scale)", breaks = seq(0,7,1)) +
  coord_trans(x = "log10") +
  scale_y_continuous("", labels = c("...","...",), 
                     breaks = seq(1,23,1)) +
  theme(axis.title.x = element_text(size=18, face="bold", colour = "black"),
        axis.title.y = element_text(size=18, face="bold", colour = "black"),
        #  legend.key.size = unit(3.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, colour = "black", face="bold"),
        axis.text = element_text(size = 15, face = "bold", colour = "black"),
        legend.position = "bottom")
ggsave("_SharedFolder_article-turnout-lifestyles/graphs/odds_ratio_turnout.png", width = 12, height = 10)


#### FIN DU DOCUMENT ####