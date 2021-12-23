library(tidyverse)

Data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/PilotsFusion.rds")

model <- glm(op_turnout2019 ~ ses_dwelling_app + cons_Vegan, 
                 data = Data,  family = binomial(link = "logit"))


