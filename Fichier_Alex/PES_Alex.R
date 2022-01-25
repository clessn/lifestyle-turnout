# 0.1 - Packages ####
library(tidyverse)

# 0.2 - Data ####
Data <- readRDS("_SharedFolder_article-turnout-lifestyles/data/VPL-data-19_01_2019/lifestyle_coding/data/DataBaV_31-05-2020.rds")
Data$age_cat <- NA
Data$age_cat[Data$age_under25 == 1] <- 1
Data$age_cat[Data$age_25to45 == 1] <- 2
Data$age_cat[Data$age_45to60 == 1] <- 3
Data$age_cat[Data$age_over60 == 1] <- 4

