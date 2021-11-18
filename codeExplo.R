#### BEGINNING OF THE DOCUMENT ####

#### 1. Loading packages and data ####

library(ggcorrplot)
library(dplyr)
library(stargazer)
library(ggplot2)
library(tidyverse)
library(descriptr)


Data1 <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds") %>%
  select(1:2, 14:24 : 76:202, 215:348, 347:391) 
