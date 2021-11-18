#### BEGINNING OF THE DOCUMENT ####

#### 1. Loading packages and data ####

library(ggcorrplot)
library(dplyr)
library(stargazer)
library(ggplot2)
library(tidyverse)
library(descriptr)


Data1 <- read.csv("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.csv")
  select(215:225, 242:312, 344:378)
Data2 <- read.csv("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle2.csv")


test <- ds_screener(Data1)