library(tidyverse)

Data <- read.csv("_SharedFolder_article-turnout-lifestyles/data/CES_MERGED_1968-2019_2021-11-16.csv") %>%
  select(c(X.1, year, immigrant, contains("IdpreALL")))


