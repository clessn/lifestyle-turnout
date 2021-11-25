library(tidyverse)

DataRaw <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")

DataRaw$ses_age_cat_v2 <- NA
DataRaw$ses_age_cat_v2[DataRaw$ses_age >= 18 & DataRaw$ses_age < 25] <- 1 #18-24
DataRaw$ses_age_cat_v2[DataRaw$ses_age >= 25 & DataRaw$ses_age < 35] <- 2 #25-34
DataRaw$ses_age_cat_v2[DataRaw$ses_age >= 35 & DataRaw$ses_age < 55] <- 3 #35-54
DataRaw$ses_age_cat_v2[DataRaw$ses_age >= 55] <- 4 #55 ans et plus

DataRaw$ses_age24m <- 0
DataRaw$ses_age24m[DataRaw$ses_age >= 18 & DataRaw$ses_age < 25] <- 1

DataRaw$ses_age2534 <- 0
DataRaw$ses_age2534[DataRaw$ses_age >= 25 & DataRaw$ses_age < 35] <- 1

DataRaw$ses_age3554 <- 0
DataRaw$ses_age3554[DataRaw$ses_age >= 35 & DataRaw$ses_age < 55] <- 1

DataRaw$ses_age55p <- 0
DataRaw$ses_age55p[DataRaw$ses_age >= 55] <- 1


saveRDS(DataRaw, "_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")
