#Library
library(tidyverse)
library(stats)

explo <- read.csv("_SharedFolder_bav-2021/Data/Clean/27-10-2021-DatagotchiHub-2.csv") %>%
  drop_na(c(op_voteIntent_Lib, ses_dwelling_app, cons_Vegan))

explo$voteIntent <- NA
explo$voteIntent[explo$op_voteIntent_Lib == 1] <- 1
explo$voteIntent[explo$op_voteIntent_Cons == 1] <- 2
explo$voteIntent[explo$op_voteIntent_Ndp == 1] <- 3
explo$voteIntent[explo$op_voteIntent_Bloc == 1] <- 4
explo$voteIntent[explo$op_voteIntent_Green == 1] <- 5

write.csv("")

#explo$voteIntent <- factor(explo$voteIntent)

model <- lm(as.factor(voteIntent) ~ ses_dwelling_app + cons_Vegan, 
            data = explo) 


model_lib <- glm(op_voteIntent_Lib ~ ses_dwelling_app + cons_Vegan, 
             data = explo,  family = binomial(link = "logit"))

model_cons <- glm(op_voteIntent_Lib ~ ses_dwelling_app + cons_Vegan, 
                 data = explo,  family = binomial(link = "logit"))
