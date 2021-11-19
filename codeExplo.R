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

selectedData <- Data1[c(1, 14:24)] #réduire au plus possible, peut-être commencer par le turnout et SES?
selectedData <- na.omit(selectedData)

#### 2. Corrplot ####

corr_simple <- function(data=selectedData,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  
  #print table
  print(corr)
  
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  ggcorrplot(mtx_corr, lab = TRUE)
}
corr_simple()

