####################################################################################
########################## Clustering lifestyle-turnout ############################
####################################################################################

#********************************#
#### 0. PACKAGES  ####
#********************************#

library(tidyverse)
library(psych)
library(utils)
library(ggplot2)

#********************************#
#### 0.1 DATA ####
#********************************#  

Data  <- readRDS("_SharedFolder_article-turnout-lifestyles/data/VPL-data-19_01_2019/lifestyle_coding/data/DataBaV_31-05-2020.rds")

#********************************#
#### 0.2 FONCTIONS ####
#********************************#  

# 0.1 - custom functions ####

# Faire une FA sur une df. Save le graph de FA dans le path_save 
topdown_fa <- function(df, path_save, nfactors = 1) {
  # Cronbach's alpha (Test 1)
  cronbachAlpha <<- round(psych::alpha(df)$total$raw_alpha, 2)
  
  # Analyse factorielle (Test 2)
  
  factAnalysis <- factanal(df, factors=nfactors) # Analyse factorielle
  factorVarNames <- names(df)
  
  factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
  factor1stEigen <<- round(eigen(cor(df))$values[1], digit=2)
  
  
  plot <- ggplot(data.frame(factorVarNames,factorLoadings), 
                 aes(x=factorVarNames, y=factorLoadings)) + 
    coord_flip() +
    geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
    geom_text(aes(label=as.character(round(factorLoadings, 
                                           digits = 2))), vjust=0.35, hjust=-0.3, size = 8) +
    geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
    annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
             x=1.1, y=1.31, size=6.8) +
    annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.31, size=6.8) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="Coefficients de saturation", 
                       limits=c(0, 1.55), breaks=seq(0, 1, by=0.1)) +
    xlab("\n") + 
    theme_linedraw() +
    theme(axis.text.y = element_text(size=20), 
          axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 17),
          axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
          panel.grid=element_blank())
  
  ggsave(path_save,
         width = 24, height = 12)
  print(plot)
  print("What we want:")
  print(paste0("Alpha de Cronbach > 0.6 ->",cronbachAlpha))
  print(paste0("Première Valeur Propre > 1 ->",factor1stEigen))
  print(paste0("Coefficients de saturation > 0.3"))
}

# Pour cleaner: remplace un chiffre dans une colonne à NA 
fna <- function(df, col, int){
  df[[col]][df[[col]] == int] <- NA
  return(df[[col]])
}


# Quand la FA nous dit qu'il y a des colonnes qui scalent inversement au scale global, on peut appliquer cette fct 
# pour l'inverser
finverser <- function(df, col){
  vec_col <- df[[col]]
  unique_col <- unique(vec_col)
  unique_col <- unique_col[!is.na(unique_col)]
  n <- length(unique_col)
  max <- max(vec_col, na.rm = T)
  ord <- sort(as.vector(unique_col))
  rev <- rev(ord)
  for (i in 1:n){
    vec_col[vec_col == ord[i]] <- max + rev[i] 
  }
  vec_col <- vec_col - max
  return(vec_col)
}

#********************************#
#### 1 ANALYSES FACTORIELLES ####
#********************************#  

#### Pre cleaning ####

# life_satisfaction
Data$life_satisfaction_norm <- Data$life_satisfaction/10

Data$exercice_hike_rev <- finverser(Data, "exercice_hike")

#### Santé et bien-être ####

Sante_FA <- Data  %>%
  dplyr::select(c(health_phy,
                  health_mental,
                  happiness,
                  life_satisfaction_norm)) %>%
  na.omit()


topdown_fa(Sante_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/sante-bien-etreFA.png")

# Essayer avec des vars d'exercice

Sante_FA <- Data  %>%
  dplyr::select(c(health_phy,
                  health_mental,
                  happiness,
                  life_satisfaction_norm)) %>%
  na.omit()

topdown_fa(Sante_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/sante-bien-etreFA2.png")


#### Environnementaliste ####

#### Local-International ####

#### Sophistication cons ####

#### Substance use #### 

#### Vie sociale #### 




