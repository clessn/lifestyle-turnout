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

Data$favorite_transport <- NA
Data$favorite_transport[Data$transport_walk == 1 | Data$transport_bike == 1] <- 1
Data$favorite_transport[Data$transport_public == 1] <- 0.5
Data$favorite_transport[Data$transport_car == 1 | Data$transport_moto == 1 | Data$transport_taxi == 1] <- 0

Data$regime <- NA
Data$regime[Data$food_vegan == 1] <- 1 
Data$regime[Data$food_vegetarian == 1] <- 0.67
Data$regime[Data$food_semivegetarian == 1] <- 0.33
Data$regime[Data$food_normal == 1] <- 0


Data$transport_walk_scale_norm <- Data$transport_walk_scale/4 
Data$transport_bike_scale_norm <- Data$transport_bike_scale/4 
Data$transport_public_scale_norm <- Data$transport_public_scale/4
Data$transport_moto_scale_norm <- Data$transport_moto_scale/4
Data$transport_car_scale_norm <- Data$transport_car_scale/4 
Data$transport_taxi_scale_norm <- Data$transport_taxi_scale/4 


Enviro_FA <- Data  %>%
  dplyr::select(c(favorite_transport, 
                  regime, 
                  transport_walk_scale_norm, 
                  transport_bike_scale_norm, 
                  transport_public_scale_norm,
                  transport_moto_scale_norm, 
                  transport_car_scale_norm, 
                  transport_taxi_scale_norm)) %>%
  na.omit()

topdown_fa(Enviro_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/enviroFA.png")

Data$transport_moto_scale_norm_rev <- finverser(Data, "transport_moto_scale_norm")
Data$transport_walk_scale_norm_rev <- finverser(Data, "transport_walk_scale_norm")
Data$food_normal_rev <- finverser(Data, "food_normal")
Data$transport_car_scale_norm_rev <- finverser(Data, "transport_car_scale_norm")
Data$transport_car_rev <- finverser(Data, "transport_car")




Data$mix_veg <-  Data$food_vegan + Data$food_vegetarian 


Enviro_FA <- Data  %>%
  dplyr::select(c(favorite_transport, 
                  regime, 
                  transport_walk_scale_norm_rev, 
                  transport_bike_scale_norm, 
                  transport_public_scale_norm,
                  transport_moto_scale_norm_rev, 
                  transport_car_scale_norm, 
                  transport_taxi_scale_norm)) %>%
  na.omit()

topdown_fa(Enviro_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/enviroFA2.png")

Enviro_FA <- Data  %>%
  dplyr::select(c(favorite_transport, 
                  regime, 
                  transport_bike_scale_norm, 
                  transport_public_scale_norm,
                  transport_taxi_scale_norm)) %>%
  na.omit()

topdown_fa(Enviro_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/enviroFA3.png")


Enviro_FA <- Data  %>%
  dplyr::select(c(favorite_transport, 
                  transport_public_scale_norm,
                  transport_car_rev)) %>%
  na.omit()

topdown_fa(Enviro_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/enviroFA4.png")

### Au lieu de faire une dimension enviro, on va faire une dimension transport


#### Local-International ####

Data$coffeelocint <- NA
Data$coffeelocint[Data$coffee_shop_independent == 1] <- 1
Data$coffeelocint[Data$coffee_shop_timHortons == 1] <- 0.5
Data$coffeelocint[Data$coffee_shop_mcdo == 1 |Data$coffee_shop_starbucks == 1] <- 0



Locint_FA <- Data  %>%
  dplyr::select(c(coffeelocint, 
                  food_normal_rev, 
                  transport_car_scale_norm)) %>%
  na.omit()

topdown_fa(Locint_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/LocintFA.png")


#### Test_Hipster ####

Data$smoker_cannabis_scale_norm <- Data$smoker_cannabis_scale/4 
Data$smoker_cig_scale_norm <- Data$smoker_cig_scale/4


Data$smoker_cig_scale_norm_rev <- finverser(Data, "smoker_cig_scale_norm")


Hips_FA <- Data  %>%
  dplyr::select(c(coffee_shop_independent, 
                  food_normal_rev, 
                  transport_public_scale_norm,
                  smoker_cig_scale_norm_rev
                  )) %>%
  na.omit()

topdown_fa(Hips_FA, "_SharedFolder_article-turnout-lifestyles/graphs/factorAnalysis/HypsFA.png")




#### Sophistication cons ####
Data$smoker_cannabis_scale_norm <- Data$smoker_cannabis_scale/4 
Data$smoker_cig_scale_norm <- Data$smoker_cig_scale/
Data$drink_wine_scale_norm <- Data$drink_wine_scale/
Data$drink_beer_scale_norm <- Data$drink_beer_scale/
Data$drink_beer_spirit_norm <- Data$drink_spirit_scale/


#### Substance use #### 

#### Vie sociale #### 




