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

# PES
Data  <- readRDS("_SharedFolder_article-turnout-lifestyles/data/VPL-data-19_01_2019/lifestyle_coding/data/DataBaV_31-05-2020.rds")

# Pilot
DataPilot <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")

# Datagotchi
Datagotchi <- read.csv("_SharedFolder_bav-2021/Data/Clean/27-10-2021-DatagotchiHub-2.csv")

#********************************#
#### 0.2 FONCTIONS ####
#********************************#  

# 0.1 - custom functions ####

# Faire une FA sur une df. Save le graph de FA dans le path_save 
topdown_fa <- function(df, nom_fichier, nfactors = 1) {
  # Cronbach's alpha (Test 1)
  cronbachAlpha <<- round(psych::alpha(df)$total$raw_alpha, 2)
  
  # Analyse factorielle (Test 2)
  
  factAnalysis <- factanal(df, factors=nfactors) # Analyse factorielle
  factorVarNames <- names(df)
  
  factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
  factor1stEigen <<- round(eigen(cor(df))$values[1], digit=2)
  
  
  FAplot <- ggplot(data.frame(factorVarNames,factorLoadings), 
                   aes(x=factorVarNames, y=factorLoadings)) + 
    coord_flip() +
    geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
    geom_text(aes(label=as.character(round(factorLoadings, 
                                           digits = 2))), vjust=0.35, hjust=-0.3, size = 5) +
    geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
    annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
             x=1.1, y=1.28, size=5) +
    annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.28, size=5) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="\nCoefficients de saturation\n", 
                       limits=c(0, 1.55), breaks=seq(0, 1, by=0.1),
                       expand = c(0,0)) +
    xlab("\n") + 
    theme_linedraw() +
    theme(axis.text.y = element_text(size=15,
                                     margin = margin(r = 5, l = 3)), 
          axis.title.y = element_text(size = 15), 
          axis.text.x = element_text(size = 15),
          axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
          panel.grid=element_blank())
  
  #ggsave(FAplot, paste0("_SharedFolder_segmentation-synopsis/protectFrench/presentation/", nom_fichier, ".png"),
  #       width = 24, height = 12, device = "png")
  print(FAplot)
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

sum_nas <- function(df){
  #df <- Ses_CA
  cols <- names(df)
  ret <- data.frame(cols = cols,
                    nas = c(rep(0, length(cols))))
  vec_nas <- c()
  for (i in 1:length(cols)){
    col <- names(df)[i]
    xi <- sum(is.na(df[[col]])) 
    vec_nas[i] <- xi
  }
  ret$nas <- vec_nas
  ret <- ret %>%
    arrange(., -nas)
  print(head(ret, 10))
  return(ret)
}

cor_df <- function(data, y, columns){
  vec <- c()
  for (i in 1:length(columns)){
    column <- columns[i]
    cor <- cor(data[[y]], data[[column]])
    vec <- append(vec, cor)
  }
  df <- data.frame(column = columns,
                   corr   = vec)
  df$direction <- NA
  df$direction[df$corr > 0] <- 1
  df$direction[df$corr < 0] <- -1
  df$corr <- abs(df$corr)
  return(df)
}

#********************************#
# 1 PES ANALYSES FACTORIELLES ####
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

Data$smoker_cannabis_scale_norm <- Data$smoker_cannabis_scale/8
Data$smoker_cig_scale_norm <- Data$smoker_cig_scale/8


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
Data$smoker_cannabis_scale_norm <- Data$smoker_cannabis_scale/8
Data$smoker_cig_scale_norm <- Data$smoker_cig_scale/8
Data$drink_wine_scale_norm <- Data$drink_wine_scale/8
Data$drink_beer_scale_norm <- Data$drink_beer_scale/8
Data$drink_beer_spirit_norm <- Data$drink_spirit_scale/8


#### Substance use #### 

#### Vie sociale #### 



#********************************#
# 2 DATAGOTCHI ####
#********************************#  

# On veut garder les vars qui sont dans le Pilot1

Datagotchi <- Datagotchi %>%
  select(-c(
    starts_with("film"), starts_with("vehicule"), starts_with("animal"), "musicStyle"
  )
  )

attach(Datagotchi)

#### Environnement ####
Enviro_FA <- Datagotchi  %>%
  dplyr::select(c(
    starts_with("act_transport"),
    cons_Vegan, cons_Vege, cons_Meat
  )) %>%
  na.omit()

topdown_fa(Enviro_FA)

Enviro_FA$act_transport_Car_rev <- finverser(Enviro_FA, "act_transport_Car")
Enviro_FA$cons_Meat_rev <- finverser(Enviro_FA, "cons_Meat")

Enviro_FA <- Enviro_FA %>%
  select(-c(act_transport_Car, cons_Meat))

topdown_fa(Enviro_FA)

# J'essaye avec favorite_transport et regime. Vu que c'est moins de 3 vars, je fais un test de corrélation
  # pour checker si on peut les additionner

test <- cor.test(Datagotchi$favorite_transport, Datagotchi$regime)

test$estimate

# Les deux vars ne scalent pas ensemble. On va donc utiliser les deux vars dans notre clustering
  # sans les mettre ensemble

#### Sophistication consommation ####
Sophis_FA <- Datagotchi %>%
  dplyr::select(c(
    cons_regBeers,
    cons_sparklingDrink,
    cons_redWineDrink,
    cons_roseDrink,
    cons_whiteWineDrink,
    cons_microBeers,
    cons_spiritDrink,
    cons_cocktailsDrink)) %>%
  na.omit()

topdown_fa(Sophis_FA)

# donne rien, donc je vais faire une corMatrix du turnout avec les vars

conso <- c("cons_regBeers",
           "cons_sparklingDrink",
           "cons_redWineDrink",
           "cons_roseDrink",
           "cons_whiteWineDrink",
           "cons_microBeers",
           "cons_spiritDrink",
           "cons_cocktailsDrink")

CorrsTurnout <- cor_df(DataPilot, "op_turnout2019", conso)

# rien, on va utiliser toutes les variables pour commencer
