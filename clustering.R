#**********************************************************************************#
########################## Clustering lifestyle-turnout ############################
#**********************************************************************************#

#********************************#
#### 0. PACKAGES  ####
#********************************#

library(tidyverse)
library(psych)
library(utils)
library(ggplot2)
library(corrr)
library(useful)
library(cluster) 
library(NbClust)
library(klaR)
library(factoextra)

#********************************#
#### 0.1 DATA ####
#********************************#  

# Datagotchi
Dt <- read.csv("_SharedFolder_bav-2021/Data/Clean/27-10-2021-DatagotchiHub-2.csv")

#********************************#
#### 0.2 FONCTIONS ####
#********************************#  

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

#********************************#
#### 1 ORGANIZING VARIABLES ####
#********************************#  

# On veut garder les vars qui sont dans le Pilot1

Dt <- Dt %>%
  dplyr::select(-c(
    starts_with("film"), starts_with("animal"), "musicStyle"
  )
  )

##### Transport ####

# Tel que discuté avec Cath, au lieu de faire une échelle on va faire une dummy cityTransport
# qui compte les bike/walk/publicTransportation

Dt$clus_cityTransport <- Dt$act_transport_Bicycle + Dt$act_transport_PublicTransportation + Dt$act_transport_Walk

table(Dt$clus_cityTransport)
table(Dt$act_transport_Bicycle)
table(Dt$act_transport_PublicTransportation)
table(Dt$act_transport_Walk)

# good

##### Habitation ####

# Encore une fois selon Cath: merge condo, loft, appartement, tour

Dt$clus_cityHabitation <- Dt$ses_dwelling_app + Dt$ses_dwelling_condo + Dt$ses_dwelling_loft + Dt$ses_dwelling_tour

table(Dt$clus_cityHabitation)

# good

##### Cafes ####

# Encore une fois selon Cath: dummy variable ou on merge les chaines

Dt$clus_coffeeChains <- Dt$cons_coffee_TimH + Dt$cons_coffee_Starbucks + Dt$cons_coffee_McDo + Dt$cons_coffee_SC

table(Dt$clus_coffeeChains)

# good

##### Outdoor ####

# Tel que Cath l'a fait: merger fishing, hunting, motorizedOutdoorActivities et pickup 

Dt <- Dt %>%
  mutate(act_Fishing = ifelse(act_Fishing == "0.5" | act_Fishing == "0.75" | act_Fishing =="1", 1, 0),
         act_Hunting = ifelse(act_Hunting == "0.5" | act_Hunting == "0.75" | act_Hunting =="1", 1, 0),
         act_MotorizedOutdoorActivities = ifelse(act_MotorizedOutdoorActivities == "0.5" | act_MotorizedOutdoorActivities == "0.75" | act_MotorizedOutdoorActivities =="1", 1, 0)
         )

Dt$clus_outdoorAct <- (Dt$act_Fishing + Dt$act_Hunting + Dt$act_MotorizedOutdoorActivities + Dt$vehicule_PickUp)/4

table(Dt$clus_outdoorAct)

# ok

##### Clothing ####

# Encore une fois en suivant Cath

# merge formel and chic 
Dt$clus_swagFormel <- NA
Dt$clus_swagFormel[Dt$app_swag_Chic == 1 | Dt$app_swag_Formel == 1] <- 1  # chic/formal 
Dt$clus_swagFormel[Dt$app_swag_Chic != 1 & Dt$app_swag_Formel != 1] <- 0  # 

# merge casual and classique 
Dt$clus_swagCasual <- NA
Dt$clus_swagCasual[Dt$app_swag_Casual == 1 | Dt$app_swag_Classique == 1] <- 1  # casual/classique
Dt$clus_swagCasual[Dt$app_swag_Casual != 1 & Dt$app_swag_Classique != 1] <- 0  # 

# Hipster
Dt$clus_swagHippie <- Dt$app_swag_VintageHippBoheme

# Rock
Dt$clus_swagRock <- Dt$app_swag_Rock

# Sport
Dt$clus_swagSport <- Dt$app_swag_Sport

##### Regime ####

# merge vegan and vegeterian 
Dt$clus_noMeat <- NA
Dt$clus_noMeat[Dt$cons_Vege == 0 & Dt$cons_Vegan == 0] <- 0  # meat 
Dt$clus_noMeat[Dt$cons_Vege != 0 | Dt$cons_Vegan != 0] <- 1  # no meat 
table(Dt$clus_noMeat)

##### Shopping ####

# merge independent shops et friperies 
Dt$clus_shopIndep <- Dt$cons_brand_BInd + Dt$cons_brand_Frip
Dt$clus_shopChaines <- Dt$cons_brand_MaR + Dt$cons_brand_GSurf + Dt$cons_brand_ChainesB
Dt$clus_shopOnline <- Dt$cons_brand_OnlineOnly

##### Alcool ####

# Selon Cath, opposer vin à bière

Dt$clus_drinkWine <- Dt$cons_whiteWineDrink + Dt$cons_roseDrink + Dt$cons_redWineDrink + Dt$cons_sparklingDrink
Dt$clus_drinkBeer <- Dt$cons_regBeers + Dt$cons_microBeers

##### Smoking ####

# Selon Cath: no merge (keeping only those who never smoked as a category (opposed to those who smoke, stopped or
# are trying to stop))

Dt$clus_neverSmoked <- Dt$cons_SmokeNever

#********************************#
#### 2 CLUSTERING ####
#*******************************#  

ClusterData <- Dt %>%
  dplyr::select(starts_with("clus")) %>%
  na.omit()

names(ClusterData)
sum_nas(ClusterData)

means <- apply(ClusterData, 2, mean)
sds <- apply(ClusterData, 2, sd)
ClusterNor <- scale(ClusterData, center = means, scale = sds)

##### optimal cluster number ####

set.seed(123)
#DataSample <- ClusterData[sample(nrow(ClusterData), size = 1070, replace = FALSE), ]

fviz_nbclust(ClusterNor, kmeans, method = "wss")
# le elbow/knee se trouve à 8 clusters
#ggsave("_SharedFolder_article-turnout-lifestyles/clustering/optimal_k/wss.png")

fviz_nbclust(ClusterNor, kmeans, method = "silhouette")
# max à 4 clusters
#ggsave("_SharedFolder_article-turnout-lifestyles/clustering/optimal_k/silhouette.png")

# On veut donc 4 clusters pour commencer

##### Making clusters ####
set.seed(123)

K4test <- kmeans(ClusterNor, centers = 4, nstart = 25)
str(K4test)

#k4 <- plot(K4test, data=ClusterNor)

fviz_cluster(K4test, data = ClusterData, geom = "point")
ggsave("_SharedFolder_article-turnout-lifestyles/clustering/k4_kmeans.png",
       width = 12, height = 10)

