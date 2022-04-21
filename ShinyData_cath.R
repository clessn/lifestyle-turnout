#### BEGINNING OF THE DOCUMENT ####

library(tidyverse)

#### Ouvrir la base de données ####

# Datagotchi
Data <- read.csv("_SharedFolder_bav-2021/Data/Clean/27-10-2021-DatagotchiHub-2.csv") %>%
  mutate(metal   = ifelse(musicStyle == "metal" | musicStyle == "industrial", 1, 0),
         rap_rnb = ifelse(musicStyle == "rap_rnb", 1, 0),
         electro = ifelse(musicStyle == "electro", 1, 0), 
         rock = ifelse(musicStyle == "rock", 1, 0),
         country = ifelse(musicStyle == "country", 1, 0),
         alternative_rock = ifelse(musicStyle == "alternative_rock", 1, 0),
         ambiance = ifelse(musicStyle == "ambiance", 1, 0),
         ballad = ifelse(musicStyle == "ballad", 1, 0), 
         blues_soul = ifelse(musicStyle == "blues_soul", 1, 0), 
         classical = ifelse(musicStyle == "classical", 1, 0),
         folk = ifelse(musicStyle == "folk_rock", 1, 0),
         hard_rock = ifelse(musicStyle == "hard_rock", 1, 0),
         jazz = ifelse(musicStyle == "jazz", 1, 0),    
         new_wave = ifelse(musicStyle == "new_wave", 1, 0),
         pop = ifelse(musicStyle == "pop", 1, 0),
         punk = ifelse(musicStyle == "punk", 1, 0), 
         act_Fishing = ifelse(act_Fishing == "0.5" | act_Fishing == "0.75" | act_Fishing =="1", 1, 0), 
         act_Hunting = ifelse(act_Hunting == "0.5" | act_Hunting == "0.75" | act_Hunting =="1", 1, 0), 
         act_VisitsMuseumsGaleries = ifelse(act_VisitsMuseumsGaleries == "0.5" | act_VisitsMuseumsGaleries == "0.75" | act_VisitsMuseumsGaleries =="1", 1, 0), 
         act_Volunteering = ifelse(act_Volunteering == "0.5" | act_Volunteering == "0.75" | act_Volunteering =="1", 1, 0), 
         act_MotorizedOutdoorActivities = ifelse(act_MotorizedOutdoorActivities == "0.5" | act_MotorizedOutdoorActivities == "0.75" | act_MotorizedOutdoorActivities =="1", 1, 0))


# 0.3 Fonctions ####

# Pour cleaner: remplace un chiffre dans une colonne à NA 
fna <- function(df, col, int){
  df[[col]][df[[col]] == int] <- NA
  return(df[[col]])
}


regress <-
  function(data,
           y,
           x,
           reg_type,
           controls = as.character(),
           family = NA) {
    x_express <- paste0(x, collapse = " + ")
    vars_express <- paste0(y, " ~ ", x_express)
    controls_express <- paste0(controls, collapse = " + ")
    varsWcont_express <- if (length(controls) == 0) {
      vars_express
    }
    else {
      paste0(vars_express, " + ", controls_express)
    }
    express <-
      paste0(reg_type, "(", varsWcont_express, ", data = ", data)
    express <- if (is.na(family)) {
      paste0(express, ")")
    }
    else {
      paste0(express, ", family = ", family, ")")
    }
    model <- eval(parse(text = express))
    return(model)
  }

### >>> Test #####
model <- regress(data = "DataPilot",
                 y = "op_turnout2019",
                 x = c("cons_whiteWineDrink", "cons_Smoke",
                       "cons_noDrink", "act_PerformingArts"),
                 controls = c("ses_gender", "ses_age24m"),
                 reg_type = "glm",
                 family = "binomial")


predict_data <- function(data,
                         y,
                         axisX,
                         x,
                         reg_type,
                         controls = as.character(),
                         family = NA,
                         group=NA,
                         xSep=10){
  if (is.na(group)){
    x <- c(axisX,x)
    model <- regress(data=data,y=y,x=x,controls=controls,reg_type = reg_type,family=family)
    
    DataObj <- eval(parse(text=data))
    
    numX <- which(names(DataObj)==axisX)
    minX <- min(DataObj[,numX],na.rm=T)
    maxX <- max(DataObj[,numX],na.rm=T)
    
    expressVars <- c(x,controls)
    expressMean <- paste0(expressVars," = mean(DataObj$",expressVars,", na.rm = T)")
    expressMean[1] <- paste0(axisX," = seq(minX,maxX, (maxX-minX)/xSep)")
    expressMean <- paste0(expressMean,collapse = ", ")
    expressMean <- paste0("data.frame(",expressMean,")")
    PredData <- eval(parse(text=expressMean))
    predictModel <- predict(model, PredData, se.fit=T, type="response")
    
    #erreur <- tryCatch({
    #              predictModel <- predict(model, PredData, se.fit=T, type="response")
    #            },
    #              error = function(cond){
    #                message(paste0("Warning: really weak predictive model for ", y, " and ", axisX))
    #                TRUE
    #              }
    #              )
    #
    #if (erreur == TRUE) {
    #  GraphDataOut <- data.frame(y = y, axisX = axisX, "se" = NA, "ciLow" = NA, "ciHigh" = NA)
    #  return(GraphDataOut)
    #} else {
    #  predictModel <- predict(model, PredData, se.fit=T, type="response")
    #}
    
    predY <- predictModel$fit
    predX <- seq(minX,maxX, (maxX-minX)/xSep)
    se <- predictModel$se.fit
    
    GraphData <- data.frame(predY,predX,se)
    GraphData$ciLow68<-GraphData$predY - (1.96*GraphData$se)
    GraphData$ciHi68 <-GraphData$predY + (1.96*GraphData$se)
    names(GraphData) <- c(y,axisX,"se","ciLow","ciHigh")
    GraphDataOut <- GraphData
  }
  else {
    if (group %in% x){
      x <- c(axisX,x)
      model <<- regress(data=data,y=y,x=x,controls=controls,reg_type = reg_type,family=family)
      
      DataObj <- eval(parse(text=data))
      
      numX <- which(names(DataObj)==axisX)
      minX <- min(DataObj[,numX],na.rm=T)
      maxX <- max(DataObj[,numX],na.rm=T)
      x <- x[-which(x==group)]
      groupCat <- as.numeric(names(table(DataObj[which(names(DataObj)==group)])))
      
      for (i in groupCat){
        groupX <- paste0(group,"=",i)
        
        expressVars <- c(x,controls)
        expressMean <- paste0(expressVars," = mean(DataObj$",expressVars,", na.rm = T)")
        expressMean <- c(groupX,expressMean)
        expressMean <- c(paste0(axisX," = seq(minX,maxX, (maxX-minX)/xSep)"),expressMean)
        expressMean <- paste0(expressMean,collapse = ", ")
        expressMean <- paste0("data.frame(",expressMean,")")
        
        PredData <- eval(parse(text=expressMean))
        predictModel <- predict(model,PredData,se.fit=T,type="response")
        
        predY <- predictModel$fit
        predX <- seq(minX,maxX, (maxX-minX)/xSep)
        se <- predictModel$se.fit
        
        GraphData <- data.frame(predY,predX,se)
        GraphData$group <- i
        GraphData$ciLow68<-GraphData$predY - (1.96*GraphData$se)
        GraphData$ciHi68 <-GraphData$predY + (1.96*GraphData$se)
        GraphData$pValAxisX <- summary(model)$coefficients[2,4]  
        names(GraphData) <- c(y,axisX,"se", "group", "ciLow","ciHigh","pValAxisX")
        if (i==groupCat[1]){
          GraphDataOut <- GraphData
        }
        else {
          GraphDataOut <- rbind(GraphDataOut,GraphData)
        }
      }
    }
    else if (group %in% axisX){
      stop(paste0("Argument 'group=",group,"' can't be the same as axisX!"))
    }
    else {
      stop(paste0("Argument 'group=",group,"' must be present in x!"))
    }
  }
  return(GraphDataOut)
}

# >>> Test ####

Test <- predict_data(data = "DataPilot",
                        y = "op_turnout2019",
                        axisX ="ses_income",
                        x = "ses_age_cat_v2",
                        controls = c("ses_gender", "ses_celib"),
                        reg_type = "glm",
                        family = "binomial")


GraphDataGroup %>% filter(pValAxisX<=0.05) %>%
  ggplot(aes(x = valAxisX, y = valDependent,group=group)) +
  geom_line(aes(color=group)) +
  geom_ribbon(aes(ymin = ciLow,
                  ymax = ciHigh,fill=group), alpha = 0.2) +
  facet_grid(dependent~axisX) +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = "top")

# Get data for Shiny ####

# Generating a dataframe of all possible y-axisX combinations
Dt <- Data
Dt_string <- deparse(substitute(Dt))
dependents <- names(Dt %>% select(contains("voteIntent")))

# Prédéfinir les groupes, séparer le menu
Dt$age3554 <- 1 - Dt$age34m - Dt$age55p
Dt$incomeMid <- 1 - Dt$incomeLow - Dt$incomeHigh
Dt$ontario <- 1 - Dt$west - Dt$quebec - Dt$maritimes
Dt$ses_sexOri_other <- 1 - Dt$ses_hetero
## DWELLING 
# merge condo, loft, appartement, tour 
Dt$ses_dwelling_cityAppMerged <- NA 
Dt$ses_dwelling_cityAppMerged[Dt$ses_dwelling_app == 1 | Dt$ses_dwelling_condo == 1 | Dt$ses_dwelling_tour == 1 | Dt$ses_dwelling_coop == 1] <- 1 # city transports
Dt$ses_dwelling_cityAppMerged[Dt$ses_dwelling_app != 1 & Dt$ses_dwelling_condo != 1 & Dt$ses_dwelling_tour != 1 & Dt$ses_dwelling_coop != 1] <- 0 
#table(Dt$ses_dwelling_cityAppMerged)
Dt$male <- 1 - Dt$female

groups <- list(group_age = c("age34m", "age3554", "age55p"),
               group_educ = c("educBHS", "educUniv"),
               group_income = c("incomeLow", "incomeMid", "incomeHigh"),
               group_region = c("ontario", "quebec", "maritimes", "west"),
               group_sexOri = c("ses_hetero", "ses_sexOri_other"),
               group_immigrant = c("immigrant"),
               group_habit = c("ses_dwelling_loft", "ses_dwelling_detachedHouse", "ses_dwelling_townHouse",
                               "ses_dwelling_semiDetached", "ses_dwelling_HLM", "ses_dwelling_mobile", "ses_dwelling_cityAppMerged",
                               "ses_dwelling_other"),
               group_sex = c("male", "female"))

Uniques <- data.frame(col = as.character(),
                      table = as.character())

for (i in 1:length(names(Dt))){
  groupi <- names(Dt)[i]
  tablei <- paste0(names(table(Dt[[groupi]])), collapse = ", ")
  Uniques[i, 1] <- groupi
  Uniques[i, 2] <- tablei
}

Max <- data.frame(col = as.character(),
                  table = as.character())

for (i in 1:length(names(Dt))){
  groupi <- names(Dt)[i]
  tablei <- max(Dt[[groupi]], na.rm = T)
  Max[i, 1] <- groupi
  Max[i, 2] <- tablei
}

# AxisX vars to add

## FOOD REGIME 
# merge vegan and vegeterian 
Dt$cons_noMeat <- NA
Dt$cons_noMeat[Dt$cons_Vege == 0 & Dt$cons_Vegan == 0] <- 0  # meat 
Dt$cons_noMeat[Dt$cons_Vege != 0 | Dt$cons_Vegan != 0] <- 1  # no meat 
#table(Dt$cons_noMeat)

## SMOKING 
# no merge (keeping only those who never smoked as a category (opposed to those who smoke, stopped or
# are trying to stop))

## CLOTHING STYLE 
# merge formel and chic 
Dt$app_swag_FormelMerged <- NA
Dt$app_swag_FormelMerged[Dt$app_swag_Chic == 1 | Dt$app_swag_Formel == 1] <- 1  # chic/formal 
Dt$app_swag_FormelMerged[Dt$app_swag_Chic != 1 & Dt$app_swag_Formel != 1] <- 0  # 
#table(Dt$app_swag_FormelMerged)

# merge casual and classique 
Dt$app_swag_CasualMerged <- NA
Dt$app_swag_CasualMerged[Dt$app_swag_Casual == 1 | Dt$app_swag_Classique == 1] <- 1  # casual/classique
Dt$app_swag_CasualMerged[Dt$app_swag_Casual != 1 & Dt$app_swag_Classique != 1] <- 0  # 
#table(Dt$app_swag_CasualMerged)

## DRINKS
# merge all wines (rosé, white, red and champagne) 
Dt$cons_WineMerged <- NA
Dt$cons_WineMerged[Dt$cons_redWineDrink == 1 | Dt$cons_whiteWineDrink == 1 | Dt$cons_roseDrink == 1 | Dt$cons_sparklingDrink == 1] <- 1  # wine person  
Dt$cons_WineMerged[Dt$cons_redWineDrink != 1 & Dt$cons_whiteWineDrink != 1 & Dt$cons_roseDrink != 1 & Dt$cons_sparklingDrink != 1] <- 0  # 
#table(Dt$cons_WineMerged)

## COFFE SHOPS 
# merge all commercial chains 
Dt$cons_coffee_chains <- NA 
Dt$cons_coffee_chains[Dt$cons_coffee_TimH == 1 | Dt$cons_coffee_Starbucks == 1 | Dt$cons_coffee_SC == 1 | Dt$cons_coffee_McDo == 1] <- 1 # chains coffee 
Dt$cons_coffee_chains[Dt$cons_coffee_TimH != 1 & Dt$cons_coffee_Starbucks != 1 & Dt$cons_coffee_SC != 1 & Dt$cons_coffee_McDo != 1] <- 0 
#table(Dt$cons_coffee_chains)

## PETS 
# merge domestic animals 
Dt$animal_domesticMerged <- NA 
Dt$animal_domesticMerged[Dt$animal_domestic == 1 | Dt$animal_cat == 1 | Dt$animal_catNdog == 1 | Dt$animal_catNdog == 1] <- 1 # domestic animals 
Dt$animal_domesticMerged[Dt$animal_domestic != 1 & Dt$animal_cat != 1 & Dt$animal_catNdog != 1 & Dt$animal_catNdog != 1] <- 0
#table(Dt$animal_domesticMerged)

## TRANSPORTS
# merging all city transits (walk, bicyle, public transit)
Dt$act_transport_cityMerged <- NA 
Dt$act_transport_cityMerged[Dt$act_transport_PublicTransportation == 1 | Dt$act_transport_Walk == 1 | Dt$act_transport_Bicycle == 1] <- 1 # city transports
Dt$act_transport_cityMerged[Dt$act_transport_PublicTransportation != 1 & Dt$act_transport_Walk != 1 & Dt$act_transport_Bicycle != 1] <- 0 
#table(Dt$act_transport_cityMerged)

## VEHICULE 
# merging luxury and roadster cars 
Dt$vehicule_LuxeMerged <- NA 
Dt$vehicule_LuxeMerged[Dt$vehicule_Cabriolet == 1 | Dt$vehicule_luxury == 1] <- 1 # luxury car
Dt$vehicule_LuxeMerged[Dt$vehicule_Cabriolet != 1 & Dt$vehicule_luxury != 1] <- 0
#table(Dt$vehicule_LuxeMerged)

# merging 4x4 and SUV
Dt$vehicule_SUVMerged <- NA 
Dt$vehicule_SUVMerged[Dt$vehicule_4x4 == 1 | Dt$vehicule_VUS == 1] <- 1  # suv car 
Dt$vehicule_SUVMerged[Dt$vehicule_4x4 != 1 & Dt$vehicule_VUS != 1] <- 0 
#table(Dt$vehicule_SUVMerged)

## SHOPPING 
# merge independent shops, department stores and chain stores 
Dt$cons_brand_shopMerged <- NA 
Dt$cons_brand_shopMerged[Dt$cons_brand_MaR == 1 | Dt$cons_brand_ChainesB == 1 | Dt$cons_brand_BInd == 1] <- 1 # city transports
Dt$cons_brand_shopMerged[Dt$cons_brand_MaR != 1 & Dt$cons_brand_ChainesB != 1 & Dt$cons_brand_BInd != 1] <- 0 
#table(Dt$cons_brand_shopMerged)


# Remove ses and others
axisX <- names(Dt %>%
  select(-c(musicStyle, realPred, female, male, contains("age"),
            contains("ses"), west, maritimes, quebec, ontario,
            immigrant, contains("lang"), contains("educ"),
            contains("income"), contains("voteIntent"))))

CombsGroup <- expand.grid(dependents, axisX, names(groups))

# Create empty GraphData dataframe
GraphDataGroup <- data.frame(dependent = as.character(),
                             axisX = as.character(),
                             group = as.character(),
                             cate = as.character(),
                             valDependent = as.numeric(),
                             valAxisX = as.numeric(),
                             se = as.numeric(),
                             ciLow = as.numeric(),
                             ciHigh = as.numeric(),
                             pValAxisX=as.numeric())


for (i in 1:nrow(CombsGroup)){
  #testing loop
  #i <- 208
  #testing loop
  
  dependenti <- as.character(CombsGroup[i, 1])
  axisXi <- as.character(CombsGroup[i, 2])
  groupi_name <- as.character(CombsGroup[i, 3])
  cates <- groups[[groupi_name]]
  if (i %% 10 == 0){
    print(paste0(i, "/", nrow(CombsGroup), " (", round(i/nrow(CombsGroup)*100), "%) => ", dependenti, " - ", axisXi, " - ", groupi))
  }
  for (j in 1:length(cates)){
    #j<- 1
    catej <- cates[j]
    PredDfj <- predict_data(data = "Dt",
                            y = dependenti,
                            axisX = axisXi,
                            x = cates,
                            #controls = controls,
                            reg_type = "glm",
                            family = "binomial",
                            group = catej)
    
    PredDfj <- PredDfj %>%
      filter(group == 1) %>%
      mutate(group = catej,
             pValAxisX=coef(summary(model))[2,4])
    
    PredDfj <- cbind(dependenti, axisXi, groupi_name, PredDfj) %>%
      select(c(dependent = dependenti, axisX = axisXi, group = groupi_name,
               cate = group,
               valDependent = as.character(CombsGroup[i, 1]),
               valAxisX = as.character(CombsGroup[i, 2]),
               se, ciLow, ciHigh,pValAxisX))
    
    GraphDataGroup <- rbind(GraphDataGroup, PredDfj)
  }
}

saveRDS(GraphDataGroup, "_SharedFolder_article-turnout-lifestyles/data/ShinyCathData.rds")
saveRDS(GraphDataGroup, "shiny-cath/ShinyData.rds")
GraphDataGroup <- readRDS("_SharedFolder_article-turnout-lifestyles/data/ShinyData.rds")

library(tidyverse)
GraphDataGroup %>% filter(pValAxisX<=0.05) %>%
  filter(group %in% c("ses_age_24m", "ses_age_2534", "ses_age3554", "ses_age55p")) %>%
  ggplot(aes(x = valAxisX, y = valDependent,group=group)) +
  geom_line(aes(color=group)) +
  geom_ribbon(aes(ymin = ciLow,
                  ymax = ciHigh,fill=group), alpha = 0.2) +
  facet_grid(dependent~axisX) +
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = "top")

Data <- readRDS("shiny-turnout/ShinyData.rds") %>%
  filter(!grepl('voteIntent', axisX) &
           !grepl('educ', axisX) &
           !grepl('turnout', axisX) &
           !grepl("potential", axisX) &
           !grepl("income", axisX) &
           !grepl("quebec", axisX) &
           !grepl("ontario", axisX) &
           !grepl("maritimes", axisX) &
           !grepl("west", axisX) &
           !grepl("male", axisX) &
           !grepl("female", axisX) &
           !grepl("age", axisX) &
           !grepl("lang", axisX))






