#### BEGINNING OF THE DOCUMENT ####

library(tidyverse)

#### Ouvrir la base de données ####

# Pilot
DataPilot <- readRDS("_SharedFolder_article-turnout-lifestyles/data/CleanData-Lifestyle.rds")


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
Dt <- DataPilot
Dt_string <- deparse(substitute(Dt))
dependents <- "op_turnout2019" # turnout 2019

# Prédéfinir les groupes, séparer le menu
groups <- list(group_age = c("ses_age24m", "ses_age2534", "ses_age3554", "ses_age55p"),
               group_educ = c("ses_educ_None", "ses_educ_Prim", "ses_educ_Sec", "ses_educ_Coll", 
                               "ses_educ_Bacc", "ses_educ_Master", "ses_educ_PhD"),
               group_income = c("incomeLow", "incomeMid", "incomeHigh"),
               group_region = c("ontario", "quebec", "maritimes", "west"),
               group_urban = c("ses_urbain", "ses_sururbain", "ses_rural"),
               group_relation = c("ses_celib", "ses_married", "ses_relationship"),
               group_sexOri = c("ses_hetero", "ses_gai", "ses_bisex", "ses_sexOri_other"),
               group_immigrant = c("immigrant"),
               group_habit = c("ses_dwelling_app", "ses_dwelling_loft", "ses_dwelling_condo", "ses_dwelling_tour",
                                "ses_dwelling_detachedHouse", "ses_dwelling_townHouse", "ses_dwelling_semiDetached",
                                "ses_dwelling_coop", "ses_dwelling_HLM", "ses_dwelling_mobile", "ses_dwelling_other"),
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
  tablei <- max(Dt[[groupi]])
  Max[i, 1] <- groupi
  Max[i, 2] <- tablei
}

# Only keep normalized variables
axisX <- Max %>%
  filter(table <= 1) %>%
  pull(col)

# Remove ses
AxisX <- Dt %>%
  select(all_of(axisX)) %>%
  select(-c(starts_with("ses"), "op_turnout2019"))

axisX <- names(AxisX)

## Générer les variables de l'axe des X
#Dt$axisX_cityTransport <- Dt$act_transport_Bicycle + Dt$act_transport_PublicTransportation + Dt$act_transport_Walk
#table(Dt$axisX_cityTransport)
#
#Dt$axisX_coffeeChains <- Dt$cons_coffee_TimH + Dt$cons_coffee_Starbucks + Dt$cons_coffee_McDo + Dt$cons_coffee_SC
#table(Dt$axisX_coffeeChains)
#Dt$axisX_coffeeChains[Dt$axisX_coffeeChains == 2] <- 1
#table(Dt$axisX_coffeeChains)
#
#Dt$axisX_outdoorAct <- Dt$act_Fishing + Dt$act_Hunting + Dt$act_MotorizedOutdoorActivities
#table(Dt$axisX_outdoorAct)
#Dt$axisX_outdoorAct[Dt$axisX_outdoorAct > 1] <- 1
#table(Dt$axisX_outdoorAct)
#hist(Dt$axisX_outdoorAct)
#
#Dt$axisX_cons_smoke <- fna(Dt, "cons_smoke_status", 5)
#table(Dt$axisX_cons_smoke)
#
#Dt$axisX_Meat <- Dt$cons_Meat
#table(Dt$axisX_Meat)
##fuck Meat
#
#Dt$axisX_shopIndep <- Dt$cons_brand_BInd + Dt$cons_brand_Frip
#table(Dt$axisX_shopIndep)
#
#Dt$axisX_shopChaines <- Dt$cons_brand_MaR + Dt$cons_brand_GSurf + Dt$cons_brand_ChainesB
#table(Dt$axisX_shopChaines)
#
#Dt$axisX_shopOnline <- Dt$cons_brand_OnlineOnly
#table(Dt$axisX_shopOnline)
#
#Dt$axisX_drinkWine <- Dt$cons_whiteWineDrink + Dt$cons_roseDrink + Dt$cons_redWineDrink + Dt$cons_sparklingDrink
#table(Dt$axisX_drinkWine)
#
#Dt$axisX_drinkBeer <- Dt$cons_regBeers + Dt$cons_microBeers
#table(Dt$axisX_drinkBeer)
#
#axisX <- c(names(Dt %>% select(starts_with("axisX"))))
#
#Dt$controls_cityHabitation <- Dt$ses_dwelling_app + Dt$ses_dwelling_condo + Dt$ses_dwelling_loft + Dt$ses_dwelling_tour
#Dt$controls_educ <- Dt$ses_educ
#Dt$controls_so <- Dt$ses_sexualorientation

#controls <- c(names(Dt %>% select(starts_with("controls"))))

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
#  PredDfi <- predict_data(data = "Dt",
#                          y = dependenti,
#                          axisX = axisXi,
#                          x = group,
#                          #controls = controls,
#                          reg_type = "glm",
#                          family = "binomial",
#                          group = groupi)
#  
#  PredDfi <- PredDfi %>%
#    filter(group == 1) %>%
#    mutate(group = groupi,
#           pValAxisX=coef(summary(model))[2,4])
#  
#  PredDfi <- cbind(dependenti, axisXi, groupi, PredDfi) %>%
#    select(c(dependent = dependenti, axisX = axisXi, group = groupi,
#             valDependent = as.character(CombsGroup[i, 1]),
#             valAxisX = as.character(CombsGroup[i, 2]),
#             se, ciLow, ciHigh,pValAxisX))
#  
#  GraphDataGroup <- rbind(GraphDataGroup, PredDfi)
}

saveRDS(GraphDataGroup, "_SharedFolder_article-turnout-lifestyles/data/ShinyData.rds")
saveRDS(GraphDataGroup, "shiny-turnout/ShinyData.rds")
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


# Most sign relations ####

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

Agg <- Data %>%
  mutate(modelId = paste0(axisX, " ~ ", cate)) %>%
  group_by(modelId) %>%
  summarise(pval = unique(pValAxisX))

Agg2 <- Data %>%
  group_by(axisX) %>%
  summarise(pval = mean(pValAxisX))


Lifestyle_Ses <- Data %>%
  filter(pValAxisX <= 0.05 &
        (valAxisX == 0.1 |
         valAxisX == 1)) %>%
  select(c(axisX, cate, valDependent, valAxisX, pValAxisX)) %>%
  pivot_wider(.,
              names_from = valAxisX,
              names_prefix = "val_",
              values_from = valDependent) %>%
  mutate(effect_turnout = val_1 - val_0.1) %>%
  select(c(lifestyle = axisX, ses = cate, pValue = pValAxisX, lifestyle0 = val_0.1,
           lifestyle1 = val_1, effect_turnout))

LifestyleOnly <- Lifestyle_Ses %>%
  group_by(lifestyle) %>%
  summarise(mean_pValue = mean(pValue),
            mean_lifestyle0 = mean(lifestyle0),
            mean_lifestyle1 = mean(lifestyle1),
            mean_effect_turnout = mean(effect_turnout))


model <- glm(op_turnout2019 ~ cons_socmedia_Other + cons_socmedia_Snap + cons_socmedia_Tiktok +
               cons_socmedia_Facebook + cons_socmedia_Insta + cons_socmedia_YT + cons_socmedia_Twitter,
             family = "binomial", data = DataPilot)


DataPilot$socmedia_badTurnout <- NA
DataPilot$socmedia_badTurnout <- DataPilot$cons_socmedia_Insta + DataPilot$cons_socmedia_Snap +
  DataPilot$cons_socmedia_Tiktok + DataPilot$cons_socmedia_Other

vars_to_test <- c("socmedia_badTurnout", "act_transport_Taxi", "act_transport_PublicTransportation",
                  "act_MotorizedOutdoorActivities", "act_DoingTeamSport", "act_holidays_Relax", "cons_Vegan",
                  "cons_noDrink", "cons_brand_OnlineOnly", "act_VideoGames", "cons_coffee_type_Instant")

Results <- data.frame(var = as.character(),
                      varVoted = as.numeric(),
                      varTotal = as.numeric(),
                      varPropVoted = as.numeric(),
                      varEffect = as.numeric(),
                      pValue = as.numeric(),
                      ciLow = as.numeric(),
                      ciHigh = as.numeric()
                      )

for (i in 1:length(vars_to_test)){
  #i <- 1
  #var <- vars_to_test[i]
  var <- "act_VideoGames"
  Desc <- as.data.frame(DataPilot %>%
                          group_by(.data[[var]]) %>%
                          summarise(voted2019 = sum(op_turnout2019),
                                    n = as.numeric(n())) %>%
                          mutate(prop = voted2019/n))
  
  test <- binom.test(x = Desc[2, 2],
                     n = Desc[2, 3],
                     p = sum(DataPilot$op_turnout2019)/nrow(DataPilot),
                     alternative = "less",
                     conf.level = 0.95)
  
  eff <- test$estimate - test$null.value
  
  Results[i, 1] <- var
  Results[i, 2] <- test$statistic
  Results[i, 3] <- test$parameter
  Results[i, 4] <- test$estimate
  Results[i, 5] <- eff
  Results[i, 6] <- test$p.value
  Results[i, 7] <- test$conf.int[1]
  Results[i, 8] <- test$conf.int[2]
}



# Merger taxi et public transport: dépendent d'un service de transport