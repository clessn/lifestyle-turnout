####################################################################################################
######################### Domestication BAV 2021 - Sondage Synopsis ################################
####################################################################################################

library(foreign)
library(tidyverse)
library(haven)
library(varhandle)

minmaxNormalization <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}


#opening the data frame !! N'oubliez pas d'ouvrir dans le r projet bav-2021 pour éviter la multiplication des paths !!
#  Personne ne devrait avoir à modifier les paths ci-dessous
#D <- read_sav("~/Dropbox/collaborateurs/clessnProjets/_SharedFolder_bav-2021/Data/Raw/UL04A-pnd.Sav")
D <- read_sav("../bav-2021/_SharedFolder_bav-2021/Data/Raw/UL04A-pnd.Sav")
#C <- read.spss("~/Dropbox/collaborateurs/clessnProjets/_SharedFolder_bav-2021/Data/Raw/UL04alangue.Sav", to.data.frame = T)
#C <- read.spss("../bav-2021/_SharedFolder_bav-2021/Data/Raw/UL04alangue.Sav", to.data.frame = T)
#E <- readxl::read_xlsx("~/Dropbox/collaborateurs/clessnProjets/_SharedFolder_bav-2021/Data/Raw/ULA04-OPEN.xlsx")
E <- readxl::read_xlsx("../bav-2021/_SharedFolder_bav-2021/Data/Raw/ULA04-OPEN.xlsx")
#df_origin <- read_sav("~/Dropbox/collaborateurs/clessnProjets/_SharedFolder_bav-2021/Data/Raw/UL04A-pnd.Sav")
df_origin <- read_sav("../bav-2021/_SharedFolder_bav-2021/Data/Raw/UL04A-pnd.Sav")


CleanData2 <- data.frame(X = c(1:nrow(df_origin)))

df_origin1 <- as.data.frame(sapply(df_origin, as.numeric))

# write.csv(D, "../bav-2021/_SharedFolder_bav-2021/Data/Raw/UL04A-pnd.csv")
# Data1 <- read.csv("../bav-2021/_SharedFolder_bav-2021/Data/Raw/UL04A-pnd.csv")

# for (name in names(Data)) {
#   type <- class(Data[[name]]);
#   if (type == "factor") {
#     print(name);
#     print(anyDuplicated(levels(Data[[name]])));    
#   }
# }

Data <- inner_join(E, D, C, by = "CASEID") #binding the two dataframes together 
Data$LANGM <- NULL #remove the original "langue maternelle" variable from the data frame


#### 0.1 Creating a clean empty dataframe ####
CleanData <- data.frame(year=seq(2021, 2021, length=nrow(Data)))

#### 0.2 Year ####
CleanData$year <- seq(2021, 2021,length=nrow(Data))

CleanData$CASEID <- D$CASEID
CleanData2$CASEID <- df_origin1$CASEID


#### Patrick ####

## GENRE 
CleanData2$male <- 0
CleanData2$male[df_origin1$SEXE == 1] <- 1
table(CleanData2$male)

CleanData2$female <- 0
CleanData2$female[df_origin1$SEXE == 2] <- 1
table(CleanData2$female)

CleanData2$ses_genderOther <- 0
CleanData2$ses_genderOther[df_origin1$SEXE == 3] <- 1
table(CleanData2$ses_genderOther)

CleanData2$ses_gender <- NA
CleanData2$ses_gender[df_origin1$SEXE == 1] <- 1 #male 
CleanData2$ses_gender[df_origin1$SEXE == 2] <- 2 #female 
table(CleanData2$ses_gender)

## ÂGE 
CleanData2$age34m <- 0
CleanData2$age34m[df_origin1$QAGE >= 18 & df_origin1$QAGE < 35] <- 1
table(CleanData2$age34m)

CleanData2$age3554 <- 0
CleanData2$age3554[df_origin1$QAGE >= 35 & df_origin1$QAGE < 55] <- 1
table(CleanData2$age3554)

CleanData2$age55p <- 0
CleanData2$age55p[df_origin1$QAGE >= 55] <- 1
table(CleanData2$age55p)

CleanData2$ses_age <- NA 
CleanData2$ses_age[df_origin1$QAGE >= 18 & df_origin1$QAGE < 35] <- 1 #18-35
CleanData2$ses_age[df_origin1$QAGE >= 35 & df_origin1$QAGE < 55] <- 2 #35-55
CleanData2$ses_age[df_origin1$QAGE >= 55] <- 3 #55 ans et plus
table(CleanData2$ses_age)

# Celle ci n'a que des NA partout dans la source de données
#CleanData$sesPostalCode <- df_origin$CP 

## LANGUE MATERNELLE - quelle est votre langue maternelle, autrement dit, la première langue que vous avez apprise et que vous comprenez encore aujourd'hui?

CleanData2$langFr <- 0
CleanData2$langFr[df_origin1$LANGM == 2] <- 1
table(CleanData2$langFr)

CleanData2$langEn <- 0
CleanData2$langEn[df_origin1$LANGM == 1] <- 1
table(CleanData2$langEn)

CleanData2$ses_languageOther <- 0
CleanData2$ses_languageOther[df_origin1$LANGM == 3] <- 1
table(CleanData2$ses_languageOther)

CleanData2$ses_language <- NA
CleanData2$ses_language[df_origin1$LANGM == 2] <- 1 #French
CleanData2$ses_language[df_origin1$LANGM == 1] <- 2 #English
CleanData2$ses_language[df_origin1$LANGM == 3] <- 3 #Other
table(CleanData2$ses_language)



## Que faites-vous le plus souvent pour faire de l'exercice? 

CleanData2$act_exercice <-NA 
CleanData2$act_exercice[df_origin1$B1M1 == 1 | df_origin1$B1M2 == 1 | df_origin1$B1M3 == 1 | df_origin1$B1M4 == 1 | df_origin1$B1M5 == 1 | df_origin1$B1M6 == 1 | df_origin1$B1M7 == 1] <- 1  # "Gym"
CleanData2$act_exercice[df_origin1$B1M1 == 2 | df_origin1$B1M2 == 2 | df_origin1$B1M3 == 2 | df_origin1$B1M4 == 2 | df_origin1$B1M5 == 2 | df_origin1$B1M6 == 2 | df_origin1$B1M7 == 2] <- 2  # "Team sport"
CleanData2$act_exercice[df_origin1$B1M1 == 3 | df_origin1$B1M2 == 3 | df_origin1$B1M3 == 3 | df_origin1$B1M4 == 3 | df_origin1$B1M5 == 3 | df_origin1$B1M6 == 3 | df_origin1$B1M7 == 3] <- 3  # "Walk"
CleanData2$act_exercice[df_origin1$B1M1 == 4 | df_origin1$B1M2 == 4 | df_origin1$B1M3 == 4 | df_origin1$B1M4 == 4 | df_origin1$B1M5 == 4 | df_origin1$B1M6 == 4 | df_origin1$B1M7 == 4] <- 4  # "Running"
CleanData2$act_exercice[df_origin1$B1M1 == 5 | df_origin1$B1M2 == 5 | df_origin1$B1M3 == 5 | df_origin1$B1M4 == 5 | df_origin1$B1M5 == 5 | df_origin1$B1M6 == 5 | df_origin1$B1M7 == 5] <- 5  # "Yoga"
CleanData2$act_exercice[df_origin1$B1M1 == 6 | df_origin1$B1M2 == 6 | df_origin1$B1M3 == 6 | df_origin1$B1M4 == 6 | df_origin1$B1M5 == 6 | df_origin1$B1M6 == 6 | df_origin1$B1M7 == 6] <- 6  # "Swimming"
CleanData2$act_exercice[df_origin1$B1M1 == 7 | df_origin1$B1M2 == 7 | df_origin1$B1M3 == 7 | df_origin1$B1M4 == 7 | df_origin1$B1M5 == 7 | df_origin1$B1M6 == 7 | df_origin1$B1M7 == 7] <- 7  # "Other exercice"
CleanData2$act_exercice[df_origin1$B1M1 == 8 | df_origin1$B1M2 == 8 | df_origin1$B1M3 == 8 | df_origin1$B1M4 == 8 | df_origin1$B1M5 == 8 | df_origin1$B1M6 == 8 | df_origin1$B1M7 == 8] <- 8  # "No exercice"
table(CleanData2$act_exercice)

CleanData2$act_Gym <- 0
CleanData2$act_Gym[df_origin1$B1M1 == 1 |
                    df_origin1$B1M2 == 1 | 
                    df_origin1$B1M3 == 1 | 
                    df_origin1$B1M4 == 1 | 
                    df_origin1$B1M5 == 1 | 
                    df_origin1$B1M6 == 1 | 
                    df_origin1$B1M7 == 1] <- 1
table(CleanData2$act_Gym)


CleanData2$act_TeamSport <- 0
CleanData2$act_TeamSport[df_origin1$B1M1 == 2 |
                          df_origin1$B1M2 == 2 | 
                          df_origin1$B1M3 == 2 | 
                          df_origin1$B1M4 == 2 | 
                          df_origin1$B1M5 == 2 | 
                          df_origin1$B1M6 == 2 | 
                          df_origin1$B1M7 == 2] <- 1
table(CleanData2$act_TeamSport)

CleanData2$act_Walk <- 0
CleanData2$act_Walk[df_origin1$B1M1 == 3 |
                     df_origin1$B1M2 == 3 | 
                     df_origin1$B1M3 == 3 | 
                     df_origin1$B1M4 == 3 | 
                     df_origin1$B1M5 == 3 | 
                     df_origin1$B1M6 == 3 | 
                     df_origin1$B1M7 == 3] <- 1
table(CleanData2$act_Walk)


CleanData2$act_Run <- 0
CleanData2$act_Run[df_origin1$B1M1 == 4 |
                    df_origin1$B1M2 == 4 | 
                    df_origin1$B1M3 == 4 | 
                    df_origin1$B1M4 == 4 | 
                    df_origin1$B1M5 == 4 | 
                    df_origin1$B1M6 == 4 | 
                    df_origin1$B1M7 == 4] <- 1
table(CleanData2$act_Run)

CleanData2$act_Yoga <- 0
CleanData2$act_Yoga[df_origin1$B1M1 == 5 |
                     df_origin1$B1M2 == 5 | 
                     df_origin1$B1M3 == 5 | 
                     df_origin1$B1M4 == 5 | 
                     df_origin1$B1M5 == 5 | 
                     df_origin1$B1M6 == 5 | 
                     df_origin1$B1M7 == 5] <- 1
table(CleanData2$act_Yoga)

CleanData2$act_Swimming <- 0
CleanData2$act_Swimming[df_origin1$B1M1 == 6 |
                         df_origin1$B1M2 == 6 | 
                         df_origin1$B1M3 == 6 | 
                         df_origin1$B1M4 == 6 | 
                         df_origin1$B1M5 == 6 | 
                         df_origin1$B1M6 == 6 | 
                         df_origin1$B1M7 == 6] <- 1
table(CleanData2$act_Swimming)

CleanData2$act_Other <- 0
CleanData2$act_Other[df_origin1$B1M1 == 7 |
                      df_origin1$B1M2 == 7 | 
                      df_origin1$B1M3 == 7 | 
                      df_origin1$B1M4 == 7 | 
                      df_origin1$B1M5 == 7 | 
                      df_origin1$B1M6 == 7 | 
                      df_origin1$B1M7 == 7] <- 1
table(CleanData2$act_Other)

CleanData2$act_None <- 0
CleanData2$act_None[df_origin1$B1M1 == 8 |
                     df_origin1$B1M2 == 8 | 
                     df_origin1$B1M3 == 8 | 
                     df_origin1$B1M4 == 8 | 
                     df_origin1$B1M5 == 8 | 
                     df_origin1$B1M6 == 8 | 
                     df_origin1$B1M7 == 8] <- 1
table(CleanData2$act_None)

## Que faites-vous le plus souvent pour faire de l'exercice? # 2

CleanData2$act_exercice <-NA 
CleanData2$act_exercice[df_origin1$B1M1 == 1 | df_origin1$B1M2 == 1 | df_origin1$B1M3 == 1 | df_origin1$B1M4 == 1 | df_origin1$B1M5 == 1 | df_origin1$B1M6 == 1 | df_origin1$B1M7 == 1] <- 1  # "Gym"
CleanData2$act_exercice[df_origin1$B1M1 == 2 | df_origin1$B1M2 == 2 | df_origin1$B1M3 == 2 | df_origin1$B1M4 == 2 | df_origin1$B1M5 == 2 | df_origin1$B1M6 == 2 | df_origin1$B1M7 == 2] <- 2  # "Team sport"
CleanData2$act_exercice[df_origin1$B1M1 == 3 | df_origin1$B1M2 == 3 | df_origin1$B1M3 == 3 | df_origin1$B1M4 == 3 | df_origin1$B1M5 == 3 | df_origin1$B1M6 == 3 | df_origin1$B1M7 == 3] <- 3  # "Walk"
CleanData2$act_exercice[df_origin1$B1M1 == 4 | df_origin1$B1M2 == 4 | df_origin1$B1M3 == 4 | df_origin1$B1M4 == 4 | df_origin1$B1M5 == 4 | df_origin1$B1M6 == 4 | df_origin1$B1M7 == 4] <- 4  # "Running"
CleanData2$act_exercice[df_origin1$B1M1 == 5 | df_origin1$B1M2 == 5 | df_origin1$B1M3 == 5 | df_origin1$B1M4 == 5 | df_origin1$B1M5 == 5 | df_origin1$B1M6 == 5 | df_origin1$B1M7 == 5] <- 5  # "Yoga"
CleanData2$act_exercice[df_origin1$B1M1 == 6 | df_origin1$B1M2 == 6 | df_origin1$B1M3 == 6 | df_origin1$B1M4 == 6 | df_origin1$B1M5 == 6 | df_origin1$B1M6 == 6 | df_origin1$B1M7 == 6] <- 6  # "Swimming"
CleanData2$act_exercice[df_origin1$B1M1 == 7 | df_origin1$B1M2 == 7 | df_origin1$B1M3 == 7 | df_origin1$B1M4 == 7 | df_origin1$B1M5 == 7 | df_origin1$B1M6 == 7 | df_origin1$B1M7 == 7] <- 7  # "Other exercice"
CleanData2$act_exercice[df_origin1$B1M1 == 8 | df_origin1$B1M2 == 8 | df_origin1$B1M3 == 8 | df_origin1$B1M4 == 8 | df_origin1$B1M5 == 8 | df_origin1$B1M6 == 8 | df_origin1$B1M7 == 8] <- 8  # "No exercice"
table(CleanData2$act_exercice)

CleanData2$act_Gym <- 0
CleanData2$act_Gym[df_origin1$B1M1 == 1 |
                     df_origin1$B1M2 == 1 | 
                     df_origin1$B1M3 == 1 | 
                     df_origin1$B1M4 == 1 | 
                     df_origin1$B1M5 == 1 | 
                     df_origin1$B1M6 == 1 | 
                     df_origin1$B1M7 == 1] <- 1
table(CleanData2$act_Gym)


CleanData2$act_TeamSport <- 0
CleanData2$act_TeamSport[df_origin1$B1M1 == 2 |
                           df_origin1$B1M2 == 2 | 
                           df_origin1$B1M3 == 2 | 
                           df_origin1$B1M4 == 2 | 
                           df_origin1$B1M5 == 2 | 
                           df_origin1$B1M6 == 2 | 
                           df_origin1$B1M7 == 2] <- 1
table(CleanData2$act_TeamSport)

CleanData2$act_Walk <- 0
CleanData2$act_Walk[df_origin1$B1M1 == 3 |
                      df_origin1$B1M2 == 3 | 
                      df_origin1$B1M3 == 3 | 
                      df_origin1$B1M4 == 3 | 
                      df_origin1$B1M5 == 3 | 
                      df_origin1$B1M6 == 3 | 
                      df_origin1$B1M7 == 3] <- 1
table(CleanData2$act_Walk)


CleanData2$act_Run <- 0
CleanData2$act_Run[df_origin1$B1M1 == 4 |
                     df_origin1$B1M2 == 4 | 
                     df_origin1$B1M3 == 4 | 
                     df_origin1$B1M4 == 4 | 
                     df_origin1$B1M5 == 4 | 
                     df_origin1$B1M6 == 4 | 
                     df_origin1$B1M7 == 4] <- 1
table(CleanData2$act_Run)

CleanData2$act_Yoga <- 0
CleanData2$act_Yoga[df_origin1$B1M1 == 5 |
                      df_origin1$B1M2 == 5 | 
                      df_origin1$B1M3 == 5 | 
                      df_origin1$B1M4 == 5 | 
                      df_origin1$B1M5 == 5 | 
                      df_origin1$B1M6 == 5 | 
                      df_origin1$B1M7 == 5] <- 1
table(CleanData2$act_Yoga)

CleanData2$act_Swimming <- 0
CleanData2$act_Swimming[df_origin1$B1M1 == 6 |
                          df_origin1$B1M2 == 6 | 
                          df_origin1$B1M3 == 6 | 
                          df_origin1$B1M4 == 6 | 
                          df_origin1$B1M5 == 6 | 
                          df_origin1$B1M6 == 6 | 
                          df_origin1$B1M7 == 6] <- 1
table(CleanData2$act_Swimming)

CleanData2$act_Other <- 0
CleanData2$act_Other[df_origin1$B1M1 == 7 |
                       df_origin1$B1M2 == 7 | 
                       df_origin1$B1M3 == 7 | 
                       df_origin1$B1M4 == 7 | 
                       df_origin1$B1M5 == 7 | 
                       df_origin1$B1M6 == 7 | 
                       df_origin1$B1M7 == 7] <- 1
table(CleanData2$act_Other)

CleanData2$act_None <- 0
CleanData2$act_None[df_origin1$B1M1 == 8 |
                      df_origin1$B1M2 == 8 | 
                      df_origin1$B1M3 == 8 | 
                      df_origin1$B1M4 == 8 | 
                      df_origin1$B1M5 == 8 | 
                      df_origin1$B1M6 == 8 | 
                      df_origin1$B1M7 == 8] <- 1
table(CleanData2$act_None)



## How often do you engage in the following? ... 1) Never; 5) Very often

CleanData2$act_Fishing <- 0
CleanData2$act_Fishing <- minmaxNormalization(df_origin1$B2_A1)
table(CleanData2$act_Fishing)

CleanData2$act_Fishing2 <- NA
CleanData2$act_Fishing2[df_origin1$B2_A1 == 1] <- 1 #"Never"
CleanData2$act_Fishing2[df_origin1$B2_A1 == 2] <- 2 
CleanData2$act_Fishing2[df_origin1$B2_A1 == 3] <- 3 
CleanData2$act_Fishing2[df_origin1$B2_A1 == 4] <- 4 
CleanData2$act_Fishing2[df_origin1$B2_A1 == 5] <- 5 #"Very often"
table(CleanData2$act_Fishing2)

CleanData2$act_Hunting <- 0
CleanData2$act_Hunting <- minmaxNormalization(df_origin1$B2_A2)
table(CleanData2$act_Hunting)

CleanData2$act_Hunting2 <- NA
CleanData2$act_Hunting2[df_origin1$B2_A2 == 1] <- 1 #"Never"
CleanData2$act_Hunting2[df_origin1$B2_A2 == 2] <- 2 
CleanData2$act_Hunting2[df_origin1$B2_A2 == 3] <- 3 
CleanData2$act_Hunting2[df_origin1$B2_A2 == 4] <- 4 
CleanData2$act_Hunting2[df_origin1$B2_A2 == 5] <- 5 #"Very often"
table(CleanData2$act_Hunting2)

CleanData2$act_WinterBoard <- 0
CleanData2$act_WinterBoard <- minmaxNormalization(df_origin1$B2_A3)
table(CleanData2$act_WinterBoard)

CleanData2$act_WinterBoard2 <- NA
CleanData2$act_WinterBoard2[df_origin1$B2_A3 == 1] <- 1 #"Never"
CleanData2$act_WinterBoard2[df_origin1$B2_A3 == 2] <- 2 
CleanData2$act_WinterBoard2[df_origin1$B2_A3 == 3] <- 3 
CleanData2$act_WinterBoard2[df_origin1$B2_A3 == 4] <- 4 
CleanData2$act_WinterBoard2[df_origin1$B2_A3 == 5] <- 5 #"Very often"
table(CleanData2$act_WinterBoard2)
# 
CleanData2$act_DoingTeamSport <- 0
CleanData2$act_DoingTeamSport <- minmaxNormalization(df_origin1$B2_A4)
table(CleanData2$act_DoingTeamSport)

CleanData2$act_DoingTeamSport2 <- NA
CleanData2$act_DoingTeamSport2[df_origin1$B2_A4 == 1] <- 1 #"Never"
CleanData2$act_DoingTeamSport2[df_origin1$B2_A4 == 2] <- 2 
CleanData2$act_DoingTeamSport2[df_origin1$B2_A4 == 3] <- 3 
CleanData2$act_DoingTeamSport2[df_origin1$B2_A4 == 4] <- 4 
CleanData2$act_DoingTeamSport2[df_origin1$B2_A4 == 5] <- 5 #"Very often"
table(CleanData2$act_DoingTeamSport2)

CleanData2$act_VisitsMuseumsGaleries <- 0
CleanData2$act_VisitsMuseumsGaleries <- minmaxNormalization(df_origin1$B2_A5)
table(CleanData2$act_VisitsMuseumsGaleries)

CleanData2$act_VisitsMuseumsGaleries2 <- NA
CleanData2$act_VisitsMuseumsGaleries2[df_origin1$B2_A5 == 1] <- 1 #"Never"
CleanData2$act_VisitsMuseumsGaleries2[df_origin1$B2_A5 == 2] <- 2 
CleanData2$act_VisitsMuseumsGaleries2[df_origin1$B2_A5 == 3] <- 3 
CleanData2$act_VisitsMuseumsGaleries2[df_origin1$B2_A5 == 4] <- 4 
CleanData2$act_VisitsMuseumsGaleries2[df_origin1$B2_A5 == 5] <- 5 #"Very often"
table(CleanData2$act_VisitMuseumsGaleries2)

CleanData2$act_PerformingArts <- 0
CleanData2$act_PerformingArts <- minmaxNormalization(df_origin1$B2_A6)
table(CleanData2$act_PerformingArts)

CleanData2$act_PerformingArts2 <- NA
CleanData2$act_PerformingArts2[df_origin1$B2_A6 == 1] <- 1 #"Never"
CleanData2$act_PerformingArts2[df_origin1$B2_A6 == 2] <- 2 
CleanData2$act_PerformingArts2[df_origin1$B2_A6 == 3] <- 3 
CleanData2$act_PerformingArts2[df_origin1$B2_A6 == 4] <- 4 
CleanData2$act_PerformingArts2[df_origin1$B2_A6 == 5] <- 5 #"Very often"
table(CleanData2$act_PerformingArts2)

CleanData2$act_PartiesAndSocial <- 0
CleanData2$act_PartiesAndSocial <- minmaxNormalization(df_origin1$B2_A7)
table(CleanData2$act_PartiesAndSocial)

CleanData2$act_PartiesAndSocial2 <- NA
CleanData2$act_PartiesAndSocial2[df_origin1$B2_A7 == 1] <- 1 #"Never"
CleanData2$act_PartiesAndSocial2[df_origin1$B2_A7 == 2] <- 2 
CleanData2$act_PartiesAndSocial2[df_origin1$B2_A7 == 3] <- 3 
CleanData2$act_PartiesAndSocial2[df_origin1$B2_A7 == 4] <- 4 
CleanData2$act_PartiesAndSocial2[df_origin1$B2_A7 == 5] <- 5 #"Very often"
table(CleanData2$act_PartiesAndSocial2)

CleanData2$act_ManualTasks <- 0
CleanData2$act_ManualTasks <- minmaxNormalization(df_origin1$B2_A8)
table(CleanData2$act_ManualTasks)

CleanData2$act_ManualTasks2 <- NA
CleanData2$act_ManualTasks2[df_origin1$B2_A8 == 1] <- 1 #"Never"
CleanData2$act_ManualTasks2[df_origin1$B2_A8 == 2] <- 2 
CleanData2$act_ManualTasks2[df_origin1$B2_A8 == 3] <- 3 
CleanData2$act_ManualTasks2[df_origin1$B2_A8 == 4] <- 4 
CleanData2$act_ManualTasks2[df_origin1$B2_A8 == 5] <- 5 #"Very often"
table(CleanData2$act_ManualTasks2)

CleanData2$act_MotorizedOutdoorActivities <- 0
CleanData2$act_MotorizedOutdoorActivities <- minmaxNormalization(df_origin1$B2_A9)
table(CleanData2$act_MotorizedOutdoorActivities)

CleanData2$act_MotorizedOutdoorActivities2 <- NA
CleanData2$act_MotorizedOutdoorActivities2[df_origin1$B2_A9 == 1] <- 1 #"Never"
CleanData2$act_MotorizedOutdoorActivities2[df_origin1$B2_A9 == 2] <- 2 
CleanData2$act_MotorizedOutdoorActivities2[df_origin1$B2_A9 == 3] <- 3 
CleanData2$act_MotorizedOutdoorActivities2[df_origin1$B2_A9 == 4] <- 4 
CleanData2$act_MotorizedOutdoorActivities2[df_origin1$B2_A9 == 5] <- 5 #"Very often"
table(CleanData2$act_MotorizedOutdoorActivities2)

CleanData2$act_Outdoors <- 0
CleanData2$act_Outdoors <- minmaxNormalization(df_origin1$B2_A10)
table(CleanData2$act_Outdoors)

CleanData2$act_Outdoors2 <- NA
CleanData2$act_Outdoors2[df_origin1$B2_A10 == 1] <- 1 #"Never"
CleanData2$act_Outdoors2[df_origin1$B2_A10 == 2] <- 2 
CleanData2$act_Outdoors2[df_origin1$B2_A10 == 3] <- 3 
CleanData2$act_Outdoors2[df_origin1$B2_A10 == 4] <- 4 
CleanData2$act_Outdoors2[df_origin1$B2_A10 == 5] <- 5 #"Very often"
table(CleanData2$act_Outdoors2)

CleanData2$act_Volunteering <- 0
CleanData2$act_Volunteering <- minmaxNormalization(df_origin1$B2_A11)
table(CleanData2$act_Volunteering)

CleanData2$act_Volunteering2 <- NA
CleanData2$act_Volunteering2[df_origin1$B2_A11 == 1] <- 1 #"Never"
CleanData2$act_Volunteering2[df_origin1$B2_A11 == 2] <- 2 
CleanData2$act_Volunteering2[df_origin1$B2_A11 == 3] <- 3 
CleanData2$act_Volunteering2[df_origin1$B2_A11 == 4] <- 4 
CleanData2$act_Volunteering2[df_origin1$B2_A11 == 5] <- 5 #"Very often"
table(CleanData2$act_Volunteering2)

CleanData2$act_Arts <- 0
CleanData2$act_Arts <- minmaxNormalization(df_origin1$B2_A12)
table(CleanData2$act_Arts)

CleanData2$act_Arts2 <- NA
CleanData2$act_Arts2[df_origin1$B2_A12 == 1] <- 1 #"Never"
CleanData2$act_Arts2[df_origin1$B2_A12 == 2] <- 2 
CleanData2$act_Arts2[df_origin1$B2_A12 == 3] <- 3 
CleanData2$act_Arts2[df_origin1$B2_A12 == 4] <- 4 
CleanData2$act_Arts2[df_origin1$B2_A12 == 5] <- 5 #"Very often"
table(CleanData2$act_Arts2)

CleanData2$act_Worship <- 0
CleanData2$act_Worship <- minmaxNormalization(df_origin1$B2_A13)
table(CleanData2$act_Worship)

CleanData2$act_Worship2 <- NA
CleanData2$act_Worship2[df_origin1$B2_A13 == 1] <- 1 #"Never"
CleanData2$act_Worship2[df_origin1$B2_A13 == 2] <- 2 
CleanData2$act_Worship2[df_origin1$B2_A13 == 3] <- 3 
CleanData2$act_Worship2[df_origin1$B2_A13 == 4] <- 4 
CleanData2$act_Worship2[df_origin1$B2_A13 == 5] <- 5 #"Very often"
table(CleanData2$act_Worship2)

CleanData2$act_DoingYoga <- 0
CleanData2$act_DoingYoga <- minmaxNormalization(df_origin1$B2_A14)
table(CleanData2$act_DoingYoga)

CleanData2$act_DoingYoga2 <- NA
CleanData2$act_DoingYoga2[df_origin1$B2_A14 == 1] <- 1 #"Never"
CleanData2$act_DoingYoga2[df_origin1$B2_A14 == 2] <- 2 
CleanData2$act_DoingYoga2[df_origin1$B2_A14 == 3] <- 3 
CleanData2$act_DoingYoga2[df_origin1$B2_A14 == 4] <- 4 
CleanData2$act_DoingYoga2[df_origin1$B2_A14 == 5] <- 5 #"Very often"
table(CleanData2$act_DoingYoga2)

CleanData2$act_Travel <- 0
CleanData2$act_Travel <- minmaxNormalization(df_origin1$B2_A15)
table(CleanData2$act_Travel)

CleanData2$act_Travel2 <- NA
CleanData2$act_Travel2[df_origin1$B2_A15 == 1] <- 1 #"Never"
CleanData2$act_Travel2[df_origin1$B2_A15 == 2] <- 2 
CleanData2$act_Travel2[df_origin1$B2_A15 == 3] <- 3 
CleanData2$act_Travel2[df_origin1$B2_A15 == 4] <- 4 
CleanData2$act_Travel2[df_origin1$B2_A15 == 5] <- 5 #"Very often"
table(CleanData2$act_Travel2)

CleanData2$act_VideoGames <- 0
CleanData2$act_VideoGames <- minmaxNormalization(df_origin1$B2_A16)
table(CleanData2$act_VideoGames)

CleanData2$act_VideoGames2 <- NA
CleanData2$act_VideoGames2[df_origin1$B2_A16 == 1] <- 1 #"Never"
CleanData2$act_VideoGames2[df_origin1$B2_A16 == 2] <- 2 
CleanData2$act_VideoGames2[df_origin1$B2_A16 == 3] <- 3 
CleanData2$act_VideoGames2[df_origin1$B2_A16 == 4] <- 4 
CleanData2$act_VideoGames2[df_origin1$B2_A16 == 5] <- 5 #"Very often"
table(CleanData2$act_VideoGames2)

CleanData2$act_Books <- 0
CleanData2$act_Books <- minmaxNormalization(df_origin1$B2_A17)
table(CleanData2$act_Books)

CleanData2$act_Books2 <- NA
CleanData2$act_Books2[df_origin1$B2_A17 == 1] <- 1 #"Never"
CleanData2$act_Books2[df_origin1$B2_A17 == 2] <- 2 
CleanData2$act_Books2[df_origin1$B2_A17 == 3] <- 3 
CleanData2$act_Books2[df_origin1$B2_A17 == 4] <- 4 
CleanData2$act_Books2[df_origin1$B2_A17 == 5] <- 5 #"Very often"
table(CleanData2$act_Books2)


# Problème avec la variable B2B 
# Question Activities	Activities	
# À quel jeu vidéo jouez-vous le plus souvent? 	
# Which video game do you play most often? 	
# Question ouverte (base de données)	
# Open question (data base)
# Toutes les valeurs sont "96"
# Toutes *SAUF 8* valeurs de B2BO sont NA, les autres sont codées


#### Justine ####

## Quel serait votre moment de vacances favoris parmi les suivants?
table(Data$B3)
CleanData$act_holidays <- NA
CleanData$act_holidays[Data$B3 == 1] <- 1 #"Profiter de la plage"
CleanData$act_holidays[Data$B3 == 2] <- 2 #"Être en nature"
CleanData$act_holidays[Data$B3 == 3] <- 3 #"Faire du sport ou des activités"
CleanData$act_holidays[Data$B3 == 4] <- 4 #"Visiter une ville"
CleanData$act_holidays[Data$B3 == 5] <- 5 #"Découvrir une culture différente de la mienne"
CleanData$act_holidays[Data$B3 == 6] <- 6 #"Faire une croisière"
CleanData$act_holidays[Data$B3 == 7] <- 7 #"S'amuser entre amis"
CleanData$act_holidays[Data$B3 == 8] <- 8 #"Se reposer et ne rien faire"
CleanData$act_holidays[Data$B3 == 9] <- 9 #"Autres"
table(CleanData$act_holidays)

CleanData$act_holidays_Beach <- NA
CleanData$act_holidays_Beach[CleanData$act_holidays == 1] <- 1
CleanData$act_holidays_Beach[CleanData$act_holidays != 1] <- 0
table(CleanData$act_holidays_Beach)

CleanData$act_holidays_Nature <- NA
CleanData$act_holidays_Nature[CleanData$act_holidays == 2] <- 1
CleanData$act_holidays_Nature[CleanData$act_holidays != 2] <- 0
table(CleanData$act_holidays_Nature)

CleanData$act_holidays_Sport <- NA
CleanData$act_holidays_Sport[CleanData$act_holidays == 3] <- 1
CleanData$act_holidays_Sport[CleanData$act_holidays != 3] <- 0
table(CleanData$act_holidays_Sport)

CleanData$act_holidays_City <- NA
CleanData$act_holidays_City[CleanData$act_holidays == 4] <- 1
CleanData$act_holidays_City[CleanData$act_holidays != 4] <- 0
table(CleanData$act_holidays_City)

CleanData$act_holidays_Culture <- NA
CleanData$act_holidays_Culture[CleanData$act_holidays == 5] <- 1
CleanData$act_holidays_Culture[CleanData$act_holidays != 5] <- 0
table(CleanData$act_holidays_Culture)

CleanData$act_holidays_Cruise <- NA
CleanData$act_holidays_Cruise[CleanData$act_holidays == 6] <- 1
CleanData$act_holidays_Cruise[CleanData$act_holidays != 6] <- 0
table(CleanData$act_holidays_Cruise)

CleanData$act_holidays_Friends <- NA
CleanData$act_holidays_Friends[CleanData$act_holidays == 7] <- 1
CleanData$act_holidays_Friends[CleanData$act_holidays != 7] <- 0
table(CleanData$act_holidays_Friends)

CleanData$act_holidays_Relax <- NA
CleanData$act_holidays_Relax[CleanData$act_holidays == 8] <- 1
CleanData$act_holidays_Relax[CleanData$act_holidays != 8] <- 0
table(CleanData$act_holidays_Relax)

CleanData$act_holidays_Other <- NA
CleanData$act_holidays_Other[CleanData$act_holidays == 9] <- 1
CleanData$act_holidays_Other[CleanData$act_holidays != 9] <- 0
table(CleanData$act_holidays_Other)


## Sur quels types de sujet lisez-vous généralement un livre ou une revue?

CleanData$act_subject_reading <-NA 
CleanData$act_subject_reading[Data$B5M1 == 1 | Data$B5M2 == 1 | Data$B5M3 == 1 | Data$B5M4 == 1 | Data$B5M5 == 1 | Data$B5M6 == 1 | Data$B5M7 == 1 | Data$B5M8 == 1] <- 1    # "Sports magazines"
CleanData$act_subject_reading[Data$B5M1 == 2 | Data$B5M2 == 2 | Data$B5M3 == 2 | Data$B5M4 == 2 | Data$B5M5 == 2 | Data$B5M6 == 2 | Data$B5M7 == 2 | Data$B5M8 == 2] <- 2    # "Politics and public affairs magazines"
CleanData$act_subject_reading[Data$B5M1 == 3 | Data$B5M2 == 3 | Data$B5M3 == 3 | Data$B5M4 == 3 | Data$B5M5 == 3 | Data$B5M6 == 3 | Data$B5M7 == 3 | Data$B5M8 == 3] <- 3    # "Travels magazines"
CleanData$act_subject_reading[Data$B5M1 == 4 | Data$B5M2 == 4 | Data$B5M3 == 4 | Data$B5M4 == 4 | Data$B5M5 == 4 | Data$B5M6 == 4 | Data$B5M7 == 4 | Data$B5M8 == 4] <- 4    # "Arts and decoration magazines"
CleanData$act_subject_reading[Data$B5M1 == 5 | Data$B5M2 == 5 | Data$B5M3 == 5 | Data$B5M4 == 5 | Data$B5M5 == 5 | Data$B5M6 == 5 | Data$B5M7 == 5 | Data$B5M8 == 5] <- 5    # "Cooking magazines"
CleanData$act_subject_reading[Data$B5M1 == 6 | Data$B5M2 == 6 | Data$B5M3 == 6 | Data$B5M4 == 6 | Data$B5M5 == 6 | Data$B5M6 == 6 | Data$B5M7 == 6 | Data$B5M8 == 6] <- 6    # "Lifestyle magazines"
CleanData$act_subject_reading[Data$B5M1 == 7 | Data$B5M2 == 7 | Data$B5M3 == 7 | Data$B5M4 == 7 | Data$B5M5 == 7 | Data$B5M6 == 7 | Data$B5M7 == 7 | Data$B5M8 == 7] <- 7    # "Photography magazines"
CleanData$act_subject_reading[Data$B5M1 == 8 | Data$B5M2 == 8 | Data$B5M3 == 8 | Data$B5M4 == 8 | Data$B5M5 == 8 | Data$B5M6 == 8 | Data$B5M7 == 8 | Data$B5M8 == 8] <- 8    # "Fashion magazines"
CleanData$act_subject_reading[Data$B5M1 == 9 | Data$B5M2 == 9 | Data$B5M3 == 9 | Data$B5M4 == 9 | Data$B5M5 == 9 | Data$B5M6 == 9 | Data$B5M7 == 9 | Data$B5M8 == 9] <- 9    # "Other suject magazines"
CleanData$act_subject_reading[Data$B5M1 == 10] <- 10 # "No magazine"
table(CleanData$act_subject_reading)

# 
# #### 1) Sport;
# 
# CleanData$act_reading_Sport1 <- NA
# CleanData$act_reading_Sport1[Data$B5M1==1]<-1
# CleanData$act_reading_Sport1[Data$B5M1!=1 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Sport1)
# 
# CleanData$act_reading_Sport2 <- NA
# CleanData$act_reading_Sport2[Data$B5M2==1]<-1
# CleanData$act_reading_Sport2[Data$B5M2!=1 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Sport2)
# 
# CleanData$act_reading_Sport3 <- NA
# CleanData$act_reading_Sport3[Data$B5M3==1]<-1
# CleanData$act_reading_Sport3[Data$B5M3!=1 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Sport3)
# 
# CleanData$act_reading_Sport4 <- NA
# CleanData$act_reading_Sport4[Data$B5M4==1]<-1
# CleanData$act_reading_Sport4[Data$B5M4!=1 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Sport4)
# 
# CleanData$act_reading_Sport5 <- NA
# CleanData$act_reading_Sport5[Data$B5M5==1]<-1
# CleanData$act_reading_Sport5[Data$B5M5!=1 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Sport5)
# 
# CleanData$act_reading_Sport6 <- NA
# CleanData$act_reading_Sport6[Data$B5M6==1]<-1
# CleanData$act_reading_Sport6[Data$B5M6!=1 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Sport6)
# 
# CleanData$act_reading_Sport7 <- NA
# CleanData$act_reading_Sport7[Data$B5M7==1]<-1
# CleanData$act_reading_Sport7[Data$B5M7!=1 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Sport7)
# 
# CleanData$act_reading_Sport8 <- NA
# CleanData$act_reading_Sport8[Data$B5M8==1]<-1
# CleanData$act_reading_Sport8[Data$B5M8!=1 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Sport8)
# 
# # CleanData$act_reading_Sport9 <- NA
# # CleanData$act_reading_Sport9[Data$B5M9==1]<-1
# # CleanData$act_reading_Sport9[Data$B5M9!=1 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Sport9)
# 
# #### 2) Affaires publiques et politique;
# 
# CleanData$act_reading_Politics1 <- NA
# CleanData$act_reading_Politics1[Data$B5M1==2]<-1
# CleanData$act_reading_Politics1[Data$B5M1!=2 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Politics1)
# 
# CleanData$act_reading_Politics2 <- NA
# CleanData$act_reading_Politics2[Data$B5M2==2]<-1
# CleanData$act_reading_Politics2[Data$B5M2!=2 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Politics2)
# 
# CleanData$act_reading_Politics3 <- NA
# CleanData$act_reading_Politics3[Data$B5M3==2]<-1
# CleanData$act_reading_Politics3[Data$B5M3!=2 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Politics3)
# 
# CleanData$act_reading_Politics4 <- NA
# CleanData$act_reading_Politics4[Data$B5M4==2]<-1
# CleanData$act_reading_Politics4[Data$B5M4!=2 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Politics4)
# 
# CleanData$act_reading_Politics5 <- NA
# CleanData$act_reading_Politics5[Data$B5M5==2]<-1
# CleanData$act_reading_Politics5[Data$B5M5!=2 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Politics5)
# 
# CleanData$act_reading_Politics6 <- NA
# CleanData$act_reading_Politics6[Data$B5M6==2]<-1
# CleanData$act_reading_Politics6[Data$B5M6!=2 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Politics6)
# 
# CleanData$act_reading_Politics7 <- NA
# CleanData$act_reading_Politics7[Data$B5M7==2]<-1
# CleanData$act_reading_Politics7[Data$B5M7!=2 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Politics7)
# 
# CleanData$act_reading_Politics8 <- NA
# CleanData$act_reading_Politics8[Data$B5M8==2]<-1
# CleanData$act_reading_Politics8[Data$B5M8!=2 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Politics8)
# 
# # CleanData$act_reading_Politics9 <- NA
# # CleanData$act_reading_Politics9[Data$B5M9==2]<-1
# # CleanData$act_reading_Politics9[Data$B5M9!=2 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Politics9)
# 
# #### 3) Voyages;
# 
# CleanData$act_reading_Travels1 <- NA
# CleanData$act_reading_Travels1[Data$B5M1==3]<-1
# CleanData$act_reading_Travels1[Data$B5M1!=3 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Travels1)
# 
# CleanData$act_reading_Travels2 <- NA
# CleanData$act_reading_Travels2[Data$B5M2==3]<-1
# CleanData$act_reading_Travels2[Data$B5M2!=3 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Travels2)
# 
# CleanData$act_reading_Travels3 <- NA
# CleanData$act_reading_Travels3[Data$B5M3==3]<-1
# CleanData$act_reading_Travels3[Data$B5M3!=3 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Travels3)
# 
# CleanData$act_reading_Travels4 <- NA
# CleanData$act_reading_Travels4[Data$B5M4==3]<-1
# CleanData$act_reading_Travels4[Data$B5M4!=3 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Travels4)
# 
# CleanData$act_reading_Travels5 <- NA
# CleanData$act_reading_Travels5[Data$B5M5==3]<-1
# CleanData$act_reading_Travels5[Data$B5M5!=3 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Travels5)
# 
# CleanData$act_reading_Travels6 <- NA
# CleanData$act_reading_Travels6[Data$B5M6==3]<-1
# CleanData$act_reading_Travels6[Data$B5M6!=3 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Politics6)
# 
# CleanData$act_reading_Travels7 <- NA
# CleanData$act_reading_Travels7[Data$B5M7==3]<-1
# CleanData$act_reading_Travels7[Data$B5M7!=3 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Travels7)
# 
# CleanData$act_reading_Travels8 <- NA
# CleanData$act_reading_Travels8[Data$B5M8==3]<-1
# CleanData$act_reading_Travels8[Data$B5M8!=3 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Travels8)
# 
# # CleanData$act_reading_Travels9 <- NA
# # CleanData$act_reading_Travels9[Data$B5M9==3]<-1
# # CleanData$act_reading_Travels9[Data$B5M9!=3 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Travels9)
# 
# #### 4) Arts, décoration et bricolage;
# 
# CleanData$act_reading_Arts1 <- NA
# CleanData$act_reading_Arts1[Data$B5M1==4]<-1
# CleanData$act_reading_Arts1[Data$B5M1!=4 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Arts1)
# 
# CleanData$act_reading_Arts2 <- NA
# CleanData$act_reading_Arts2[Data$B5M2==4]<-1
# CleanData$act_reading_Arts2[Data$B5M2!=4 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Arts2)
# 
# CleanData$act_reading_Arts3 <- NA
# CleanData$act_reading_Arts3[Data$B5M3==4]<-1
# CleanData$act_reading_Arts3[Data$B5M3!=4 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Arts3)
# 
# CleanData$act_reading_Arts4 <- NA
# CleanData$act_reading_Arts4[Data$B5M4==4]<-1
# CleanData$act_reading_Arts4[Data$B5M4!=4 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Arts4)
# 
# CleanData$act_reading_Arts5 <- NA
# CleanData$act_reading_Arts5[Data$B5M5==4]<-1
# CleanData$act_reading_Arts5[Data$B5M5!=4 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Arts5)
# 
# CleanData$act_reading_Arts6 <- NA
# CleanData$act_reading_Arts6[Data$B5M6==4]<-1
# CleanData$act_reading_Arts6[Data$B5M6!=4 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Arts6)
# 
# CleanData$act_reading_Arts7 <- NA
# CleanData$act_reading_Arts7[Data$B5M7==4]<-1
# CleanData$act_reading_Arts7[Data$B5M7!=4 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Arts7)
# 
# CleanData$act_reading_Arts8 <- NA
# CleanData$act_reading_Arts8[Data$B5M8==4]<-1
# CleanData$act_reading_Arts8[Data$B5M8!=4 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Arts8)
# 
# # CleanData$act_reading_Arts9 <- NA
# # CleanData$act_reading_Arts9[Data$B5M9==4]<-1
# # CleanData$act_reading_Arts9[Data$B5M9!=4 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Arts9)
# 
# #### 5) Cuisine et recettes;
# 
# CleanData$act_reading_Cooking1 <- NA
# CleanData$act_reading_Cooking1[Data$B5M1==5]<-1
# CleanData$act_reading_Cooking1[Data$B5M1!=5 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Cooking1)
# 
# CleanData$act_reading_Cooking2 <- NA
# CleanData$act_reading_Cooking2[Data$B5M2==5]<-1
# CleanData$act_reading_Cooking2[Data$B5M2!=5 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Cooking2)
# 
# CleanData$act_reading_Cooking3 <- NA
# CleanData$act_reading_Cooking3[Data$B5M3==5]<-1
# CleanData$act_reading_Cooking3[Data$B5M3!=5 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Cooking3)
# 
# CleanData$act_reading_Cooking4 <- NA
# CleanData$act_reading_Cooking4[Data$B5M4==5]<-1
# CleanData$act_reading_Cooking4[Data$B5M4!=5 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Cooking4)
# 
# CleanData$act_reading_Cooking5 <- NA
# CleanData$act_reading_Cooking5[Data$B5M5==5]<-1
# CleanData$act_reading_Cooking5[Data$B5M5!=5 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Cooking5)
# 
# CleanData$act_reading_Cooking6 <- NA
# CleanData$act_reading_Cooking6[Data$B5M6==5]<-1
# CleanData$act_reading_Cooking6[Data$B5M6!=5 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Cooking6)
# 
# CleanData$act_reading_Cooking7 <- NA
# CleanData$act_reading_Cooking7[Data$B5M7==5]<-1
# CleanData$act_reading_Cooking7[Data$B5M7!=5 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Cooking7)
# 
# CleanData$act_reading_Cooking8 <- NA
# CleanData$act_reading_Cooking8[Data$B5M8==5]<-1
# CleanData$act_reading_Cooking8[Data$B5M8!=5 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Cooking8)
# 
# # CleanData$act_reading_Cooking9 <- NA
# # CleanData$act_reading_Cooking9[Data$B5M9==5]<-1
# # CleanData$act_reading_Cooking9[Data$B5M9!=5 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Cooking9)
# 
# #### 6) Lifestyle;
# 
# CleanData$act_reading_Lifestyle1 <- NA
# CleanData$act_reading_Lifestyle1[Data$B5M1==6]<-1
# CleanData$act_reading_Lifestyle1[Data$B5M1!=6 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Lifestyle1)
# 
# CleanData$act_reading_Lifestyle2 <- NA
# CleanData$act_reading_Lifestyle2[Data$B5M2==6]<-1
# CleanData$act_reading_Lifestyle2[Data$B5M2!=6 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Lifestyle2)
# 
# CleanData$act_reading_Lifestyle3 <- NA
# CleanData$act_reading_Lifestyle3[Data$B5M3==6]<-1
# CleanData$act_reading_Lifestyle3[Data$B5M3!=6 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Lifestyle3)
# 
# CleanData$act_reading_Lifestyle4 <- NA
# CleanData$act_reading_Lifestyle4[Data$B5M4==6]<-1
# CleanData$act_reading_Lifestyle4[Data$B5M4!=6 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Lifestyle4)
# 
# CleanData$act_reading_Lifestyle5 <- NA
# CleanData$act_reading_Lifestyle5[Data$B5M5==6]<-1
# CleanData$act_reading_Lifestyle5[Data$B5M5!=6 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Lifestyle5)
# 
# CleanData$act_reading_Lifestyle6 <- NA
# CleanData$act_reading_Lifestyle6[Data$B5M6==6]<-1
# CleanData$act_reading_Lifestyle6[Data$B5M6!=6 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Lifestyle6)
# 
# CleanData$act_reading_Lifestyle7 <- NA
# CleanData$act_reading_Lifestyle7[Data$B5M7==6]<-1
# CleanData$act_reading_Lifestyle7[Data$B5M7!=6 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Lifestyle7)
# 
# CleanData$act_reading_Lifestyle8 <- NA
# CleanData$act_reading_Lifestyle8[Data$B5M8==6]<-1
# CleanData$act_reading_Lifestyle8[Data$B5M8!=6 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Lifestyle8)
# 
# # CleanData$act_reading_Lifestyle9 <- NA
# # CleanData$act_reading_Lifestyle9[Data$B5M9==6]<-1
# # CleanData$act_reading_Lifestyle9[Data$B5M9!=6 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Lifestyle9)
# 
# #### 7) Photographie;
# 
# CleanData$act_reading_Photography1 <- NA
# CleanData$act_reading_Photography1[Data$B5M1==7]<-1
# CleanData$act_reading_Photography1[Data$B5M1!=7 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Photography1)
# 
# CleanData$act_reading_Photography2 <- NA
# CleanData$act_reading_Photography2[Data$B5M2==7]<-1
# CleanData$act_reading_Photography2[Data$B5M2!=7 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Photography2)
# 
# CleanData$act_reading_Photography3 <- NA
# CleanData$act_reading_Photography3[Data$B5M3==7]<-1
# CleanData$act_reading_Photography3[Data$B5M3!=7 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Photography3)
# 
# CleanData$act_reading_Photography4 <- NA
# CleanData$act_reading_Photography4[Data$B5M4==7]<-1
# CleanData$act_reading_Photography4[Data$B5M4!=7 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Photography4)
# 
# CleanData$act_reading_Photography5 <- NA
# CleanData$act_reading_Photography5[Data$B5M5==7]<-1
# CleanData$act_reading_Photography5[Data$B5M5!=7 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Photography5)
# 
# CleanData$act_reading_Photography6 <- NA
# CleanData$act_reading_Photography6[Data$B5M6==7]<-1
# CleanData$act_reading_Photography6[Data$B5M6!=7 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Photography6)
# 
# CleanData$act_reading_Photography7 <- NA
# CleanData$act_reading_Photography7[Data$B5M7==7]<-1
# CleanData$act_reading_Photography7[Data$B5M7!=7 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Photography7)
# 
# CleanData$act_reading_Photography8 <- NA
# CleanData$act_reading_Photography8[Data$B5M8==7]<-1
# CleanData$act_reading_Photography8[Data$B5M8!=7 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Photography8)
# 
# # CleanData$act_reading_Photography9 <- NA
# # CleanData$act_reading_Photography9[Data$B5M9==7]<-1
# # CleanData$act_reading_Photography9[Data$B5M9!=7 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Photography9)
# 
# #### 8) Mode;
# 
# CleanData$act_reading_Fashion1 <- NA
# CleanData$act_reading_Fashion1[Data$B5M1==8]<-1
# CleanData$act_reading_Fashion1[Data$B5M1!=8 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Fashion1)
# 
# CleanData$act_reading_Fashion2 <- NA
# CleanData$act_reading_Fashion2[Data$B5M2==8]<-1
# CleanData$act_reading_Fashion2[Data$B5M2!=8 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Fashion2)
# 
# CleanData$act_reading_Fashion3 <- NA
# CleanData$act_reading_Fashion3[Data$B5M3==8]<-1
# CleanData$act_reading_Fashion3[Data$B5M3!=8 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Fashion3)
# 
# CleanData$act_reading_Fashion4 <- NA
# CleanData$act_reading_Fashion4[Data$B5M4==8]<-1
# CleanData$act_reading_Fashion4[Data$B5M4!=8 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Fashion4)
# 
# CleanData$act_reading_Fashion5 <- NA
# CleanData$act_reading_Fashion5[Data$B5M5==8]<-1
# CleanData$act_reading_Fashion5[Data$B5M5!=8 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Fashion5)
# 
# CleanData$act_reading_Fashion6 <- NA
# CleanData$act_reading_Fashion6[Data$B5M6==8]<-1
# CleanData$act_reading_Fashion6[Data$B5M6!=8 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Fashion6)
# 
# CleanData$act_reading_Fashion7 <- NA
# CleanData$act_reading_Fashion7[Data$B5M7==8]<-1
# CleanData$act_reading_Fashion7[Data$B5M7!=8 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Fashion7)
# 
# CleanData$act_reading_Fashion8 <- NA
# CleanData$act_reading_Fashion8[Data$B5M8==8]<-1
# CleanData$act_reading_Fashion8[Data$B5M8!=8 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Fashion8)
# 
# # CleanData$act_reading_Fashion9 <- NA
# # CleanData$act_reading_Fashion9[Data$B5M9==8]<-1
# # CleanData$act_reading_Fashion9[Data$B5M9!=8 & !is.na(Data$B5M9)]<-0
# # table(Data$B5M9)
# # table(CleanData$act_reading_Fashion9)
# 
# #### 9) Autres (veuillez préciser)
# 
# CleanData$act_reading_Other1 <- NA
# CleanData$act_reading_Other1[Data$B5M1==9]<-1
# CleanData$act_reading_Other1[Data$B5M1!=9 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_Other1)
# 
# CleanData$act_reading_Other2 <- NA
# CleanData$act_reading_Other2[Data$B5M2==9]<-1
# CleanData$act_reading_Other2[Data$B5M2!=9 & !is.na(Data$B5M2)]<-0
# table(Data$B5M2)
# table(CleanData$act_reading_Other2)
# 
# CleanData$act_reading_Other3 <- NA
# CleanData$act_reading_Other3[Data$B5M3==9]<-1
# CleanData$act_reading_Other3[Data$B5M3!=9 & !is.na(Data$B5M3)]<-0
# table(Data$B5M3)
# table(CleanData$act_reading_Fashion3)
# 
# CleanData$act_reading_Other4 <- NA
# CleanData$act_reading_Other4[Data$B5M4==9]<-1
# CleanData$act_reading_Other4[Data$B5M4!=9 & !is.na(Data$B5M4)]<-0
# table(Data$B5M4)
# table(CleanData$act_reading_Other4)
# 
# CleanData$act_reading_Other5 <- NA
# CleanData$act_reading_Other5[Data$B5M5==9]<-1
# CleanData$act_reading_Other5[Data$B5M5!=9 & !is.na(Data$B5M5)]<-0
# table(Data$B5M5)
# table(CleanData$act_reading_Other5)
# 
# CleanData$act_reading_Other6 <- NA
# CleanData$act_reading_Other6[Data$B5M6==9]<-1
# CleanData$act_reading_Other6[Data$B5M6!=9 & !is.na(Data$B5M6)]<-0
# table(Data$B5M6)
# table(CleanData$act_reading_Other6)
# 
# CleanData$act_reading_Other7 <- NA
# CleanData$act_reading_Other7[Data$B5M7==9]<-1
# CleanData$act_reading_Other7[Data$B5M7!=9 & !is.na(Data$B5M7)]<-0
# table(Data$B5M7)
# table(CleanData$act_reading_Other7)
# 
# CleanData$act_reading_Other8 <- NA
# CleanData$act_reading_Other8[Data$B5M8==9]<-1
# CleanData$act_reading_Other8[Data$B5M8!=9 & !is.na(Data$B5M8)]<-0
# table(Data$B5M8)
# table(CleanData$act_reading_Other8)

# CleanData$act_reading_Other9 <- NA
# CleanData$act_reading_Other9[Data$B5M9==9]<-1
# CleanData$act_reading_Other9[Data$B5M9!=9 & !is.na(Data$B5M9)]<-0
# table(Data$B5M9)
# table(CleanData$act_reading_Other9)

#### 10) Aucun 

# CleanData$act_reading_None1 <- NA
# CleanData$act_reading_None1[Data$B5M1==10]<-1
# CleanData$act_reading_None1[Data$B5M1!=10 & !is.na(Data$B5M1)]<-0
# table(Data$B5M1)
# table(CleanData$act_reading_None1)

#### Quel type de transport utilisez-vous le plus régulièrement au quotidien? 
table(Data$C1)
CleanData$act_transport <- NA
CleanData$act_transport[Data$C1 == 1] <- 1 #"Voiture"
CleanData$act_transport[Data$C1 == 2] <- 2 #"SUV"
CleanData$act_transport[Data$C1 == 3] <- 3 #"Moto"
CleanData$act_transport[Data$C1 == 4] <- 4 #"Marche à pied"
CleanData$act_transport[Data$C1 == 5] <- 5 #"Vélo"
CleanData$act_transport[Data$C1 == 6] <- 6 #"Bus ou autre transport en commun"
CleanData$act_transport[Data$C1 == 7] <- 7 #"Taxi ou UBER"
table(CleanData$act_transport)

CleanData$act_transport_Car <- NA
CleanData$act_transport_Car[CleanData$act_transport == 1] <- 1
CleanData$act_transport_Car[CleanData$act_transport != 1] <- 0
table(CleanData$act_transport_Car)

CleanData$act_transport_SUV <- NA
CleanData$act_transport_SUV[CleanData$act_transport == 2] <- 1
CleanData$act_transport_SUV[CleanData$act_transport != 2] <- 0
table(CleanData$act_transport_SUV)

CleanData$act_transport_Moto <- NA
CleanData$act_transport_Moto[CleanData$act_transport == 3] <- 1
CleanData$act_transport_Moto[CleanData$act_transport != 3] <- 0
table(CleanData$act_transport_Moto)

CleanData$act_transport_Walk <- NA
CleanData$act_transport_Walk[CleanData$act_transport == 4] <- 1
CleanData$act_transport_Walk[CleanData$act_transport != 4] <- 0
table(CleanData$act_transport_Walk)

CleanData$act_transport_Bicycle <- NA
CleanData$act_transport_Bicycle[CleanData$act_transport == 5] <- 1
CleanData$act_transport_Bicycle[CleanData$act_transport != 5] <- 0
table(CleanData$act_transport_Bicycle)

CleanData$act_transport_PublicTransportation <- NA
CleanData$act_transport_PublicTransportation[CleanData$act_transport == 6] <- 1
CleanData$act_transport_PublicTransportation[CleanData$act_transport != 6] <- 0
table(CleanData$act_transport_PublicTransportation)

CleanData$act_transport_PublicTransportation2 <- NA
CleanData$act_transport_PublicTransportation2[CleanData$act_transport == 6] <- 2
CleanData$act_transport_PublicTransportation2[CleanData$act_transport != 6] <- 1
table(CleanData$act_transport_PublicTransportation2)

CleanData$act_transport_Taxi <- NA
CleanData$act_transport_Taxi[CleanData$act_transport == 7] <- 1
CleanData$act_transport_Taxi[CleanData$act_transport != 7] <- 0
table(CleanData$act_transport_Taxi)

## Quel modèle de voiture utilisez-vous le plus régulièrement? 

# table(Data$C2) # Question ouverte

## Diriez-vous que votre choix de moyen de transport fait partie de qui vous êtes, 
# ou est simplement la façon la plus efficace de vous rendre du point A au point B?

table(Data$C3)
CleanData$act_transportWhoIAm<-NA
CleanData$act_transportWhoIAm[Data$C3==1] <- 1
CleanData$act_transportWhoIAm[Data$C3==2] <- 0
table(CleanData$act_transportWhoIAm)

CleanData$act_transportAtoB<-NA
CleanData$act_transportAtoB[Data$C3==2] <- 1
CleanData$act_transportAtoB[Data$C3==1] <- 0
table(CleanData$act_transportAtoB)

## Parmi les catégories suivantes, laquelle décrit/décrivait le mieux votre domaine d'emploi (avant la pandémie du coronavirus)? 

table(Data$D2)
CleanData$act_occupField <- NA
CleanData$act_occupField[Data$D2 == 1] <- 1     # "Gestion"
CleanData$act_occupField[Data$D2 == 2] <- 2     # "Affaires, finance et administration"
CleanData$act_occupField[Data$D2 == 3] <- 3     # "Sciences naturelles et appliquées et domaines apparentés"
CleanData$act_occupField[Data$D2 == 4] <- 4     # "Secteur de la santé"
CleanData$act_occupField[Data$D2 == 5] <- 5     # "Enseignement, droit et services sociaux, communautaires et gouvernementaux"
CleanData$act_occupField[Data$D2 == 6] <- 6     # "Arts, culture, sport et loisirs"
CleanData$act_occupField[Data$D2 == 7] <- 7     # "Vente et services"
CleanData$act_occupField[Data$D2 == 8] <- 8     # "Métiers, transport, machinerie et domaines apparentés"
CleanData$act_occupField[Data$D2 == 9] <- 9     # "Ressources naturelles, agriculture et production connexe"
CleanData$act_occupField[Data$D2 == 10] <- 10    #  "Fabrication et services d'utilité publique"
CleanData$act_occupField[Data$D2 == 11] <- 11   #  "Autres"
table(CleanData$act_occupField)

CleanData$act_occupField_Management <- NA
CleanData$act_occupField_Management[CleanData$act_occupField == 1] <- 1
CleanData$act_occupField_Management[CleanData$act_occupField != 1] <- 0
table(CleanData$act_occupField_Management)

CleanData$act_occupField_Business <- NA
CleanData$act_occupField_Business[CleanData$act_occupField == 2] <- 1
CleanData$act_occupField_Business[CleanData$act_occupField != 2] <- 0
table(CleanData$act_occupField_Business)

CleanData$act_occupField_Sciences <- NA
CleanData$act_occupField_Sciences[CleanData$act_occupField == 3] <- 1
CleanData$act_occupField_Sciences[CleanData$act_occupField != 3] <- 0
table(CleanData$act_occupField_Sciences)

CleanData$act_occupField_Health <- NA
CleanData$act_occupField_Health[CleanData$act_occupField == 4] <- 1
CleanData$act_occupField_Health[CleanData$act_occupField != 4] <- 0
table(CleanData$act_occupField_Health)

CleanData$act_occupField_Social <- NA
CleanData$act_occupField_Social[CleanData$act_occupField == 5] <- 1
CleanData$act_occupField_Social[CleanData$act_occupField != 5] <- 0
table(CleanData$act_occupField_Social)

CleanData$act_occupField_Recreation <- NA
CleanData$act_occupField_Recreation[CleanData$act_occupField == 6] <- 1
CleanData$act_occupField_Recreation[CleanData$act_occupField != 6] <- 0
table(CleanData$act_occupField_Recreation)

CleanData$act_occupField_Sales <- NA
CleanData$act_occupField_Sales[CleanData$act_occupField == 7] <- 1
CleanData$act_occupField_Sales[CleanData$act_occupField != 7] <- 0
table(CleanData$act_occupField_Sales)

CleanData$act_occupField_TranspMachinery <- NA
CleanData$act_occupField_TranspMachinery[CleanData$act_occupField == 8] <- 1
CleanData$act_occupField_TranspMachinery[CleanData$act_occupField != 8] <- 0
table(CleanData$act_occupField_TranspMachinery)

CleanData$act_occupField_NaturalRes <- NA
CleanData$act_occupField_NaturalRes[CleanData$act_occupField == 9] <- 1
CleanData$act_occupField_NaturalRes[CleanData$act_occupField != 9] <- 0
table(CleanData$act_occupField_NaturalRes)

CleanData$act_occupField_Manufact <- NA
CleanData$act_occupField_Manufact[CleanData$act_occupField == 10] <- 1
CleanData$act_occupField_Manufact[CleanData$act_occupField != 10] <- 0
table(CleanData$act_occupField_Manufact)

CleanData$act_occupField_Other <- NA
CleanData$act_occupField_Other[CleanData$act_occupField == 11] <- 1
CleanData$act_occupField_Other[CleanData$act_occupField != 11] <- 0
table(CleanData$act_occupField_Other)

## Parmi les catégories suivantes, laquelle décrit/décrivait le mieux votre type de travail (avant la pandémie du coronavirus)? 

table(Data$D3)
CleanData$act_work <- NA
CleanData$act_work[Data$D3 == 1] <- "Emplois de main-d'œuvre (par exemple, travailleur des champs pétroliers, concierge)"
CleanData$act_work[Data$D3 == 2] <- "Emplois intermédiaires (par exemple, boucher industriel, chauffeur de camion long-courrier, serveur)"
CleanData$act_work[Data$D3 == 3] <- "Emplois techniques et/ou métiers spécialisés (par exemple, chef, plombier, électricien)"
CleanData$act_work[Data$D3 == 4] <- "Emplois professionnels (par exemple, médecin, dentiste, architecte)"
CleanData$act_work[Data$D3 == 5] <- "Emplois de gestion"
CleanData$act_work[Data$D3 == 6] <- "Autres"
table(CleanData$act_work)

CleanData$act_work_Labour <- NA
CleanData$act_work_Labour[CleanData$act_work == "Emplois de main-d'œuvre (par exemple, travailleur des champs pétroliers, concierge)"] <- 1
CleanData$act_work_Labour[CleanData$act_work != "Emplois de main-d'œuvre (par exemple, travailleur des champs pétroliers, concierge)"] <- 0
table(CleanData$act_work_Labour)

CleanData$act_work_Intermediate <- NA
CleanData$act_work_Intermediate[CleanData$act_work == "Emplois intermédiaires (par exemple, boucher industriel, chauffeur de camion long-courrier, serveur)"] <- 1
CleanData$act_work_Intermediate[CleanData$act_work != "Emplois intermédiaires (par exemple, boucher industriel, chauffeur de camion long-courrier, serveur)"] <- 0
table(CleanData$act_work_Intermediate)

CleanData$act_work_Technical <- NA
CleanData$act_work_Technical[CleanData$act_work == "Emplois techniques et/ou métiers spécialisés (par exemple, chef, plombier, électricien)"] <- 1
CleanData$act_work_Technical[CleanData$act_work != "Emplois techniques et/ou métiers spécialisés (par exemple, chef, plombier, électricien)"] <- 0
table(CleanData$act_work_Technical)

CleanData$act_work_Professional <- NA
CleanData$act_work_Professional[CleanData$act_work == "Emplois professionnels (par exemple, médecin, dentiste, architecte)"] <- 1
CleanData$act_work_Professional[CleanData$act_work != "Emplois professionnels (par exemple, médecin, dentiste, architecte)"] <- 0
table(CleanData$act_work_Professional)

CleanData$act_work_Management <- NA
CleanData$act_work_Management[CleanData$act_work == "Emplois de gestion"] <- 1
CleanData$act_work_Management[CleanData$act_work != "Emplois de gestion"] <- 0
table(CleanData$act_work_Management)

CleanData$act_work_Other <- NA
CleanData$act_work_Other[CleanData$act_work == "Autres"] <- 1
CleanData$act_work_Other[CleanData$act_work != "Autres"] <- 0
table(CleanData$act_work_Other)

## Occupez-vous un emploi (employé salarié ou travailleur autonome)?

# Où se trouve cette question ?

## Combien d'heures travaillez-vous en moyenne par semaine?

table(Data$D4)
CleanData$simon_workingless35 <- NA
CleanData$simon_workingless35[Data$D4 < 35] <- 1
CleanData$simon_workingless35[Data$D4 >= 35] <- 0
table(CleanData$simon_workingless35)

CleanData$simon_working35to45 <- NA
CleanData$simon_working35to45[Data$D4 >= 35 & Data$D4 <= 45] <- 1
CleanData$simon_working35to45[Data$D4 < 35 | Data$D4 > 45] <- 0
table(CleanData$simon_working35to45)

CleanData$simon_workingMore45<- NA
CleanData$simon_workingMore45[Data$D4 > 45] <- 1
CleanData$simon_workingMore45[Data$D4 <= 45] <- 0
table(CleanData$simon_workingMore45)

## Avez-vous un poste permanent ou êtes-vous sous contrat?

table(Data$D5)
CleanData$simon_positionPermanent <- NA
CleanData$simon_positionPermanent[Data$D5 == 1] <- 1
CleanData$simon_positionPermanent[Data$D5 != 1] <- 0
table(CleanData$simon_positionPermanent)

CleanData$simon_positionContract <- NA
CleanData$simon_positionContract[Data$D5 == 2] <- 1
CleanData$simon_positionContract[Data$D5 != 2] <- 0
table(CleanData$simon_positionContract)

CleanData$simon_positionOther <- NA
CleanData$simon_positionOther[Data$D5 == 3] <- 1
CleanData$simon_positionOther[Data$D5 != 3] <- 0
table(CleanData$simon_positionOther)

## Dans quelle mesure avez-vous du contrôle sur votre horaire de travail et sur la façon dont vous travaillez? 

table(Data$D6)
CleanData$simon_controlSchedule <- NA
CleanData$simon_controlSchedule[Data$D6==1]<- 0    # Pas du tout; 
CleanData$simon_controlSchedule[Data$D6==2]<- 0.25 # Un peu;
CleanData$simon_controlSchedule[Data$D6==3]<- 0.5  # Modérément;
CleanData$simon_controlSchedule[Data$D6==4]<- 0.75 # Considérablement;
CleanData$simon_controlSchedule[Data$D6==5]<- 1    # Extrêmement
table(CleanData$simon_controlSchedule)

## Faites-vous partie d'un syndicat ou association professionnelle qui agit en tant qu'unité de négociation? 

table(Data$D7)
CleanData$simon_union <- NA
CleanData$simon_union[Data$D7 == 1] <- 1 # Oui
CleanData$simon_union[Data$D7 == 2 | Data$D7 == 3] <- 0 # Non ou Autre
table(CleanData$simon_union)

## À quel point vous sentez-vous important(e) pour les autres?

table(Data$D8)
CleanData$simon_mattering_Important <- NA
CleanData$simon_mattering_Important[Data$D8==1]<- 0    # Pas du tout;
CleanData$simon_mattering_Important[Data$D8==2]<- 0.33 # Un peu; 
CleanData$simon_mattering_Important[Data$D8==3]<- 0.66 # Modérément;
CleanData$simon_mattering_Important[Data$D8==4]<- 1    # Beaucoup
table(CleanData$simon_mattering_Important)

## À quel point pensez-vous que les autres vous prêtent attention?

table(Data$D9)
CleanData$simon_mattering_Attention <- NA
CleanData$simon_mattering_Attention[Data$D9==1]<- 0    # Pas du tout;
CleanData$simon_mattering_Attention[Data$D9==2]<- 0.33 # Un peu; 
CleanData$simon_mattering_Attention[Data$D9==3]<- 0.66 # Modérément;
CleanData$simon_mattering_Attention[Data$D9==4]<- 1    # Beaucoup
table(CleanData$simon_mattering_Attention)

## À quel point manqueriez-vous aux gens si vous n'étiez plus là?

table(Data$D10)
CleanData$simon_mattering_Miss <- NA
CleanData$simon_mattering_Miss[Data$D10==1]<- 0    # Pas du tout;
CleanData$simon_mattering_Miss[Data$D10==2]<- 0.33 # Un peu; 
CleanData$simon_mattering_Miss[Data$D10==3]<- 0.66 # Modérément;
CleanData$simon_mattering_Miss[Data$D10==4]<- 1    # Beaucoup
table(CleanData$simon_mattering_Miss)

## À quel point les gens sont-ils généralement intéressés par ce que vous avez à dire?

table(Data$D11)
CleanData$simon_mattering_Interested <- NA
CleanData$simon_mattering_Interested[Data$D11==1]<- 0    # Pas du tout;
CleanData$simon_mattering_Interested[Data$D11==2]<- 0.33 # Un peu; 
CleanData$simon_mattering_Interested[Data$D11==3]<- 0.66 # Modérément;
CleanData$simon_mattering_Interested[Data$D11==4]<- 1    # Beaucoup
table(CleanData$simon_mattering_Interested)

## À quel point les autres personnes dépendent-elles de vous?

table(Data$D12)
CleanData$simon_mattering_Dependant <- NA
CleanData$simon_mattering_Dependant[Data$D12==1]<- 0    # Pas du tout;
CleanData$simon_mattering_Dependant[Data$D12==2]<- 0.33 # Un peu; 
CleanData$simon_mattering_Dependant[Data$D12==3]<- 0.66 # Modérément;
CleanData$simon_mattering_Dependant[Data$D12==4]<- 1    # Beaucoup
table(CleanData$simon_mattering_Dependant)


#### William ####
## Les affirmations ci-dessous concernent vos sensations et vos pensées. 
## Cochez la case qui correspond le mieux à votre vécu durant les 2 dernières semaines: 


#### Je me suis senti optimiste quant à l'avenir
table(D$D13_A1)
CleanData$simon_MentalOptimist <- NA
CleanData$simon_MentalOptimist[D$D13_A1==1]<- 0    #Jamais; 
CleanData$simon_MentalOptimist[D$D13_A1==2]<- 0.25 #Rarement; 
CleanData$simon_MentalOptimist[D$D13_A1==3]<- 0.5  #Parfois; 
CleanData$simon_MentalOptimist[D$D13_A1==4]<- 0.75 #Souvent; 
CleanData$simon_MentalOptimist[D$D13_A1==5]<- 1    #Tout le temps
table(CleanData$simon_MentalOptimist)
#### Je me suis senti(e) utile 
table(D$D13_A2)
CleanData$simon_MentalUseful <- NA
CleanData$simon_MentalUseful[D$D13_A2==1]<- 0    #Jamais; 
CleanData$simon_MentalUseful[D$D13_A2==2]<- 0.25 #Rarement; 
CleanData$simon_MentalUseful[D$D13_A2==3]<- 0.5  #Parfois; 
CleanData$simon_MentalUseful[D$D13_A2==4]<- 0.75 #Souvent; 
CleanData$simon_MentalUseful[D$D13_A2==5]<- 1    #Tout le temps
table(CleanData$simon_MentalUseful)
#### Je me suis senti(e) utile 
table(D$D13_A3)
CleanData$simon_Mentalrelaxed <- NA
CleanData$simon_Mentalrelaxed[D$D13_A3==1]<- 0    #Jamais; 
CleanData$simon_Mentalrelaxed[D$D13_A3==2]<- 0.25 #Rarement; 
CleanData$simon_Mentalrelaxed[D$D13_A3==3]<- 0.5  #Parfois; 
CleanData$simon_Mentalrelaxed[D$D13_A3==4]<- 0.75 #Souvent; 
CleanData$simon_Mentalrelaxed[D$D13_A3==5]<- 1    #Tout le temps
table(CleanData$simon_Mentalrelaxed)
#### J'ai bien résolu les problèmes auxquels j'ai été confronté
table(D$D13_A4)
CleanData$simon_MentalDeltProb <- NA
CleanData$simon_MentalDeltProb[D$D13_A4==1]<- 0    #Jamais; 
CleanData$simon_MentalDeltProb[D$D13_A4==2]<- 0.25 #Rarement; 
CleanData$simon_MentalDeltProb[D$D13_A4==3]<- 0.5  #Parfois; 
CleanData$simon_MentalDeltProb[D$D13_A4==4]<- 0.75 #Souvent; 
CleanData$simon_MentalDeltProb[D$D13_A4==5]<- 1    #Tout le temps
table(CleanData$simon_MentalDeltProb)
#### Ma pensée était claire
table(D$D13_A5)
CleanData$simon_MentalThinkClear <- NA
CleanData$simon_MentalThinkClear[D$D13_A5==1]<- 0    #Jamais; 
CleanData$simon_MentalThinkClear[D$D13_A5==2]<- 0.25 #Rarement; 
CleanData$simon_MentalThinkClear[D$D13_A5==3]<- 0.5  #Parfois; 
CleanData$simon_MentalThinkClear[D$D13_A5==4]<- 0.75 #Souvent; 
CleanData$simon_MentalThinkClear[D$D13_A5==5]<- 1    #Tout le temps
table(CleanData$simon_MentalThinkClear)
#### Je me suis senti(e) proche des autres
table(D$D13_A6)
CleanData$simon_MentalNearOthers <- NA
CleanData$simon_MentalNearOthers[D$D13_A6==1]<- 0    #Jamais; 
CleanData$simon_MentalNearOthers[D$D13_A6==2]<- 0.25 #Rarement; 
CleanData$simon_MentalNearOthers[D$D13_A6==3]<- 0.5  #Parfois; 
CleanData$simon_MentalNearOthers[D$D13_A6==4]<- 0.75 #Souvent; 
CleanData$simon_MentalNearOthers[D$D13_A6==5]<- 1    #Tout le temps
table(CleanData$simon_MentalNearOthers)
#### J'ai été capable de prendre mes propres décisions
table(D$D13_A7)
CleanData$simon_MentalDecided <- NA
CleanData$simon_MentalDecided[D$D13_A7==1]<- 0    #Jamais; 
CleanData$simon_MentalDecided[D$D13_A7==2]<- 0.25 #Rarement; 
CleanData$simon_MentalDecided[D$D13_A7==3]<- 0.5  #Parfois; 
CleanData$simon_MentalDecided[D$D13_A7==4]<- 0.75 #Souvent; 
CleanData$simon_MentalDecided[D$D13_A7==5]<- 1    #Tout le temps
table(CleanData$simon_MentalDecided)
#### Peu d'intérêt ou de plaisir à faire les choses
table(D$D14_A1)
CleanData$simon_MentalNoFun <- NA
CleanData$simon_MentalNoFun[D$D14_A1==1]<- 0    #Jamais; 
CleanData$simon_MentalNoFun[D$D14_A1==2]<- 0.33 #Plusieurs jours; 
CleanData$simon_MentalNoFun[D$D14_A1==3]<- 0.66 #Plus de sept jour; 
CleanData$simon_MentalNoFun[D$D14_A1==4]<- 1    #Presque tous les jours 
table(CleanData$simon_MentalNoFun)
#### Se sentir triste, déprimé(e) ou déséspéré(e)
table(D$D14_A2)
CleanData$simon_MentalSad <- NA
CleanData$simon_MentalSad[D$D14_A2==1]<- 0    #Jamais; 
CleanData$simon_MentalSad[D$D14_A2==2]<- 0.33 #Plusieurs jours; 
CleanData$simon_MentalSad[D$D14_A2==3]<- 0.66 #Plus de sept jour; 
CleanData$simon_MentalSad[D$D14_A2==4]<- 1    #Presque tous les jours 
table(CleanData$simon_MentalSad)

#### Mes choix quotidiens sont souvent influencés par des considérations écologiques.
table(D$E1_A1)
CleanData$act_envDriveChoice <- NA
CleanData$act_envDriveChoice[D$E1_A1==1] <- 1    #Fortement d’accord
CleanData$act_envDriveChoice[D$E1_A1==2] <- 0.66 #Plutôt d’accord;
CleanData$act_envDriveChoice[D$E1_A1==3] <- 0.33 #Plutôt en désaccord
CleanData$act_envDriveChoice[D$E1_A1==4] <- 0    #Fortement en désaccord
table(CleanData$act_envDriveChoice)

CleanData$act_envDriveChoice2 <- NA
CleanData$act_envDriveChoice2[D$E1_A1==1] <- 1    #Fortement d’accord
CleanData$act_envDriveChoice2[D$E1_A1==2] <- 2 #Plutôt d’accord;
CleanData$act_envDriveChoice2[D$E1_A1==3] <- 3 #Plutôt en désaccord
CleanData$act_envDriveChoice2[D$E1_A1==4] <- 4    #Fortement en désaccord
table(CleanData$act_envDriveChoice2)

#### J'aime essayer des recettes provenant de cultures variées.
table(D$E1_A2)
CleanData$cons_tryCultRecipes <- NA
CleanData$cons_tryCultRecipes[D$E1_A2==1] <- 1    #Fortement d’accord
CleanData$cons_tryCultRecipes[D$E1_A2==2] <- 0.66 #Plutôt d’accord;
CleanData$cons_tryCultRecipes[D$E1_A2==3] <- 0.33 #Plutôt en désaccord
CleanData$cons_tryCultRecipes[D$E1_A2==4] <- 0    #Fortement en désaccord
table(CleanData$cons_tryCultRecipes)

CleanData$cons_tryCultRecipes2 <- NA
CleanData$cons_tryCultRecipes2[D$E1_A2==1] <- 1 #Fortement d’accord
CleanData$cons_tryCultRecipes2[D$E1_A2==2] <- 2 #Plutôt d’accord;
CleanData$cons_tryCultRecipes2[D$E1_A2==3] <- 3 #Plutôt en désaccord
CleanData$cons_tryCultRecipes2[D$E1_A2==4] <- 4 #Fortement en désaccord
table(CleanData$cons_tryCultRecipes2)

#### J'ai tendance à préparer les repas à l'avance. 
table(D$E1_A3)
CleanData$cons_planMeals <- NA
CleanData$cons_planMeals[D$E1_A3==1] <- 1    #Fortement d’accord
CleanData$cons_planMeals[D$E1_A3==2] <- 0.66 #Plutôt d’accord;
CleanData$cons_planMeals[D$E1_A3==3] <- 0.33 #Plutôt en désaccord
CleanData$cons_planMeals[D$E1_A3==4] <- 0    #Fortement en désaccord
table(CleanData$cons_planMeals)

CleanData$cons_planMeals2 <- NA
CleanData$cons_planMeals2[D$E1_A3==1] <- 1 #Fortement d’accord
CleanData$cons_planMeals2[D$E1_A3==2] <- 2 #Plutôt d’accord;
CleanData$cons_planMeals2[D$E1_A3==3] <- 3 #Plutôt en désaccord
CleanData$cons_planMeals2[D$E1_A3==4] <- 4 #Fortement en désaccord
table(CleanData$cons_planMeals2)

#### Lorsque possible, j'évite d'acheter des produits transformés.  
table(D$E1_A4)
CleanData$cons_noTransFoods <- NA
CleanData$cons_noTransFoods[D$E1_A4==1] <- 1    #Fortement d’accord
CleanData$cons_noTransFoods[D$E1_A4==2] <- 0.66 #Plutôt d’accord;
CleanData$cons_noTransFoods[D$E1_A4==3] <- 0.33 #Plutôt en désaccord
CleanData$cons_noTransFoods[D$E1_A4==4] <- 0    #Fortement en désaccord
table(CleanData$cons_noTransFoods)

CleanData$cons_noTransFoods2 <- NA
CleanData$cons_noTransFoods2[D$E1_A4==1] <- 1 #Fortement d’accord
CleanData$cons_noTransFoods2[D$E1_A4==2] <- 2 #Plutôt d’accord;
CleanData$cons_noTransFoods2[D$E1_A4==3] <- 3 #Plutôt en désaccord
CleanData$cons_noTransFoods2[D$E1_A4==4] <- 4 #Fortement en désaccord
table(CleanData$cons_noTransFoods2)


#### Je suis organisé(e) dans ma cuisine.
table(D$E1_A5)
CleanData$cons_orgKitchen <- NA
CleanData$cons_orgKitchen[D$E1_A5==1] <- 1    #Fortement d’accord
CleanData$cons_orgKitchen[D$E1_A5==2] <- 0.66 #Plutôt d’accord;
CleanData$cons_orgKitchen[D$E1_A5==3] <- 0.33 #Plutôt en désaccord
CleanData$cons_orgKitchen[D$E1_A5==4] <- 0    #Fortement en désaccord
table(CleanData$cons_orgKitchen)

CleanData$cons_orgKitchen2 <- NA
CleanData$cons_orgKitchen2[D$E1_A5==1] <- 1 #Fortement d’accord
CleanData$cons_orgKitchen2[D$E1_A5==2] <- 2 #Plutôt d’accord;
CleanData$cons_orgKitchen2[D$E1_A5==3] <- 3 #Plutôt en désaccord
CleanData$cons_orgKitchen2[D$E1_A5==4] <- 4 #Fortement en désaccord
table(CleanData$cons_orgKitchen2)

#### Le Québec devrait devenir un État indépendant. -> Je préfèrerais que le Québec devienne un pays souverain.
table(D$E1Q_A1)
CleanData$op_qcInd <- NA
CleanData$op_qcInd[D$E1Q_A1==1] <- 1    #Fortement d’accord
CleanData$op_qcInd[D$E1Q_A1==2] <- 0.66 #Plutôt d’accord;
CleanData$op_qcInd[D$E1Q_A1==3] <- 0.33 #Plutôt en désaccord
CleanData$op_qcInd[D$E1Q_A1==4] <- 0    #Fortement en désaccord
table(CleanData$op_qcInd)

#### Je me sens davantage Canadien(ne) que Québécois(e) 
table(D$E1Q_A2)
CleanData$op_moreCanThanQc <- NA
CleanData$op_moreCanThanQc[D$E1Q_A2==1] <- 1    #Fortement d’accord
CleanData$op_moreCanThanQc[D$E1Q_A2==2] <- 0.66 #Plutôt d’accord;
CleanData$op_moreCanThanQc[D$E1Q_A2==3] <- 0.33 #Plutôt en désaccord
CleanData$op_moreCanThanQc[D$E1Q_A2==4] <- 0    #Fortement en désaccord
table(CleanData$op_moreCanThanQc)

#### Posséder les dernières technologies me permet de donner le meilleur de moi-même. 
table(D$E1_A6)
CleanData$op_LastTechBestMe <- NA
CleanData$op_LastTechBestMe[D$E1_A6==1] <- 1    #Fortement d’accord
CleanData$op_LastTechBestMe[D$E1_A6==2] <- 0.66 #Plutôt d’accord;
CleanData$op_LastTechBestMe[D$E1_A6==3] <- 0.33 #Plutôt en désaccord
CleanData$op_LastTechBestMe[D$E1_A6==4] <- 0    #Fortement en désaccord
table(CleanData$op_LastTechBestMe)

#### Ça ne me gêne pas de vivre parmi les incertitudes et les imprévus de la vie actuelle
table(D$E2_A1)
CleanData$op_lifesIncertOk <- NA
CleanData$op_lifesIncertOk[D$E2_A1==1] <- 1    #Fortement d’accord
CleanData$op_lifesIncertOk[D$E2_A1==2] <- 0.66 #Plutôt d’accord;
CleanData$op_lifesIncertOk[D$E2_A1==3] <- 0.33 #Plutôt en désaccord
CleanData$op_lifesIncertOk[D$E2_A1==4] <- 0    #Fortement en désaccord
table(CleanData$op_lifesIncertOk)

#### Le changement est une chose essentielle, c’est une garantie qu’on avance et qu’on s’améliore
table(D$E2_A2)
CleanData$op_changeEssential <- NA
CleanData$op_changeEssential[D$E2_A2==1] <- 1    #Fortement d’accord
CleanData$op_changeEssential[D$E2_A2==2] <- 0.66 #Plutôt d’accord;
CleanData$op_changeEssential[D$E2_A2==3] <- 0.33 #Plutôt en désaccord
CleanData$op_changeEssential[D$E2_A2==4] <- 0    #Fortement en désaccord
table(CleanData$op_changeEssential)

#### Quoi que nous fassions, le destin de l’homme est fixé, l’histoire suit son cours 
table(D$E2_A3)
CleanData$op_destinyFixed <- NA
CleanData$op_destinyFixed[D$E2_A3==1] <- 1    #Fortement d’accord
CleanData$op_destinyFixed[D$E2_A3==2] <- 0.66 #Plutôt d’accord;
CleanData$op_destinyFixed[D$E2_A3==3] <- 0.33 #Plutôt en désaccord
CleanData$op_destinyFixed[D$E2_A3==4] <- 0    #Fortement en désaccord
table(CleanData$op_destinyFixed)

#### C’est important pour moi que les gens admirent les choses que je possède
table(D$E2_A4)
CleanData$op_othAdmireMyThings <- NA
CleanData$op_othAdmireMyThings[D$E2_A4==1] <- 1    #Fortement d’accord
CleanData$op_othAdmireMyThings[D$E2_A4==2] <- 0.66 #Plutôt d’accord;
CleanData$op_othAdmireMyThings[D$E2_A4==3] <- 0.33 #Plutôt en désaccord
CleanData$op_othAdmireMyThings[D$E2_A4==4] <- 0    #Fortement en désaccord
table(CleanData$op_othAdmireMyThings)

#### Pour moi, le vrai luxe c’est davantage vivre une expérience unique que de posséder quelque chose d’unique
table(D$E2_A5)
CleanData$op_prefExperience <- NA
CleanData$op_prefExperience[D$E2_A5==1] <- 1    #Fortement d’accord
CleanData$op_prefExperience[D$E2_A5==2] <- 0.66 #Plutôt d’accord;
CleanData$op_prefExperience[D$E2_A5==3] <- 0.33 #Plutôt en désaccord
CleanData$op_prefExperience[D$E2_A5==4] <- 0    #Fortement en désaccord
table(CleanData$op_prefExperience)

#### Dans un groupe, j’aime bien me distinguer des autres par de petits détails dans mon apparence et mon comportement.
table(D$E2_A6)
CleanData$op_likes2BeDifferent <- NA
CleanData$op_likes2BeDifferent[D$E2_A6==1] <- 1    #Fortement d’accord
CleanData$op_likes2BeDifferent[D$E2_A6==2] <- 0.66 #Plutôt d’accord;
CleanData$op_likes2BeDifferent[D$E2_A6==3] <- 0.33 #Plutôt en désaccord
CleanData$op_likes2BeDifferent[D$E2_A6==4] <- 0    #Fortement en désaccord
table(CleanData$op_likes2BeDifferent)

#### Certains termes ne devraient jamais être utilisés, même dans un contexte académique. .
table(D$E2_A7)
CleanData$op_limitWordUse <- NA
CleanData$op_limitWordUse[D$E2_A7==1] <- 1    #Fortement d’accord
CleanData$op_limitWordUse[D$E2_A7==2] <- 0.66 #Plutôt d’accord;
CleanData$op_limitWordUse[D$E2_A7==3] <- 0.33 #Plutôt en désaccord
CleanData$op_limitWordUse[D$E2_A7==4] <- 0    #Fortement en désaccord
table(CleanData$op_limitWordUse)

## Parmi les valeurs suivantes, quelles sont, dans l'ordre, les trois auxquelles vous vous identifiez le plus? 
table(D$E3M1)
table(D$E3M2)
table(D$E3M3)
#### 1) Sentiment d'appartenance;
CleanData$val_belonging1 <- NA
CleanData$val_belonging1[D$E3M1==1]<-1
CleanData$val_belonging1[D$E3M1!=1 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_belonging1)

CleanData$val_belonging2 <- NA
CleanData$val_belonging2[D$E3M2==1]<-1
CleanData$val_belonging2[D$E3M2!=1 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_belonging2)

CleanData$val_belonging3 <- NA
CleanData$val_belonging3[D$E3M3==1]<-1
CleanData$val_belonging3[D$E3M3!=1 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_belonging3)

CleanData$val_belongingAll <- NA 
CleanData$val_belongingAll[D$E3M1==1 | D$E3M2==1 | D$E3M3==1] <-1 
CleanData$val_belongingAll[D$E3M1!=1 & D$E3M2!=1 & D$E3M3!=1] <-0 
table(CleanData$val_belongingAll)

#### 2) Besoin d'excitation; 
CleanData$val_exitement1 <- NA
CleanData$val_exitement1[D$E3M1==2]<-1
CleanData$val_exitement1[D$E3M1!=2 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_exitement1)

CleanData$val_exitement2 <- NA
CleanData$val_exitement2[D$E3M2==2]<-1
CleanData$val_exitement2[D$E3M2!=2 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_exitement2)

CleanData$val_exitement3 <- NA
CleanData$val_exitement3[D$E3M3==2]<-1
CleanData$val_exitement3[D$E3M3!=2 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_exitement3)

CleanData$val_excitementAll <- NA 
CleanData$val_excitementAll[D$E3M1==2 | D$E3M2==2 | D$E3M3==2] <-1 
CleanData$val_excitementAll[D$E3M1!=2 & D$E3M2!=2 & D$E3M3!=2] <-0 
table(CleanData$val_excitementAll)

#### 3) Relations chaleureuses avec les autres; 
CleanData$val_relationship1 <- NA
CleanData$val_relationship1[D$E3M1==3]<-1
CleanData$val_relationship1[D$E3M1!=3 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_relationship1)

CleanData$val_relationship2 <- NA
CleanData$val_relationship2[D$E3M2==3]<-1
CleanData$val_relationship2[D$E3M2!=3 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_relationship2)

CleanData$val_relationship3 <- NA
CleanData$val_relationship3[D$E3M3==3]<-1
CleanData$val_relationship3[D$E3M3!=3 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_relationship3)

CleanData$val_relationshipAll <- NA 
CleanData$val_relationshipAll[D$E3M1==3 | D$E3M2==3 | D$E3M3==3] <-1 
CleanData$val_relationshipAll[D$E3M1!=3 & D$E3M2!=3 & D$E3M3!=3] <-0 
table(CleanData$val_relationshipAll)

#### 4) Épanouissement personnel; 
CleanData$val_fulfillment1 <- NA
CleanData$val_fulfillment1[D$E3M1==4]<-1
CleanData$val_fulfillment1[D$E3M1!=4 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_fulfillment1)

CleanData$val_fulfillment2 <- NA
CleanData$val_fulfillment2[D$E3M2==4]<-1
CleanData$val_fulfillment2[D$E3M2!=4 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_fulfillment2)

CleanData$val_fulfillment3 <- NA
CleanData$val_fulfillment3[D$E3M3==4]<-1
CleanData$val_fulfillment3[D$E3M3!=4 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_fulfillment3)

CleanData$val_fulfillmentAll <- NA 
CleanData$val_fulfillmentAll[D$E3M1==4 | D$E3M2==4 | D$E3M3==4] <-1 
CleanData$val_fulfillmentAll[D$E3M1!=4 & D$E3M2!=4 & D$E3M3!=4] <-0 
table(CleanData$val_fulfillmentAll)

#### 5) Être respecté; 
CleanData$val_bRespected1 <- NA
CleanData$val_bRespected1[D$E3M1==5]<-1
CleanData$val_bRespected1[D$E3M1!=5 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_bRespected1)

CleanData$val_bRespected2 <- NA
CleanData$val_bRespected2[D$E3M2==5]<-1
CleanData$val_bRespected2[D$E3M2!=5 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_bRespected2)

CleanData$val_bRespected3 <- NA
CleanData$val_bRespected3[D$E3M3==5]<-1
CleanData$val_bRespected3[D$E3M3!=5 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_bRespected3)

CleanData$val_bRespectedAll <- NA 
CleanData$val_bRespectedAll[D$E3M1==5 | D$E3M2==5 | D$E3M3==5] <-1 
CleanData$val_bRespectedAll[D$E3M1!=5 & D$E3M2!=5 & D$E3M3!=5] <-0 
table(CleanData$val_bRespectedAll)

#### 6) Amusement et joie de vivre; 
CleanData$val_fun1 <- NA
CleanData$val_fun1[D$E3M1==6]<-1
CleanData$val_fun1[D$E3M1!=6 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_fun1)

CleanData$val_fun2 <- NA
CleanData$val_fun2[D$E3M2==6]<-1
CleanData$val_fun2[D$E3M2!=6 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_fun2)

CleanData$val_fun3 <- NA
CleanData$val_fun3[D$E3M3==6]<-1
CleanData$val_fun3[D$E3M3!=6 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_fun3)

CleanData$val_funAll <- NA 
CleanData$val_funAll[D$E3M1==6 | D$E3M2==6 | D$E3M3==6] <-1 
CleanData$val_funAll[D$E3M1!=6 & D$E3M2!=6 & D$E3M3!=6] <-0 
table(CleanData$val_funAll)

#### 7) Sécurité; 
CleanData$val_security1 <- NA
CleanData$val_security1[D$E3M1==7]<-1
CleanData$val_security1[D$E3M1!=7 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_security1)

CleanData$val_security2 <- NA
CleanData$val_security2[D$E3M2==7]<-1
CleanData$val_security2[D$E3M2!=7 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_security2)

CleanData$val_security3 <- NA
CleanData$val_security3[D$E3M3==7]<-1
CleanData$val_security3[D$E3M3!=7 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_security3)

CleanData$val_securityAll <- NA 
CleanData$val_securityAll[D$E3M1==7 | D$E3M2==7 | D$E3M3==7] <-1 
CleanData$val_securityAll[D$E3M1!=7 & D$E3M2!=7 & D$E3M3!=7] <-0 
table(CleanData$val_securityAll)

#### 8) Respect de soi; 
CleanData$val_selfRespect1 <- NA
CleanData$val_selfRespect1[D$E3M1==8]<-1
CleanData$val_selfRespect1[D$E3M1!=8 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_selfRespect1)

CleanData$val_selfRespect2 <- NA
CleanData$val_selfRespect2[D$E3M2==8]<-1
CleanData$val_selfRespect2[D$E3M2!=8 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_selfRespect2)

CleanData$val_selfRespect3 <- NA
CleanData$val_selfRespect3[D$E3M3==8]<-1
CleanData$val_selfRespect3[D$E3M3!=8 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_selfRespect3)

CleanData$val_selfRespectAll <- NA 
CleanData$val_selfRespectAll[D$E3M1==8 | D$E3M2==8 | D$E3M3==8] <-1 
CleanData$val_selfRespectAll[D$E3M1!=8 & D$E3M2!=8 & D$E3M3!=8] <-0 
table(CleanData$val_selfRespectAll)

#### 9) Sentiment d'accomplissement; 
CleanData$val_accomplish1 <- NA
CleanData$val_accomplish1[D$E3M1==9]<-1
CleanData$val_accomplish1[D$E3M1!=9 & !is.na(D$E3M1)]<-0
table(D$E3M1)
table(CleanData$val_accomplish1)

CleanData$val_accomplish2 <- NA
CleanData$val_accomplish2[D$E3M2==9]<-1
CleanData$val_accomplish2[D$E3M2!=9 & !is.na(D$E3M2)]<-0
table(D$E3M2)
table(CleanData$val_accomplish2)

CleanData$val_accomplish3 <- NA
CleanData$val_accomplish3[D$E3M3==9]<-1
CleanData$val_accomplish3[D$E3M3!=9 & !is.na(D$E3M3)]<-0
table(D$E3M3)
table(CleanData$val_accomplish3)

CleanData$val_accomplishAll <- NA 
CleanData$val_accomplishAll[D$E3M1==9 | D$E3M2==9 | D$E3M3==9] <-1 
CleanData$val_accomplishAll[D$E3M1!=9 & D$E3M2!=9 & D$E3M3!=9] <-0 
table(CleanData$val_accomplishAll)


#### Parmi les deux qualités suivantes, laquelle est la plus importante à avoir pour un enfant? La créativité ou la discipline
table(D$E4B)
CleanData$val_kidsCreative<-NA
CleanData$val_kidsCreative[D$E4B==1] <- 1
CleanData$val_kidsCreative[D$E4B==2] <- 0
table(CleanData$val_kidsCreative)

CleanData$val_kidsDisciplinated<-NA
CleanData$val_kidsDisciplinated[D$E4B==2] <- 1
CleanData$val_kidsDisciplinated[D$E4B==1] <- 0
table(CleanData$val_kidsDisciplinated)

#### Parmi les deux qualités suivantes, laquelle est la plus importante à avoir pour un enfant? La conformité ou la libre-pensée
table(D$E4)
CleanData$val_kidsConformity<-NA
CleanData$val_kidsConformity[D$E4==1] <- 1
CleanData$val_kidsConformity[D$E4==2] <- 0
table(CleanData$val_kidsConformity)

CleanData$val_kidsFreethink<-NA
CleanData$val_kidsFreethink[D$E4==2] <- 1
CleanData$val_kidsFreethink[D$E4==1] <- 0
table(CleanData$val_kidsFreethink)

#### Marc-Antoine ####

# Où achetez-vous vos vêtements le plus régulièrement?

CleanData$cons_brand <- NA
CleanData$cons_brand[Data$F1 == 1] <- 1  # "Magasins à rayon"
CleanData$cons_brand[Data$F1 == 2] <- 2 # "Boutiques indépendantes"
CleanData$cons_brand[Data$F1 == 3] <- 3  # "Chaînes de boutiques"
CleanData$cons_brand[Data$F1 == 4] <- 4  # "Grandes surfaces"
CleanData$cons_brand[Data$F1 == 5] <- 5  # "Magasins qui vendent uniquement en ligne"
CleanData$cons_brand[Data$F1 == 6] <- 6  # "Friperies"
CleanData$cons_brand[Data$F1 == 7] <- 7  # "Autres"
table(CleanData$cons_brand)

CleanData$cons_brand_MaR <- NA
CleanData$cons_brand_MaR[CleanData$cons_brand == 1] <- 1
CleanData$cons_brand_MaR[CleanData$cons_brand != 1] <- 0
table(CleanData$cons_brand_MaR)

CleanData$cons_brand_OnlineOnly <- NA
CleanData$cons_brand_OnlineOnly[CleanData$cons_brand == 5] <- 1
CleanData$cons_brand_OnlineOnly[CleanData$cons_brand != 5] <- 0
table(CleanData$cons_brand_OnlineOnly)

CleanData$cons_brand_BInd <- NA
CleanData$cons_brand_BInd[CleanData$cons_brand == 2] <- 1
CleanData$cons_brand_BInd[CleanData$cons_brand != 2] <- 0
table(CleanData$cons_brand_BInd)

CleanData$cons_brand_ChainesB <- NA
CleanData$cons_brand_ChainesB[CleanData$cons_brand == 3] <- 1
CleanData$cons_brand_ChainesB[CleanData$cons_brand != 3] <- 0
table(CleanData$cons_brand_ChainesB)

CleanData$cons_brand_GSurf <- NA
CleanData$cons_brand_GSurf[CleanData$cons_brand == 4] <- 1
CleanData$cons_brand_GSurf[CleanData$cons_brand != 4] <- 0
table(CleanData$cons_brand_GSurf)

CleanData$cons_brand_Frip2 <- NA
CleanData$cons_brand_Frip2[CleanData$cons_brand == 6] <- 2
CleanData$cons_brand_Frip2[CleanData$cons_brand != 6] <- 1
table(CleanData$cons_brand_Frip2)

CleanData$cons_brand_Frip <- NA
CleanData$cons_brand_Frip[CleanData$cons_brand == 6] <- 1
CleanData$cons_brand_Frip[CleanData$cons_brand != 6] <- 0
table(CleanData$cons_brand_Frip)

CleanData$cons_brand_Other <- NA
CleanData$cons_brand_Other[CleanData$cons_brand == 7] <- 1
CleanData$cons_brand_Other[CleanData$cons_brand != 7] <- 0
table(CleanData$cons_brand_Other)

# Le style vestimentaire d'une personne en dit beaucoup sur qui il est. 

CleanData$cons_fashion_o <- NA 
CleanData$cons_fashion_o[Data$F2 == 1] <- 1 #Fortement d'accord
CleanData$cons_fashion_o[Data$F2 == 2] <- 0.66
CleanData$cons_fashion_o[Data$F2 == 3] <- 0.33
CleanData$cons_fashion_o[Data$F2 == 4] <- 0 #Fortement en désaccord
table(CleanData$cons_fashion_o)

# Consommez-vous de la viande (ou des produits d'origine animale)?
# 1) Oui; 2) Oui, mais de façon modérée; 3) Non, je suis végétarien(e); 4) Non, je suis végan(e) 

CleanData$cons_regime <- NA
CleanData$cons_regime[Data$F3 == 1 | Data$F3 == 2] <- 1 # "Eat meat"
CleanData$cons_regime[Data$F3 == 3] <- 2 # "Vegetarian"
CleanData$cons_regime[Data$F3 == 4] <- 3 #"Vegan"
table(CleanData$cons_regime)

# viandeux 
CleanData$cons_Meat <- NA
CleanData$cons_Meat[Data$F3 == 1 | Data$F3 == 2] <- 1
CleanData$cons_Meat[Data$F3 == 3 | Data$F3 == 4] <- 0
table(CleanData$cons_Meat)

CleanData$cons_Meat2 <- NA
CleanData$cons_Meat2[Data$F3 == 1 | Data$F3 == 2] <- 2
CleanData$cons_Meat2[Data$F3 == 3 | Data$F3 == 4] <- 1
table(CleanData$cons_Meat2)

#vege
CleanData$cons_Vege <- NA
CleanData$cons_Vege[Data$F3 == 3] <- 1
CleanData$cons_Vege[Data$F3 != 3] <- 0
table(CleanData$cons_Vege)

#vegan 
CleanData$cons_Vegan <- NA
CleanData$cons_Vegan[Data$F3 == 4] <- 1
CleanData$cons_Vegan[Data$F3 != 4] <- 0
table(CleanData$cons_Vegan)


# Généralement, où allez-vous pour vous chercher un café? 

CleanData$cons_coffee_place <- NA
CleanData$cons_coffee_place[Data$F4 == 1] <- 1  #"Tim Hortons"
CleanData$cons_coffee_place[Data$F4 == 2] <- 2  #"Starbucks"
CleanData$cons_coffee_place[Data$F4 == 3] <- 3  #"Second Cup"
CleanData$cons_coffee_place[Data$F4 == 4] <- 4  #"McDonalds"
CleanData$cons_coffee_place[Data$F4 == 5] <- 5  #"Autres chaînes de cafés"
CleanData$cons_coffee_place[Data$F4 == 6] <- 6  #"Cafés indépendents"
CleanData$cons_coffee_place[Data$F4 == 7] <- 7  #"Je ne vais pas dans les cafés"
table(CleanData$cons_coffee_place)

CleanData$cons_coffee_TimH <- NA
CleanData$cons_coffee_TimH[!is.na(CleanData$cons_coffee_place)] <- 0
CleanData$cons_coffee_TimH[CleanData$cons_coffee_place == 1] <- 1
table(CleanData$cons_coffee_TimH)

CleanData$cons_coffee_TimH2 <- NA
CleanData$cons_coffee_TimH2[!is.na(CleanData$cons_coffee_place)] <- 1
CleanData$cons_coffee_TimH2[CleanData$cons_coffee_place == 1] <- 2
table(CleanData$cons_coffee_TimH2)

CleanData$cons_coffee_Other <- NA
CleanData$cons_coffee_Other[!is.na(CleanData$cons_coffee_place)] <- 0
CleanData$cons_coffee_Other[CleanData$cons_coffee_place == 5] <- 1
table(CleanData$cons_coffee_Other)

CleanData$cons_coffee_Starbucks <- NA
CleanData$cons_coffee_Starbucks[!is.na(CleanData$cons_coffee_place)] <- 0
CleanData$cons_coffee_Starbucks[CleanData$cons_coffee_place == 2] <- 1
table(CleanData$cons_coffee_Starbucks)

CleanData$cons_coffee_Starbucks2 <- NA
CleanData$cons_coffee_Starbucks2[!is.na(CleanData$cons_coffee_place)] <- 1
CleanData$cons_coffee_Starbucks2[CleanData$cons_coffee_place == 2] <- 2
table(CleanData$cons_coffee_Starbucks2)

CleanData$cons_coffee_SC <- NA
CleanData$cons_coffee_SC[!is.na(CleanData$cons_coffee_place)] <- 0
CleanData$cons_coffee_SC[CleanData$cons_coffee_place == 3] <- 1
table(CleanData$cons_coffee_SC)

CleanData$cons_coffee_McDo <- NA
CleanData$cons_coffee_McDo[!is.na(CleanData$cons_coffee_place)] <- 0
CleanData$cons_coffee_McDo[CleanData$cons_coffee_place == 3] <- 1
table(CleanData$cons_coffee_McDo)

CleanData$cons_coffee_place_ind <- NA
CleanData$cons_coffee_place_ind[CleanData$cons_coffee_place == 6] <- 0
CleanData$cons_coffee_place_ind[CleanData$cons_coffee_place != 6] <- 1
table(CleanData$cons_coffee_place_ind)

CleanData$cons_coffee_place_ind2 <- NA
CleanData$cons_coffee_place_ind2[CleanData$cons_coffee_place == 6] <- 2
CleanData$cons_coffee_place_ind2[CleanData$cons_coffee_place != 6] <- 1
table(CleanData$cons_coffee_place_ind2)

CleanData$cons_coffee_place_noCoffee <- NA
CleanData$cons_coffee_place_noCoffee[CleanData$cons_coffee_place == 7] <- 1
CleanData$cons_coffee_place_noCoffee[CleanData$cons_coffee_place != 7] <- 0
table(CleanData$cons_coffee_place_noCoffee)

# De façon générale, comment préparez-vous votre café?

CleanData$cons_coffee_type <- NA
CleanData$cons_coffee_type[Data$F5 == 1] <- 1 # "Filter coffee"
CleanData$cons_coffee_type[Data$F5 == 2] <- 2 # "Italian coffee maker"
CleanData$cons_coffee_type[Data$F5 == 3] <- 3 # "Coffee percolator"
CleanData$cons_coffee_type[Data$F5 == 4] <- 4 # "French press coffee maker"
CleanData$cons_coffee_type[Data$F5 == 5] <- 5 # "Coffee pods"
CleanData$cons_coffee_type[Data$F5 == 6] <- 6 # "Espresso machine"
CleanData$cons_coffee_type[Data$F5 == 7] <- 7 # "Instant coffee"
table(CleanData$cons_coffee_type)

CleanData$cons_coffee_type_Filtre <- NA
CleanData$cons_coffee_type_Filtre[Data$F5 == 1] <- 1
CleanData$cons_coffee_type_Filtre[Data$F5 != 1] <- 0
table(CleanData$cons_coffee_type_Filtre)

CleanData$cons_coffee_type_Filtre2 <- NA
CleanData$cons_coffee_type_Filtre2[Data$F5 == 1] <- 2
CleanData$cons_coffee_type_Filtre2[Data$F5 != 1] <- 1
table(CleanData$cons_coffee_type_Filtre2)

CleanData$cons_coffee_type_Italien <- NA
CleanData$cons_coffee_type_Italien[Data$F5 == 2] <- 1
CleanData$cons_coffee_type_Italien[Data$F5 != 2] <- 0
table(CleanData$cons_coffee_type_Italien)

CleanData$cons_coffee_type_Perco <- NA
CleanData$cons_coffee_type_Perco[Data$F5 == 3] <- 1
CleanData$cons_coffee_type_Perco[Data$F5 != 3] <- 0
table(CleanData$cons_coffee_type_Perco)

CleanData$cons_coffee_type_PresseFR <- NA
CleanData$cons_coffee_type_PresseFR[Data$F5 == 4] <- 1
CleanData$cons_coffee_type_PresseFR[Data$F5 != 4] <- 0
table(CleanData$cons_coffee_type_PresseFR)

CleanData$cons_coffee_type_Capsules <- NA
CleanData$cons_coffee_type_Capsules[Data$F5 == 5] <- 1
CleanData$cons_coffee_type_Capsules[Data$F5 != 5] <- 0
table(CleanData$cons_coffee_type_Capsules)

CleanData$cons_coffee_type_Capsules2 <- NA
CleanData$cons_coffee_type_Capsules2[Data$F5 == 5] <- 2
CleanData$cons_coffee_type_Capsules2[Data$F5 != 5] <- 1
table(CleanData$cons_coffee_type_Capsules2)

CleanData$cons_coffee_type_Expresso <- NA
CleanData$cons_coffee_type_Expresso[Data$F5 == 6] <- 1
CleanData$cons_coffee_type_Expresso[Data$F5 != 6] <- 0
table(CleanData$cons_coffee_type_Expresso)

CleanData$cons_coffee_type_Expresso2 <- NA
CleanData$cons_coffee_type_Expresso2[Data$F5 == 6] <- 2
CleanData$cons_coffee_type_Expresso2[Data$F5 != 6] <- 1
table(CleanData$cons_coffee_type_Expresso2)

CleanData$cons_coffee_type_Instant <- NA
CleanData$cons_coffee_type_Instant[Data$F5 == 7] <- 1
CleanData$cons_coffee_type_Instant[Data$F5 != 7] <- 0
table(CleanData$cons_coffee_type_Instant)

CleanData$cons_coffee_type_Instant2 <- NA
CleanData$cons_coffee_type_Instant2[Data$F5 == 7] <- 2
CleanData$cons_coffee_type_Instant2[Data$F5 != 7] <- 1
table(CleanData$cons_coffee_type_Instant2)

# Fumez-vous actuellement la cigarette? 

CleanData$cons_smoke_status <- NA
CleanData$cons_smoke_status[Data$F6 == 1] <- 1 # "Currently moking"
CleanData$cons_smoke_status[Data$F6 == 2] <- 2 # "Smoking but trying to stop"
CleanData$cons_smoke_status[Data$F6 == 3] <- 3 # "Has stopped smoking"
CleanData$cons_smoke_status[Data$F6 == 4] <- 4 # "Has never smoked"
CleanData$cons_smoke_status[Data$F6 == 5] <- 5 # "Vaping"
table(CleanData$cons_smoke_status)

CleanData$cons_Smoke <- NA
CleanData$cons_Smoke[Data$F6 == 1 ] <- 1
CleanData$cons_Smoke[Data$F6 == 3 | Data$F6 == 4 | Data$F6 == 2 | Data$F6 == 5] <- 0
table(CleanData$cons_Smoke)

CleanData$cons_SmokeStopping <- NA
CleanData$cons_SmokeStopping[Data$F6 == 2 ] <- 1
CleanData$cons_SmokeStopping[Data$F6 == 3 | Data$F6 == 4 | Data$F6 == 1 | Data$F6 == 5] <- 0
table(CleanData$cons_SmokeStopping)

CleanData$cons_SmokeStopped <- NA
CleanData$cons_SmokeStopped[Data$F6 == 3] <- 1
CleanData$cons_SmokeStopped[Data$F6 == 1 | Data$F6 == 4 | Data$F6 == 2 | Data$F6 == 5] <- 0
table(CleanData$cons_SmokeStopped)

CleanData$cons_SmokeNever <- NA
CleanData$cons_SmokeNever[Data$F6 == 4] <- 1
CleanData$cons_SmokeNever[Data$F6 == 1 | Data$F6 == 3 | Data$F6 == 2 | Data$F6 == 5] <- 0
table(CleanData$cons_SmokeNever)

CleanData$cons_VapeNation <- NA
CleanData$cons_VapeNation[Data$F6 == 5] <- 1
CleanData$cons_VapeNation[Data$F6 == 1 | Data$F6 == 2 | Data$F6 == 3 | Data$F6 == 4 ] <- 0
table(CleanData$cons_VapeNation)

# Quel est votre type d'alcool préféré? 
#1) Vin rouge; 2) Vin blanc; 3) Vin rosé; 4) Vin mousseux ou champagne; 5) Bière régulière;
#6) Bière artisanale et de microbrasseries; 7) Boissons spiritueuses; 8) Cocktails; 9) Je ne consomme pas d'alcool

CleanData$cons_favorite_drink <- NA
CleanData$cons_favorite_drink[Data$F7 == 1] <- 1  # "Red wine"
CleanData$cons_favorite_drink[Data$F7 == 2] <- 2  # "White wine"
CleanData$cons_favorite_drink[Data$F7 == 3] <- 3  # "Rosé wine"
CleanData$cons_favorite_drink[Data$F7 == 4] <- 4  # "Sparkling wines/bubbles"
CleanData$cons_favorite_drink[Data$F7 == 5] <- 5  # "Regular beers"
CleanData$cons_favorite_drink[Data$F7 == 6] <- 6  # "Craft/microbrewery beers"
CleanData$cons_favorite_drink[Data$F7 == 7] <- 7  # "Spirit drinks"
CleanData$cons_favorite_drink[Data$F7 == 8] <- 8  # "Cocktails"
CleanData$cons_favorite_drink[Data$F7 == 9] <- 9  # "I dont drink alcohol"
table(CleanData$cons_favorite_drink)

CleanData$cons_noDrink <- NA
CleanData$cons_noDrink[Data$F7 == 9] <- 1
CleanData$cons_noDrink[Data$F7 != 9] <- 0
table(CleanData$cons_noDrink)

CleanData$cons_redWineDrink <- NA
CleanData$cons_redWineDrink[Data$F7 == 1] <- 1
CleanData$cons_redWineDrink[Data$F7 != 1] <- 0
table(CleanData$cons_redWineDrink)

CleanData$cons_whiteWineDrink <- NA
CleanData$cons_whiteWineDrink[Data$F7 == 2] <- 1
CleanData$cons_whiteWineDrink[Data$F7 != 2] <- 0
table(CleanData$cons_whiteWineDrink)

CleanData$cons_roseDrink <- NA
CleanData$cons_roseDrink[Data$F7 == 3] <- 1
CleanData$cons_roseDrink[Data$F7 != 3] <- 0
table(CleanData$cons_roseDrink)

CleanData$cons_sparklingDrink <- NA
CleanData$cons_sparklingDrink[Data$F7 == 4] <- 1
CleanData$cons_sparklingDrink[Data$F7 != 4] <- 0
table(CleanData$cons_sparklingDrink)

CleanData$cons_regBeers <- NA
CleanData$cons_regBeers[Data$F7 == 5] <- 1
CleanData$cons_regBeers[Data$F7 != 5] <- 0
table(CleanData$cons_regBeers)

CleanData$cons_microBeers <- NA
CleanData$cons_microBeers[Data$F7 == 6] <- 1
CleanData$cons_microBeers[Data$F7 != 6] <- 0
table(CleanData$cons_microBeers)

CleanData$cons_microBeers2 <- NA
CleanData$cons_microBeers2[Data$F7 == 6] <- 2
CleanData$cons_microBeers2[Data$F7 != 6] <- 1
table(CleanData$cons_microBeers2)

CleanData$cons_spiritDrink <- NA
CleanData$cons_spiritDrink[Data$F7 == 7] <- 1
CleanData$cons_spiritDrink[Data$F7 != 7] <- 0
table(CleanData$cons_spiritDrink)

CleanData$cons_cocktailsDrink <- NA
CleanData$cons_cocktailsDrink[Data$F7 == 8] <- 1
CleanData$cons_cocktailsDrink[Data$F7 != 8] <- 0
table(CleanData$cons_cocktailsDrink)

CleanData$cons_beerDrink <- NA
CleanData$cons_beerDrink[Data$F7 == 5] <- 1
CleanData$cons_beerDrink[Data$F7 == 8 | Data$F7 == 6 | Data$F7 == 1 | Data$F7 == 2 |
                                Data$F7 == 9 | Data$F7 == 3 | Data$F7 == 4] <- 0
table(CleanData$cons_beerDrink)

CleanData$cons_microDrink <- NA
CleanData$cons_microDrink[Data$F7 == 6] <- 1
CleanData$cons_microDrink[Data$F7 == 5 | Data$F7 == 8 | Data$F7 == 1 | Data$F7 == 2 |
                                Data$F7 == 9 | Data$F7 == 3 | Data$F7 == 4] <- 0
table(CleanData$cons_microDrink)

CleanData$cons_bubbleDrink <- NA
CleanData$cons_bubbleDrink[Data$F7 == 4] <- 1
CleanData$cons_bubbleDrink[Data$F7 == 5 | Data$F7 == 8 | Data$F7 == 1 | Data$F7 == 2 |
                            Data$F7 == 9 | Data$F7 == 3 | Data$F7 == 6] <- 0
table(CleanData$cons_bubbleDrink)

# À quelle fréquence consommez-vous de l'alcool? 

CleanData$cons_freqDrinking <- NA
CleanData$cons_freqDrinking[Data$F8 == 7] <- 1
CleanData$cons_freqDrinking[Data$F8 == 6] <- 0.8
CleanData$cons_freqDrinking[Data$F8 == 5] <- 0.6
CleanData$cons_freqDrinking[Data$F8 == 4] <- 0.4
CleanData$cons_freqDrinking[Data$F8 == 3] <- 0.2
CleanData$cons_freqDrinking[Data$F8 == 2] <- 0
table(CleanData$cons_freqDrinking)

CleanData$cons_freqDrinking2 <- NA
CleanData$cons_freqDrinking2[Data$F8 == 7] <- 6
CleanData$cons_freqDrinking2[Data$F8 == 6] <- 5
CleanData$cons_freqDrinking2[Data$F8 == 5] <- 4
CleanData$cons_freqDrinking2[Data$F8 == 4] <- 3
CleanData$cons_freqDrinking2[Data$F8 == 3] <- 2
CleanData$cons_freqDrinking2[Data$F8 == 2] <- 1
table(CleanData$cons_freqDrinking2)

# À quelle fréquence consommez-vous de la marijuana? 

CleanData$cons_weed_freq2 <- NA
CleanData$cons_weed_freq2[Data$F9 == 7] <- 7 # "Having weed more than once a day"
CleanData$cons_weed_freq2[Data$F9 == 6] <- 6 # "Having weed once a day"
CleanData$cons_weed_freq2[Data$F9 == 5] <- 5 # "Having weed few times a week"
CleanData$cons_weed_freq2[Data$F9 == 4] <- 4 # "Having weed once a week"
CleanData$cons_weed_freq2[Data$F9 == 3] <- 3 # "Having weed once a month"
CleanData$cons_weed_freq2[Data$F9 == 2] <- 2 # "Having weed few times a year"
CleanData$cons_weed_freq2[Data$F9 == 1] <- 1 # "Never consume weed"
table(CleanData$cons_weed_freq2)

CleanData$cons_noWeed <- NA
CleanData$cons_noWeed[Data$F9 == 1] <- 1
CleanData$cons_noWeed[Data$F9 != 1] <- 0
table(CleanData$cons_noWeed)

# CleanData$cons_weed_freq <- 0
# CleanData$cons_weed_freq <- minmaxNormalization(Data$F9)
# table(CleanData$cons_weed_freq)


###########################
#### WAITING FOR YOURI ####
###########################

# Quel est votre groupe musical ou musicien(ne) préféré? 
# Quel est le meilleur livre que vous avez lu au cours des cinq dernières années?
# Quel est votre film préféré? 
  
###########################
#### WAITING FOR YOURI ####
###########################


# Quel média social utilisez-vous le plus régulièrement?

CleanData$cons_socmedia <- NA
CleanData$cons_socmedia[Data$H1 == 1] <- 1   # "Facebook"
CleanData$cons_socmedia[Data$H1 == 2] <- 2   # "Twitter"
CleanData$cons_socmedia[Data$H1 == 3] <- 3   # "Instagram"
CleanData$cons_socmedia[Data$H1 == 4] <- 4   # "Snapchat"
CleanData$cons_socmedia[Data$H1 == 5] <- 5   # "TikTok"
CleanData$cons_socmedia[Data$H1 == 6] <- 6   # "Pinterest"
CleanData$cons_socmedia[Data$H1 == 7] <- 7   # "LinkedIn"
CleanData$cons_socmedia[Data$H1 == 8] <- 8   # "YouTube"
CleanData$cons_socmedia[Data$H1 == 9] <- 9   # "Other social media"
table(CleanData$cons_socmedia)

CleanData$cons_socmedia_Facebook <- NA
CleanData$cons_socmedia_Facebook[Data$H1 == 1] <- 1 
CleanData$cons_socmedia_Facebook[Data$H1 != 1] <- 0
table(CleanData$cons_socmedia_Facebook)

CleanData$cons_socmedia_Facebook2 <- NA
CleanData$cons_socmedia_Facebook2[Data$H1 == 1] <- 2 
CleanData$cons_socmedia_Facebook2[Data$H1 != 1] <- 1
table(CleanData$cons_socmedia_Facebook2)

CleanData$cons_socmedia_Twitter <- NA
CleanData$cons_socmedia_Twitter[Data$H1 == 2] <- 1 
CleanData$cons_socmedia_Twitter[Data$H1 != 2] <- 0
table(CleanData$cons_socmedia_Twitter)

CleanData$cons_socmedia_Twitter2 <- NA
CleanData$cons_socmedia_Twitter2[Data$H1 == 2] <- 2 
CleanData$cons_socmedia_Twitter2[Data$H1 != 2] <- 1
table(CleanData$cons_socmedia_Twitter2)

CleanData$cons_socmedia_Insta <- NA
CleanData$cons_socmedia_Insta[Data$H1 == 3] <- 1 
CleanData$cons_socmedia_Insta[Data$H1 != 3] <- 0
table(CleanData$cons_socmedia_Insta)

CleanData$cons_socmedia_Insta2 <- NA
CleanData$cons_socmedia_Insta2[Data$H1 == 3] <- 2 
CleanData$cons_socmedia_Insta2[Data$H1 != 3] <- 1
table(CleanData$cons_socmedia_Insta2)

CleanData$cons_socmedia_Snap <- NA
CleanData$cons_socmedia_Snap[Data$H1 == 4] <- 1 
CleanData$cons_socmedia_Snap[Data$H1 != 4] <- 0
table(CleanData$cons_socmedia_Snap)

CleanData$cons_socmedia_Tiktok <- NA
CleanData$cons_socmedia_Tiktok[Data$H1 == 5] <- 1 
CleanData$cons_socmedia_Tiktok[Data$H1 != 5] <- 0
table(CleanData$cons_socmedia_Tiktok)

CleanData$cons_socmedia_Pinterest <- NA
CleanData$cons_socmedia_Pinterest[Data$H1 == 6] <- 1 
CleanData$cons_socmedia_Pinterest[Data$H1 != 6] <- 0
table(CleanData$cons_socmedia_Pinterest)

CleanData$cons_socmedia_LinkedIn <- NA
CleanData$cons_socmedia_LinkedIn[Data$H1 == 7] <- 1 
CleanData$cons_socmedia_LinkedIn[Data$H1 != 7] <- 0
table(CleanData$cons_socmedia_LinkedIn)

CleanData$cons_socmedia_YT <- NA
CleanData$cons_socmedia_YT[Data$H1 == 8] <- 1 
CleanData$cons_socmedia_YT[Data$H1 != 8] <- 0
table(CleanData$cons_socmedia_YT)

CleanData$cons_socmedia_Other <- NA
CleanData$cons_socmedia_Other[Data$H1 == 9] <- 1 
CleanData$cons_socmedia_Other[Data$H1 != 9] <- 0
table(CleanData$cons_socmedia_Other)

# Combien de temps par jour passez-vous sur les médias sociaux?
  
CleanData$cons_socmedia_time <- NA
CleanData$cons_socmedia_time[Data$H2 == 6] <- 1 #Plus de 5 heures 
CleanData$cons_socmedia_time[Data$H2 == 5] <- 0.8 #3-5 heures
CleanData$cons_socmedia_time[Data$H2 == 4] <- 0.6 #2-3 heures
CleanData$cons_socmedia_time[Data$H2 == 3] <- 0.4 #1-2 heures
CleanData$cons_socmedia_time[Data$H2 == 2] <- 0.2 #30-60 minutes; 
CleanData$cons_socmedia_time[Data$H2 == 1] <- 0 #Moins de 30 minutes
table(CleanData$cons_socmedia_time)

CleanData$cons_socmedia_time2 <- NA
CleanData$cons_socmedia_time2[Data$H2 == 6] <- 6 #Plus de 5 heures 
CleanData$cons_socmedia_time2[Data$H2 == 5] <- 5 #3-5 heures
CleanData$cons_socmedia_time2[Data$H2 == 4] <- 4 #2-3 heures
CleanData$cons_socmedia_time2[Data$H2 == 3] <- 3 #1-2 heures
CleanData$cons_socmedia_time2[Data$H2 == 2] <- 2 #30-60 minutes; 
CleanData$cons_socmedia_time2[Data$H2 == 1] <- 1 #Moins de 30 minutes
table(CleanData$cons_socmedia_time2)

# Dans dans la vie de tous les jours, lequel des styles vestimentaires suivants vous décrit le mieux? 

CleanData$app_swag <- NA
CleanData$app_swag[Data$H3 == 1] <- 1 # "Formel/Business"
CleanData$app_swag[Data$H3 == 2] <- 2 # "Classique"
CleanData$app_swag[Data$H3 == 3] <- 3 # "Confortable/Casual"
CleanData$app_swag[Data$H3 == 4] <- 4 # "Sportif"
CleanData$app_swag[Data$H3 == 5] <- 5 # "Elegant/Chic"
CleanData$app_swag[Data$H3 == 6 | Data$H3 == 7 | Data$H3 == 8] <- 6 # "Vintage/Hippie/Bohème"
#CleanData$app_swag[Data$H3 == 7] <- "Hippie"
#CleanData$app_swag[Data$H3 == 8] <- "Bohème"
#CleanData$app_swag[Data$H3 == 9] <- 7 # "Punk"
CleanData$app_swag[Data$H3 == 10] <- 7 #"Rock"
CleanData$app_swag[Data$H3 == 11 | Data$H3 == 9] <- 8 #"Autre ou punk"
table(CleanData$app_swag)

CleanData$app_swag_Formel <- NA
CleanData$app_swag_Formel[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Formel[CleanData$app_swag == 1] <- 1
table(CleanData$app_swag_Formel)

CleanData$app_swag_Classique <- NA
CleanData$app_swag_Classique[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Classique[CleanData$app_swag == 2] <- 1
table(CleanData$app_swag_Classique)

CleanData$app_swag_Casual <- NA
CleanData$app_swag_Casual[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Casual[CleanData$app_swag == 3] <- 1
table(CleanData$app_swag_Casual)

CleanData$app_swag_Sport <- NA
CleanData$app_swag_Sport[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Sport[CleanData$app_swag == 4] <- 1
table(CleanData$app_swag_Sport)

CleanData$app_swag_Chic <- NA
CleanData$app_swag_Chic[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Chic[CleanData$app_swag == 5] <- 1
table(CleanData$app_swag_Chic)

CleanData$app_swag_VintageHippBoheme <- NA
CleanData$app_swag_VintageHippBoheme[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_VintageHippBoheme[CleanData$app_swag == 6] <- 1
table(CleanData$app_swag_Vintage)

# CleanData$app_swag_Hippie <- NA
# CleanData$app_swag_Hippie[!is.na(CleanData$app_swag)] <- 0
# CleanData$app_swag_Hippie[CleanData$app_swag == "Hippie"] <- 1
# table(CleanData$app_swag_Hippie)

# CleanData$app_swag_Boh <- NA
# CleanData$app_swag_Boh[!is.na(CleanData$app_swag)] <- 0
# CleanData$app_swag_Boh[CleanData$app_swag == "Bohème"] <- 1
# table(CleanData$app_swag_Boh)

CleanData$app_swag_Other <- NA
CleanData$app_swag_Other[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Other[CleanData$app_swag == 8] <- 1
table(CleanData$app_swag_Other)

CleanData$app_swag_Rock <- NA
CleanData$app_swag_Rock[!is.na(CleanData$app_swag)] <- 0
CleanData$app_swag_Rock[CleanData$app_swag == 7] <- 1
table(CleanData$app_swag_Rock)



# Combien de tatouages avez-vous? 

CleanData$app_tatouages <- Data$H4

## binary variable
CleanData$app_noTattoo <- NA
CleanData$app_noTattoo[Data$H4 == 0] <- 1 #no tattoos
CleanData$app_noTattoo[Data$H4 != 0] <- 0 #tattoos (1 or more)
table(CleanData$app_noTattoo)

CleanData$app_noTattoo2 <- NA
CleanData$app_noTattoo2[Data$H4 == 0] <- 1 #no tattoos
CleanData$app_noTattoo2[Data$H4 != 0] <- 2 #tattoos (1 or more)
table(CleanData$app_noTattoo2)

# Comme vous le savez, il y aura probablement une élection fédérale cette année. Dans quelle mesure êtes-vous certain(e) d'aller voter?

CleanData$op_turnout <- NA
CleanData$op_turnout[Data$I0 == 1] <- 1 #certain voter
CleanData$op_turnout[Data$I0 == 2] <- 0.66 #probable
CleanData$op_turnout[Data$I0 == 3] <- 0.33 #improbable
CleanData$op_turnout[Data$I0 == 4] <- 0 #certain de ne pas voter 
table(CleanData$op_turnout)

# Pour quel parti voteriez-vous s'il y avait une élection fédérale canadienne aujourd'hui?

CleanData$op_intent <- NA 
CleanData$op_intent[Data$I1 == 1] <- 1 # "LPC"
CleanData$op_intent[Data$I1 == 2] <- 2  # "CPC"
CleanData$op_intent[Data$I1 == 3] <- 3 # "NDP"
CleanData$op_intent[Data$I1 == 4] <- 4  # "BQ"
CleanData$op_intent[Data$I1 == 5] <- 5 # "GP"
CleanData$op_intent[Data$I1 == 6] <- 6  # "PPC"
CleanData$op_intent[Data$I1 == 7] <- 7  # "Other party"
CleanData$op_intent[Data$I1 == 8] <- 8  # "No vote"
CleanData$op_intent[Data$I1 == 9] <- 9   # "Would boycott ballot"
CleanData$op_intent[Data$I1 == 10] <- 10 #  "Undecided"
table(CleanData$op_intent)

CleanData$op_voteIntent_Lib <- NA
CleanData$op_voteIntent_Lib[Data$I1 == 1] <- 1
CleanData$op_voteIntent_Lib[Data$I1 != 1] <- 0
CleanData$op_voteIntent_Lib[Data$I1 == 7 | Data$I1 == 8 | Data$I1 ==  9 | Data$I1 == 10] <- 0
table(CleanData$op_voteIntent_Lib)

CleanData$op_voteIntent_Cons <- NA
CleanData$op_voteIntent_Cons[Data$I1 == 2] <- 1
CleanData$op_voteIntent_Cons[Data$I1 != 2] <- 0
CleanData$op_voteIntent_Cons[Data$I1 == 7 | Data$I1 == 8 | Data$I1 ==  9 | Data$I1 == 10] <- 0
table(CleanData$op_voteIntent_Cons)

CleanData$op_voteIntent_Ndp <- NA
CleanData$op_voteIntent_Ndp[Data$I1 == 3] <- 1
CleanData$op_voteIntent_Ndp[Data$I1 != 3] <- 0
CleanData$op_voteIntent_Ndp[Data$I1 == 7 | Data$I1 == 8 | Data$I1 ==  9 | Data$I1 == 10] <- 0
table(CleanData$op_voteIntent_Ndp)

CleanData$op_voteIntent_Bloc <- NA
CleanData$op_voteIntent_Bloc[Data$I1 == 4] <- 1
CleanData$op_voteIntent_Bloc[Data$I1 != 4] <- 0
CleanData$op_voteIntent_Bloc[Data$I1 == 7 | Data$I1 == 8 | Data$I1 ==  9 | Data$I1 == 10] <- 0
table(CleanData$op_voteIntent_Bloc)

CleanData$op_voteIntent_Green <- NA
CleanData$op_voteIntent_Green[Data$I1 == 5] <- 1
CleanData$op_voteIntent_Green[Data$I1 != 5] <- 0
CleanData$op_voteIntent_Green[Data$I1 == 7 | Data$I1 == 8 | Data$I1 ==  9 | Data$I1 == 10] <- 0
table(CleanData$op_voteIntent_Green)

CleanData$op_voteIntent_PPC <- NA
CleanData$op_voteIntent_PPC[Data$I1 == 6] <- 1
CleanData$op_voteIntent_PPC[Data$I1 != 6] <- 0
CleanData$op_voteIntent_PPC[Data$I1 == 7 | Data$I1 == 8 | Data$I1 ==  9 | Data$I1 == 10] <- 0
table(CleanData$op_voteIntent_PPC)

CleanData$op_voteIntent_NoVote <- NA 
CleanData$op_voteIntent_NoVote[Data$I1 == 8] <- 1
CleanData$op_voteIntent_NoVote[Data$I1 != 8] <- 0
table(CleanData$op_voteIntent_NoVote)

# indécis, annulerais mon vote, ou je ne sais pas 
CleanData$op_voteIntent_Other <- NA 
CleanData$op_voteIntent_Other[Data$I1 == 7 | Data$I1 ==  9 | Data$I1 == 10] <- 1
CleanData$op_voteIntent_Other[Data$I1 !=  7 & Data$I1 !=  9 & Data$I1 !=  10] <- 0
table(CleanData$op_voteIntent_Other)


# Dans quelle mesure en êtes-vous certain? 
CleanData$op_voteCertainty <- NA
CleanData$op_voteCertainty[Data$I2 == 1] <- 1 #très certain 
CleanData$op_voteCertainty[Data$I2 == 2] <- 0.66 
CleanData$op_voteCertainty[Data$I2 == 3] <- 0.33
CleanData$op_voteCertainty[Data$I2 == 4] <- 0 #pas du tout certain
table(CleanData$op_voteCertainty)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la 
# prochaine élection fédérale canadienne, en général, quelle est la probabilité que vous appuyiez le Parti libéral du Canada? 
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

CleanData$op_potentialG_Lib <- NA
CleanData$op_potentialG_Lib <- Data$I3_A1/10
table(CleanData$op_potentialG_Lib)


# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Parti conservateur du Canada?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

CleanData$op_potentialG_Cons <- NA
CleanData$op_potentialG_Cons <- Data$I3_A2/10
table(CleanData$op_potentialG_Cons)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Nouveau parti démocratique?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

CleanData$op_potentialG_Ndp <- NA
CleanData$op_potentialG_Ndp <- Data$I3_A3/10
table(CleanData$op_potentialG_Ndp)

#### Catherine ####

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Bloc québécois?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(Data$I3_A4)
CleanData$op_potentialG_BQ <- NA
CleanData$op_potentialG_BQ <- Data$I3_A4/10
table(CleanData$op_potentialG_BQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection fédérale canadienne,
# en général, quelle est la probabilité que vous appuyiez le Parti vert?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(Data$I3_A5)
CleanData$op_potentialG_PV <- NA
CleanData$op_potentialG_PV <- Data$I3_A5/10
table(CleanData$op_potentialG_PV)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez la Coalition Avenir Québec?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(Data$I3Q_A6)
CleanData$op_potentialG_CAQ <- NA
CleanData$op_potentialG_CAQ <- Data$I3Q_A6/10
table(CleanData$op_potentialG_CAQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez le Parti libéral du Québec?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(Data$I3Q_A7)
CleanData$op_potentialG_PLQ <- NA
CleanData$op_potentialG_PLQ <- Data$I3Q_A7/10
table(CleanData$op_potentialG_PLQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez le Parti québécois?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(Data$I3Q_A8)
CleanData$op_potentialG_PQ <- NA
CleanData$op_potentialG_PQ <- Data$I3Q_A8/10
table(CleanData$op_potentialG_PQ)

# Quel que soit le parti pour lequel vous avez l'intention de voter à l'occasion de la prochaine élection provinciale québécoise,
# en général, quelle est la probabilité que vous appuyiez Québec solidaire?
# [Sur une échelle de 0 à 10, où 0 signifie très peu probable, et 10 très probable]

table(Data$I3Q_A9)
CleanData$op_potentialG_QS <- NA
CleanData$op_potentialG_QS <- Data$I3Q_A9/10
table(CleanData$op_potentialG_QS)

# Selon vous, quel parti obtiendra le plus de sièges lors des prochaines élections fédérales canadiennes? 

# Le Parti libéral du Canada
table(Data$I4)
CleanData$op_peoplePred_PLC <- NA 
CleanData$op_peoplePred_PLC[Data$I4 == 1] <- 1 
CleanData$op_peoplePred_PLC[Data$I4 != 1] <- 0 
table(CleanData$op_peoplePred_PLC)

# Le Parti conservateur du Canada
table(Data$I4)
CleanData$op_peoplePred_PCC <- NA 
CleanData$op_peoplePred_PCC[Data$I4 == 2] <- 1 
CleanData$op_peoplePred_PCC[Data$I4 != 2] <- 0 
table(CleanData$op_peoplePred_PCC)

# Le Nouveau parti démocratique
table(Data$I4)
CleanData$op_peoplePred_NPD <- NA 
CleanData$op_peoplePred_NPD[Data$I4 == 3] <- 1 
CleanData$op_peoplePred_NPD[Data$I4 != 3] <- 0 
table(CleanData$op_peoplePred_NPD)

# Le Bloc québécois
table(Data$I4)
CleanData$op_peoplePred_BQ <- NA 
CleanData$op_peoplePred_BQ[Data$I4 == 4] <- 1 
CleanData$op_peoplePred_BQ[Data$I4 != 4] <- 0 
table(CleanData$op_peoplePred_BQ)

# Le Parti vert
table(Data$I4)
CleanData$op_peoplePred_PV <- NA 
CleanData$op_peoplePred_PV[Data$I4 == 5] <- 1 
CleanData$op_peoplePred_PV[Data$I4 != 5] <- 0 
table(CleanData$op_peoplePred_PV)

# Le Parti populaire du Canada
table(Data$I4)
CleanData$op_peoplePred_PPC <- NA 
CleanData$op_peoplePred_PPC[Data$I4 == 6] <- 1 
CleanData$op_peoplePred_PPC[Data$I4 != 6] <- 0 
table(CleanData$op_peoplePred_PPC)

# Autre parti
table(Data$I4)
CleanData$op_peoplePred_Other <- NA 
CleanData$op_peoplePred_Other[Data$I4 == 7] <- 1 
CleanData$op_peoplePred_Other[Data$I4 != 7] <- 0 
table(CleanData$op_peoplePred_Other)

# Lors d'une élection, certaines personnes ne peuvent pas voter parce qu'elles sont malades ou occupées, ou pour une autre raison. 
# Avez-vous voté aux élections fédérales canadiennes de 2019 ? 

table(Data$I5)
CleanData$op_turnout2019 <- NA 
CleanData$op_turnout2019[Data$I5 == 1] <- 1 #oui
CleanData$op_turnout2019[Data$I5 != 1] <- 0 #non
table(CleanData$op_turnout2019)

###########################
#### WAITING FOR YOURI ####
###########################

# Parmi les personnes suivantes, quelles sont les trois avec qui vous partagez davantage de valeurs et avez le même style de vie

# Parmi les trois photos choisies: Pour quel parti vote cette personne à l'élection fédérale

# Parmi les trois photos choisies: Pour quel parti vote cette personne à l'élection provinciale

# Quel est votre plus haut niveau de scolarité complété? 

table(Data$SES1)

CleanData$ses_educ <- NA
CleanData$ses_educ[Data$SES1 == 1 | Data$SES1 == 2] <- 1   #   "Elementary school and below"
CleanData$ses_educ[Data$SES1 == 3] <- 2                    #   "High school"
CleanData$ses_educ[Data$SES1 == 4] <- 3                    #   "Technical, community college, CEGEP or College classique"
CleanData$ses_educ[Data$SES1 == 5] <- 4                    #   "Bachelor's degree"
CleanData$ses_educ[Data$SES1 == 6] <- 5                    #   "Master's degree"
CleanData$ses_educ[Data$SES1 == 7] <- 6                    #   "Doctorate"
table(CleanData$ses_educ)

# high school and below
CleanData$ses_educ_None <- NA 
CleanData$ses_educ_None[Data$SES1 == 1] <- 1 
CleanData$ses_educ_None[Data$SES1 != 1] <- 0
table(CleanData$ses_educ_None)

CleanData$ses_educ_Prim <- NA 
CleanData$ses_educ_Prim[Data$SES1 == 2] <- 1 
CleanData$ses_educ_Prim[Data$SES1 != 2] <- 0
table(CleanData$ses_educ_Prim)

CleanData$ses_educ_Sec <- NA 
CleanData$ses_educ_Sec[Data$SES1 == 3] <- 1 
CleanData$ses_educ_Sec[Data$SES1 != 3] <- 0
table(CleanData$ses_educ_Sec)

CleanData$ses_educ_Coll <- NA 
CleanData$ses_educ_Coll[Data$SES1 == 4] <- 1 
CleanData$ses_educ_Coll[Data$SES1 != 4] <- 0
table(CleanData$ses_educ_Coll)

CleanData$ses_educ_Bacc <- NA 
CleanData$ses_educ_Bacc[Data$SES1 == 5] <- 1 
CleanData$ses_educ_Bacc[Data$SES1 != 5] <- 0
table(CleanData$ses_educ_Bacc)

CleanData$ses_educ_Master <- NA 
CleanData$ses_educ_Master[Data$SES1 == 6] <- 1 
CleanData$ses_educ_Master[Data$SES1 != 6] <- 0
table(CleanData$ses_educ_Master)

CleanData$ses_educ_PhD <- NA 
CleanData$ses_educ_PhD[Data$SES1 == 7] <- 1 
CleanData$ses_educ_PhD[Data$SES1 != 7] <- 0
table(CleanData$ses_educ_PhD)

#### CLEANING ACCORDNING TO CES 

# Education 
# Below high school
CleanData$educBHS <- NA
CleanData$educBHS[!is.na(Data$SES1)] <- 0
CleanData$educBHS[Data$SES1==1 |
                    Data$SES1==2] <- 1
table(CleanData$educBHS)

# High school
CleanData$educHS <- NA
CleanData$educHS[!is.na(Data$SES1)] <- 0
CleanData$educHS[Data$SES1==3] <- 1
table(CleanData$educHS)

# College/University 
CleanData$educUniv <- NA
CleanData$educUniv[!is.na(Data$SES1)] <- 0
CleanData$educUniv[Data$SES1==4 |
                     Data$SES1==5 |
                     Data$SES1==6 |
                     Data$SES1==7] <- 1
table(CleanData$educUniv)


# Approximativement, dans laquelle des catégories suivantes le revenu de votre ménage de situe-t-il? 

table(Data$SES2)

CleanData$ses_income <- NA
CleanData$ses_income[Data$SES2 == 1 | Data$SES2 == 2] <- 1  #  "Less than 30k"
CleanData$ses_income[Data$SES2 == 3] <- 2                   #  "31k to 60k"
CleanData$ses_income[Data$SES2 == 4] <- 3                   #  "61k to 90k"
CleanData$ses_income[Data$SES2 == 5] <- 4                   #  "91k to 110k"
CleanData$ses_income[Data$SES2 == 6] <- 5                   #  "111k to 150k"
CleanData$ses_income[Data$SES2 == 7] <- 6                   #  "151k to 200k"
CleanData$ses_income[Data$SES2 == 8] <- 7                   #  "More than 200k"
table(CleanData$ses_income)

CleanData$ses_income_None <- NA
CleanData$ses_income_None[Data$SES2 == 1] <- 1 
CleanData$ses_income_None[Data$SES2 != 1] <- 0 
CleanData$ses_income_None[Data$SES2 == 9] <- NA
table(CleanData$ses_income_None)

CleanData$ses_income_i1to30 <- NA
CleanData$ses_income_i1to30[Data$SES2 == 2] <- 1 
CleanData$ses_income_i1to30[Data$SES2 != 2] <- 0 
CleanData$ses_income_i1to30[Data$SES2 == 9] <- NA
table(CleanData$ses_income_i1to30)

CleanData$ses_income_i31to60 <- NA
CleanData$ses_income_i31to60[Data$SES2 == 3] <- 1 
CleanData$ses_income_i31to60[Data$SES2 != 3] <- 0 
CleanData$ses_income_i31to60[Data$SES2 == 9] <- NA
table(CleanData$ses_income_i31to60)

CleanData$ses_income_i61to90 <- NA
CleanData$ses_income_i61to90[Data$SES2 == 4] <- 1 
CleanData$ses_income_i61to90[Data$SES2 != 4] <- 0 
CleanData$ses_income_i61to90[Data$SES2 == 9] <- NA
table(CleanData$ses_income_i61to90)

CleanData$ses_income_i91to110 <- NA
CleanData$ses_income_i91to110[Data$SES2 == 5] <- 1 
CleanData$ses_income_i91to110[Data$SES2 != 5] <- 0 
CleanData$ses_income_i91to110[Data$SES2 == 9] <- NA
table(CleanData$ses_income_i91to110)

CleanData$ses_income_i111to150  <- NA
CleanData$ses_income_i111to150 [Data$SES2 == 6] <- 1 
CleanData$ses_income_i111to150 [Data$SES2 != 6] <- 0 
CleanData$ses_income_i111to150 [Data$SES2 == 9] <- NA
table(CleanData$ses_income_i111to150)

CleanData$ses_income_i151to200  <- NA
CleanData$ses_income_i151to200 [Data$SES2 == 7] <- 1 
CleanData$ses_income_i151to200 [Data$SES2 != 7] <- 0 
CleanData$ses_income_i151to200 [Data$SES2 == 9] <- NA
table(CleanData$ses_income_i151to200)

CleanData$ses_income_i201toInf  <- NA
CleanData$ses_income_i201toInf [Data$SES2 == 8] <- 1 
CleanData$ses_income_i201toInf [Data$SES2 != 8] <- 0 
CleanData$ses_income_i201toInf [Data$SES2 == 9] <- NA
table(CleanData$ses_income_i201toInf)

### Cleaning according to CES 

## Income 

# Low
CleanData$incomeLow <- NA
CleanData$incomeLow[Data$SES2==1 | Data$SES2==2] <- 1
CleanData$incomeLow[Data$SES2!=1 & Data$SES2!=2] <- 0
table(CleanData$incomeLow)

# Medium
CleanData$incomeMid <- NA
CleanData$incomeMid[Data$SES2==3 | Data$SES2==4] <- 1
CleanData$incomeMid[Data$SES2!=3 & Data$SES2!=4] <- 0
table(CleanData$incomeMid)

# High
CleanData$incomeHigh <- NA
CleanData$incomeHigh[Data$SES2==5 | Data$SES2==6 | Data$SES2==7 | Data$SES2==8] <- 1
CleanData$incomeHigh[Data$SES2!=5 & Data$SES2!=6 & Data$SES2!=7 & Data$SES2!=8] <- 0
table(CleanData$incomeHigh)

### Province 

table(Data$PROV)

CleanData$ses_province <- NA
CleanData$ses_province[Data$PROV == 1 | Data$PROV == 2 | Data$PROV == 3 | Data$PROV == 12] <- 1 # "West"
CleanData$ses_province[Data$PROV == 9] <- 2 # "Ontario"
CleanData$ses_province[Data$PROV == 11] <- 3 # "Quebec"
CleanData$ses_province[Data$PROV == 10 | Data$PROV == 4 | Data$PROV == 6 | Data$PROV == 5] <- 4 # "Maritimes"
table(CleanData$ses_province)

CleanData$ses_prov_Alb <- NA
CleanData$ses_prov_Alb[Data$PROV==1] <- 1
CleanData$ses_prov_Alb[Data$PROV!=1] <- 0
table(CleanData$ses_prov_Alb)

CleanData$ses_prov_Bc <- NA
CleanData$ses_prov_Bc[Data$PROV==2] <- 1
CleanData$ses_prov_Bc[Data$PROV!=2] <- 0
table(CleanData$ses_prov_Bc)

CleanData$ses_prov_Manitoba <- NA
CleanData$ses_prov_Manitoba[Data$PROV==3] <- 1
CleanData$ses_prov_Manitoba[Data$PROV!=3] <- 0
table(CleanData$ses_prov_Manitoba)

CleanData$ses_prov_Nb <- NA
CleanData$ses_prov_Nb[Data$PROV==4] <- 1
CleanData$ses_prov_Nb[Data$PROV!=4] <- 0
table(CleanData$ses_prov_Nb)

CleanData$ses_prov_Nfl <- NA
CleanData$ses_prov_Nfl[Data$PROV==5] <- 1
CleanData$ses_prov_Nfl[Data$PROV!=5] <- 0
table(CleanData$ses_prov_Nfl)

CleanData$ses_prov_Ns <- NA
CleanData$ses_prov_Ns[Data$PROV==6] <- 1
CleanData$ses_prov_Ns[Data$PROV!=6] <- 0
table(CleanData$ses_prov_Ns)

# CleanData$ses_prov_NWT <- NA
# CleanData$ses_prov_NWT[Data$PROV==7] <- 1
# CleanData$ses_prov_NWT[Data$PROV!=7] <- 0
# table(CleanData$ses_prov_NWT)

# CleanData$ses_prov_Nunavut <- NA
# CleanData$ses_prov_Nunavut[Data$PROV==8] <- 1
# CleanData$ses_prov_Nunavut[Data$PROV!=8] <- 0
# table(CleanData$ses_prov_Nunavut)

CleanData$ontario <- NA
CleanData$ontario[Data$PROV==9] <- 1
CleanData$ontario[Data$PROV!=9] <- 0
table(CleanData$ontario)

CleanData$ses_prov_Pei <- NA
CleanData$ses_prov_Pei[Data$PROV==10] <- 1
CleanData$ses_prov_Pei[Data$PROV!=10] <- 0
table(CleanData$ses_prov_Pei)

CleanData$quebec <- NA
CleanData$quebec[Data$PROV==11] <- 1
CleanData$quebec[Data$PROV!=11] <- 0
table(CleanData$quebec)

CleanData$ses_prov_Skt <- NA
CleanData$ses_prov_Skt[Data$PROV==12] <- 1
CleanData$ses_prov_Skt[Data$PROV!=12] <- 0
table(CleanData$ses_prov_Skt)

# CleanData$ses_prov_Yukon <- NA
# CleanData$ses_prov_Yukon[Data$PROV==13] <- 1
# CleanData$ses_prov_Yukon[Data$PROV!=13] <- 0
# table(CleanData$ses_prov_Yukon)

CleanData$maritimes <- NA
CleanData$maritimes[CleanData$ses_prov_Pei == 1 | CleanData$ses_prov_Nb == 1 |
                         CleanData$ses_prov_Ns == 1 | CleanData$ses_prov_Nfl == 1] <- 1
CleanData$maritimes[CleanData$ses_prov_Pei != 1 & CleanData$ses_prov_Nb != 1 &
                         CleanData$ses_prov_Ns != 1 & CleanData$ses_prov_Nfl != 1] <- 0
table(CleanData$maritimes)

CleanData$west <- NA
CleanData$west[CleanData$ses_prov_Manitoba == 1 | CleanData$ses_prov_Alb == 1 |
              CleanData$ses_prov_Skt == 1 | CleanData$ses_prov_Bc == 1] <- 1
CleanData$west[CleanData$ses_prov_Manitoba != 1 & CleanData$ses_prov_Alb != 1 &
                    CleanData$ses_prov_Skt != 1 & CleanData$ses_prov_Bc != 1] <- 0
table(CleanData$west)


# Vivez-vous dans un environnement urbain, sururbain, ou rural? 

table(Data$SES3)
CleanData$ses_env <- NA
CleanData$ses_env[Data$SES3 == 1] <- 1   #  "urban"
CleanData$ses_env[Data$SES3 == 2] <- 2   #  "sururban"
CleanData$ses_env[Data$SES3 == 3] <- 3   #  "rural"
table(CleanData$ses_env)

CleanData$ses_urbain <- NA
CleanData$ses_urbain[Data$SES3 == 1] <- 1 
CleanData$ses_urbain[Data$SES3 != 1] <- 0 
table(CleanData$ses_urbain)

CleanData$ses_sururbain <- NA
CleanData$ses_sururbain[Data$SES3 == 2] <- 1 
CleanData$ses_sururbain[Data$SES3 != 2] <- 0 
table(CleanData$ses_sururbain)

CleanData$ses_rural <- NA
CleanData$ses_rural[Data$SES3 == 3] <- 1 
CleanData$ses_rural[Data$SES3 != 3] <- 0 
table(CleanData$ses_rural)

# Quel est votre statut matrionial? 
CleanData$ses_matrimonial_status <- NA
CleanData$ses_matrimonial_status[Data$SES4 == 1] <- 1   # "single"
CleanData$ses_matrimonial_status[Data$SES4 == 2] <- 2   # "married" 
CleanData$ses_matrimonial_status[Data$SES4 == 3] <- 3   # "common-law relationship" 
CleanData$ses_matrimonial_status[Data$SES4 == 4] <- 4   # "widow/widower" 
CleanData$ses_matrimonial_status[Data$SES4 == 5] <- 5   # "divorced" 
table(CleanData$ses_matrimonial_status)

table(Data$SES4)
CleanData$ses_celib <- NA
CleanData$ses_celib[Data$SES4 == 1] <- 1 
CleanData$ses_celib[Data$SES4 != 1] <- 0 
table(CleanData$ses_celib)

CleanData$ses_married <- NA
CleanData$ses_married[Data$SES4 == 2] <- 1 
CleanData$ses_married[Data$SES4 != 2] <- 0 
table(CleanData$ses_married)

CleanData$ses_commonlawRel <- NA
CleanData$ses_commonlawRel[Data$SES4 == 3] <- 1 
CleanData$ses_commonlawRel[Data$SES4 != 3] <- 0 
table(CleanData$ses_commonlawRel)

CleanData$ses_widow <- NA
CleanData$ses_widow[Data$SES4 == 4] <- 1 
CleanData$ses_widow[Data$SES4 != 4] <- 0 
table(CleanData$ses_widow)

CleanData$ses_divorced <- NA
CleanData$ses_divorced[Data$SES4 == 5] <- 1 
CleanData$ses_divorced[Data$SES4 != 5] <- 0 
table(CleanData$ses_divorced)

# married and common-law partner binded together 
CleanData$ses_relationship <- NA 
CleanData$ses_relationship[Data$SES4 == 2 | Data$SES4 == 3] <- 1 
CleanData$ses_relationship[Data$SES4 != 2 & Data$SES4 != 3] <- 0 
table(CleanData$ses_relationship)

# Combien d'enfants vivent avec vous? 
table(Data$SES5)

# 0 
CleanData$ses_noHouseholdKids <- NA 
CleanData$ses_noHouseholdKids[Data$SES5 == 0] <- 1
CleanData$ses_noHouseholdKids[Data$SES5 != 0] <- 0
table(CleanData$ses_noHouseholdKids)

# 1-2
CleanData$ses_fewHouseholdKids <- NA 
CleanData$ses_fewHouseholdKids[Data$SES5 == 1 | Data$SES5 == 2] <- 1
CleanData$ses_fewHouseholdKids[Data$SES5 != 1 & Data$SES5 != 2] <- 0
table(CleanData$ses_fewHouseholdKids)

# 3 et plus 
CleanData$ses_manyHouseholdKids <- NA 
CleanData$ses_manyHouseholdKids[Data$SES5 %in% c(3:6)] <- 1
CleanData$ses_manyHouseholdKids[Data$SES5 %in% c(0:2)] <- 0
table(CleanData$ses_manyHouseholdKids)


# En plus d'être Canadien, à quel(s) groupe(s) ethnique ou culturel appartenez-vous?


# Parmi les appellations suivantes, laquelle décrit le mieux votre orientation sexuelle? 

table(Data$SES7)
CleanData$ses_sexualorientation <- NA 
CleanData$ses_sexualorientation[Data$SES7 == 1] <- 1    # "heterosexual"
CleanData$ses_sexualorientation[Data$SES7 == 2] <- 2    # "homosexual"
CleanData$ses_sexualorientation[Data$SES7 == 3] <- 3    # "bisexual"
CleanData$ses_sexualorientation[Data$SES7 == 4] <- 4    # "other sexual orientation"
table(CleanData$ses_sexualorientation)

CleanData$ses_hetero <- NA 
CleanData$ses_hetero[Data$SES7 == 1] <- 1
CleanData$ses_hetero[Data$SES7 != 1] <- 0
table(CleanData$ses_hetero)

CleanData$ses_gai <- NA 
CleanData$ses_gai[Data$SES7 == 2] <- 1
CleanData$ses_gai[Data$SES7 != 2] <- 0
table(CleanData$ses_gai)

table(Data$SES7)
CleanData$ses_bisex <- NA 
CleanData$ses_bisex[Data$SES7 == 3] <- 1
CleanData$ses_bisex[Data$SES7 != 3] <- 0
table(CleanData$ses_bisex)

table(Data$SES7)
CleanData$ses_sexOri_other <- NA 
CleanData$ses_sexOri_other[Data$SES7 == 4] <- 1
CleanData$ses_sexOri_other[Data$SES7 != 4] <- 0
table(CleanData$ses_sexOri_other)

# Est-ce qu’au moins l'un de vos parents est né à l'extérieur du Canada? 

table(Data$SES8)
CleanData$ses_parentsBornCanada <- NA 
CleanData$ses_parentsBornCanada[Data$SES8 == 1] <- 1 #oui
CleanData$ses_parentsBornCanada[Data$SES8 == 2] <- 0 #oui
table(CleanData$ses_parentsBornCanada)

# Êtes-vous né(e) au Canada?

table(Data$SES9)
CleanData$immigrant <- NA 
CleanData$immigrant[Data$SES9 == 1] <- 0 #oui (NON PAS NÉ(E) AU CANADA)
CleanData$immigrant[Data$SES9 == 2] <- 1 #non
table(CleanData$immigrant)

# Parmi les catégories suivantes, laquelle décrit le mieux votre type d'habitation? 
# 1) Appartement dans un immeuble de moins de cinq étages ; 2) Loft ; 3) Condo ; 4) Tour d'habitation ; 6) Maison individuelle; 7) Maison de ville ; 8) Semi-détaché; 9) Coopérative ; 10) HLM ; 11) Maison mobile (bateau, camionnette, VR, etc.) ; 12) Autre (veuillez préciser)

table(Data$SES10)
CleanData$ses_dwelling <- NA 
CleanData$ses_dwelling[Data$SES10 == 1] <- 1   # "appartment"
#CleanData$ses_dwelling[Data$SES10 == 2] <- "loft"
CleanData$ses_dwelling[Data$SES10 == 3] <- 2    # "condo"
CleanData$ses_dwelling[Data$SES10 == 4] <- 3    # "highrise residential building"
CleanData$ses_dwelling[Data$SES10 == 5] <- 4    # "detached house"
CleanData$ses_dwelling[Data$SES10 == 6] <- 5    # "town house"
CleanData$ses_dwelling[Data$SES10 == 7] <- 6    # "semi-detached house"
CleanData$ses_dwelling[Data$SES10 == 8 | Data$SES10 == 9 | Data$SES10 == 10 | Data$SES10 == 2 | Data$SES10 == 11] <- 7 #"other type of house"
#CleanData$ses_dwelling[Data$SES10 == 9] <- "HLM"
#CleanData$ses_dwelling[Data$SES10 == 10] <- "mobile house"
#CleanData$ses_dwelling[Data$SES10 == 11] <- "other type of house"
table(CleanData$ses_dwelling)


# appartement dans un immeuble de moins de cinq étages
CleanData$ses_dwelling_app <- NA 
CleanData$ses_dwelling_app[Data$SES10 == 1] <- 1
CleanData$ses_dwelling_app[Data$SES10 != 1] <- 0
table(CleanData$ses_dwelling_app)

# loft 
CleanData$ses_dwelling_loft <- NA 
CleanData$ses_dwelling_loft[Data$SES10 == 2] <- 1
CleanData$ses_dwelling_loft[Data$SES10 != 2] <- 0
table(CleanData$ses_dwelling_loft)

# condo
CleanData$ses_dwelling_condo <- NA 
CleanData$ses_dwelling_condo[Data$SES10 == 3] <- 1
CleanData$ses_dwelling_condo[Data$SES10 != 3] <- 0
table(CleanData$ses_dwelling_condo)

# tour d'habitation
CleanData$ses_dwelling_tour <- NA 
CleanData$ses_dwelling_tour[Data$SES10 == 4] <- 1
CleanData$ses_dwelling_tour[Data$SES10 != 4] <- 0
table(CleanData$ses_dwelling_tour)

# maison individuelle 
CleanData$ses_dwelling_detachedHouse <- NA 
CleanData$ses_dwelling_detachedHouse[Data$SES10 == 5] <- 1
CleanData$ses_dwelling_detachedHouse[Data$SES10 != 5] <- 0
table(CleanData$ses_dwelling_detachedHouse)

# maison de ville
CleanData$ses_dwelling_townHouse <- NA 
CleanData$ses_dwelling_townHouse[Data$SES10 == 6] <- 1
CleanData$ses_dwelling_townHouse[Data$SES10 != 6] <- 0
table(CleanData$ses_dwelling_townHouse)

# maison semi-détachée
CleanData$ses_dwelling_semiDetached <- NA 
CleanData$ses_dwelling_semiDetached[Data$SES10 == 7] <- 1
CleanData$ses_dwelling_semiDetached[Data$SES10 != 7] <- 0
table(CleanData$ses_dwelling_semiDetached)

# cooperative 
CleanData$ses_dwelling_coop <- NA 
CleanData$ses_dwelling_coop[Data$SES10 == 8] <- 1
CleanData$ses_dwelling_coop[Data$SES10 != 8] <- 0
table(CleanData$ses_dwelling_coop)

# HLM
CleanData$ses_dwelling_HLM <- NA 
CleanData$ses_dwelling_HLM[Data$SES10 == 9] <- 1
CleanData$ses_dwelling_HLM[Data$SES10 != 9] <- 0
table(CleanData$ses_dwelling_HLM)

# maison mobile
CleanData$ses_dwelling_mobile <- NA 
CleanData$ses_dwelling_mobile[Data$SES10 == 10] <- 1
CleanData$ses_dwelling_mobile[Data$SES10 != 10] <- 0
table(CleanData$ses_dwelling_mobile)

# autre
CleanData$ses_dwelling_other <- NA 
CleanData$ses_dwelling_other[Data$SES10 == 11] <- 1
CleanData$ses_dwelling_other[Data$SES10 != 11] <- 0
table(CleanData$ses_dwelling_other)


# maison individuelle, maison semi-détachée et maison de ville mises ensemble 
CleanData$ses_dwelling_house <- NA 
CleanData$ses_dwelling_house[Data$SES10 == 5 | Data$SES10 == 6 | Data$SES10 == 7] <- 1
CleanData$ses_dwelling_house[Data$SES10 != 5 & Data$SES10 != 6 & Data$SES10 != 7] <- 0
table(CleanData$ses_dwelling_house)


# Si vous aviez le choix d'aller en vacances où vous voulez, quelle serait votre destination préférée?
table(Data$O_B4)


text <- Data$O_B4
# Set the text to lowercase
text <- tolower(text)
# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)
# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)
# Put the data to a new column
Data["fix_text"] <- text
head(Data$fix_text, 6000)

table(Data$fix_text)

CleanData$visit_Hawaii <- NA 
CleanData$visit_Hawaii[Data$fix_text == "hawa" | Data$fix_text == "hawai" |  Data$fix_text == "hawaii"] <- 1
CleanData$visit_Hawaii[Data$fix_text != "hawa" & Data$fix_text != "hawai" &  Data$fix_text != "hawaii"] <- 0
table(CleanData$visit_Hawaii)

CleanData$visit_Inde <- NA 
CleanData$visit_Inde[Data$fix_text == "inde" | Data$fix_text == "india" |  Data$fix_text == "hawaii"] <- 1
CleanData$visit_Inde[Data$fix_text != "inde" & Data$fix_text != "india" &  Data$fix_text != "hawaii"] <- 0
table(CleanData$visit_Inde)


CleanData$visit_USA <- NA 
CleanData$visit_USA[Data$fix_text == "etas unis" | Data$fix_text == "etats unis" |  Data$fix_text == "florida" |
               Data$fix_text == "floride" |  Data$fix_text == "etats unis" | Data$fix_text == "las vegas"| 
               Data$fix_text == "los angeles" | Data$fix_text == "maine" | Data$fix_text == "miami" | 
               Data$fix_text == "new york" | Data$fix_text == "new york city" | Data$fix_text == "usa" | 
               Data$fix_text == "vegas"] <- 1
CleanData$visit_USA[Data$fix_text != "etas unis" & Data$fix_text != "etats unis" &  Data$fix_text != "florida" &
                 Data$fix_text != "floride" &  Data$fix_text != "etats unis" & Data$fix_text != "las vegas"&
                 Data$fix_text != "los angeles" & Data$fix_text != "maine" & Data$fix_text != "miami" &
                 Data$fix_text != "new york" & Data$fix_text != "new york city" & Data$fix_text != "usa"&
                 Data$fix_text != "vegas"] <- 0
table(CleanData$visit_USA)

CleanData$visit_NZ <- NA 
CleanData$visit_NZ[Data$fix_text == "new zealand" | Data$fix_text == "nouvelle zlande"] <- 1
CleanData$visit_NZ[Data$fix_text != "new zealand" & Data$fix_text != "nouvelle zlande"] <- 0
table(CleanData$visit_NZ)

CleanData$visit_Martinique <- NA 
CleanData$visit_Martinique[Data$fix_text == "martinique"] <- 1
CleanData$visit_Martinique[Data$fix_text != "martinique"] <- 0
table(CleanData$visit_Martinique)

CleanData$visit_Mexique <- NA 
CleanData$visit_Mexique[Data$fix_text == "mexico" |
                   Data$fix_text == "mexic"  |
                   Data$fix_text == "mexique"] <- 1
CleanData$visit_Mexique[Data$fix_text != "mexico" &
                   Data$fix_text != "mexic"  &
                   Data$fix_text != "mexique"] <- - 0
table(CleanData$visit_Mexique)

CleanData$visit_Maldives <- NA 
CleanData$visit_Maldives[Data$fix_text == "maldives" | Data$fix_text == "maldive" |  Data$fix_text == "maldivess"] <- 1
CleanData$visit_Maldives[Data$fix_text != "maldives" & Data$fix_text != "maldive" &  Data$fix_text != "maldivess"] <- 0
table(CleanData$visit_Maldives)

CleanData$visit_Allemagne <- NA 
CleanData$visit_Allemagne[Data$fix_text == "germany" | Data$fix_text == "allemagne"] <- 1
CleanData$visit_Allemagne[Data$fix_text != "germany" & Data$fix_text != "allemagne"] <- 0
table(CleanData$visit_Allemagne)

CleanData$visit_Grece <- NA 
CleanData$visit_Grece[Data$fix_text == "grece" | Data$fix_text == "grce" | Data$fix_text == "greece" | 
                 Data$fix_text == "la grce"] <- 1
CleanData$visit_Grece[Data$fix_text != "grece"  & 
                 Data$fix_text != "grce"   & 
                 Data$fix_text != "greece" & 
                 Data$fix_text != "la grce"] <- 0
table(CleanData$visit_Grece)

CleanData$visit_Ireland <- NA 
CleanData$visit_Ireland[Data$fix_text == "ireland" | Data$fix_text == "irelande"] <- 1
CleanData$visit_Ireland[Data$fix_text != "ireland" & Data$fix_text != "irelande"] <- 0
table(CleanData$visit_Ireland)

CleanData$visit_Islande <- NA 
CleanData$visit_Islande[Data$fix_text == "islande"] <- 1
CleanData$visit_Islande[Data$fix_text != "islande"] <- 0
table(CleanData$visit_Islande)

CleanData$visit_Italie <- NA 
CleanData$visit_Italie[Data$fix_text == "italie" | Data$fix_text == "italiy" | Data$fix_text == "rome"] <- 1
CleanData$visit_Italie[Data$fix_text != "italie" & Data$fix_text != "italiy" & Data$fix_text != "rome"] <- 0
table(CleanData$visit_Italie)

CleanData$visit_Israel <- NA 
CleanData$visit_Israel[Data$fix_text == "israel"] <- 1
CleanData$visit_Israel[Data$fix_text != "israel"] <- 0
table(CleanData$visit_Israel)

CleanData$visit_UK <- NA
CleanData$visit_UK[Data$fix_text == "london"| 
              Data$fix_text == "london england" | 
              Data$fix_text == "londres" | 
              Data$fix_text == "royaume uni" |
              Data$fix_text == "u k" | 
              Data$fix_text == "uk" |
              Data$fix_text == "united kingdom"] <- 1
CleanData$visit_UK[Data$fix_text != "london" &
                Data$fix_text != "london england" &
                Data$fix_text != "londres" & 
                Data$fix_text != "royaume uni" &
                Data$fix_text != "u k" & 
                Data$fix_text != "uk" &
                Data$fix_text != "united kingdom"] <- 0
table(CleanData$visit_UK)

CleanData$visit_Europe <- NA 
CleanData$visit_Europe[Data$fix_text == "europe" | Data$fix_text == "europe de l ouest" |  Data$fix_text == "europe de l est" |
                  Data$fix_text == "europe parks" | Data$fix_text == "france" | Data$fix_text == "germany" |
                  Data$fix_text == "grce" | Data$fix_text == "grece" | Data$fix_text == "greece" | Data$fix_text == "ireland" | 
                  Data$fix_text == "irelande" | Data$fix_text == "italie" | Data$fix_text == "italiy" | Data$fix_text == "london"| 
                  Data$fix_text == "london england" | Data$fix_text == "londres" | Data$fix_text == "portugal" |
                  Data$fix_text == "prague" | Data$fix_text == "rome" | Data$fix_text == "spain" | Data$fix_text == "suisse" | 
                  Data$fix_text == "sweden" | Data$fix_text == "switzerland" | Data$fix_text == "wales" | Data$fix_text == "allemagne"] <- 1
CleanData$visit_Europe[Data$fix_text != "europe" & Data$fix_text != "europe de l ouest" &
                  Data$fix_text != "europe de l est" & Data$fix_text != "europe parks" &
                  Data$fix_text != "france" & Data$fix_text != "germany" &
                  Data$fix_text != "grce" & Data$fix_text != "grece" &
                  Data$fix_text != "greece" & Data$fix_text != "ireland" &
                    Data$fix_text != "irelande" | Data$fix_text != "italie" &
                    Data$fix_text != "italiy" & Data$fix_text == "london" &
                    Data$fix_text != "london england" & Data$fix_text != "londres" & Data$fix_text != "portugal" &
                    Data$fix_text != "prague" & Data$fix_text != "rome" & Data$fix_text == "spain" & Data$fix_text != "suisse" &
                    Data$fix_text != "sweden" & Data$fix_text != "switzerland" & Data$fix_text != "wales" & Data$fix_text != "allemagne"] <- 0
table(CleanData$visit_Europe)

CleanData$visit_Quebec <- NA 
CleanData$visit_Quebec[Data$fix_text == "gaspesie" | Data$fix_text == "gaspsi" |  Data$fix_text == "gaspe" | 
                  Data$fix_text == "gaspsie" | Data$fix_text == "gatineau" | Data$fix_text == "gatineau ottawa" | 
                  Data$fix_text == "lac st jean" | Data$fix_text== "les de la madeleine" | Data$fix_text == "le de la madeleine" | 
                  Data$fix_text == "montreal" | Data$fix_text == "qubec" | Data$fix_text == "quebec" | Data$fix_text == "quebec city"] <- 1
CleanData$visit_Quebec[Data$fix_text != "gaspesie" & Data$fix_text != "gaspsi" &  Data$fix_text != "gaspe" &
                    Data$fix_text != "gaspsie" & Data$fix_text != "gatineau" & Data$fix_text != "gatineau ottawa" & 
                    Data$fix_text != "lac st jean" & Data$fix_text!= "les de la madeleine" & Data$fix_text != "le de la madeleine" & 
                    Data$fix_text != "montreal" & Data$fix_text != "qubec" & Data$fix_text != "quebec" & Data$fix_text != "quebec city"] <- 0
table(CleanData$visit_Quebec)

CleanData$visit_Canada <- NA 
CleanData$visit_Canada[Data$fix_text == "faire le canada" | Data$fix_text == "newfoundland" | Data$fix_text == "niagara falls" | 
                    Data$fix_text == "nouveau brunswick" | Data$fix_text == "nouvelle cosse" | Data$fix_text == "nova scoatia" |
                    Data$fix_text == "ontario" | Data$fix_text == "ouest canadien" | Data$fix_text == "pei" | 
                    Data$fix_text == "thunder bay" | Data$fix_text == "toronto" | Data$fix_text == "vancouver" | 
                    Data$fix_text == "vancouver island" | Data$fix_text == "vancouvert" | Data$fix_text == "yukon" | 
                    Data$fix_text == "gaspesie" | Data$fix_text == "gaspsi" |  Data$fix_text == "gaspe" | 
                    Data$fix_text == "gaspsie" | Data$fix_text == "gatineau" | Data$fix_text == "gatineau ottawa" | 
                    Data$fix_text == "lac st jean" | Data$fix_text== "les de la madeleine" | Data$fix_text == "le de la madeleine" | 
                    Data$fix_text == "montreal" | Data$fix_text == "qubec" | Data$fix_text == "quebec" |
                    Data$fix_text == "quebec city" |  Data$fix_text == "canada"] <- 1
CleanData$visit_Canada[Data$fix_text != "faire le canada" & Data$fix_text != "newfoundland" & Data$fix_text != "niagara falls" & 
                         Data$fix_text != "nouveau brunswick" & Data$fix_text != "nouvelle cosse" & Data$fix_text != "nova scoatia" &
                         Data$fix_text != "ontario" & Data$fix_text != "ouest canadien" & Data$fix_text != "pei" & 
                         Data$fix_text != "thunder bay" & Data$fix_text != "toronto" & Data$fix_text != "vancouver" &
                         Data$fix_text != "vancouver island" & Data$fix_text != "vancouvert" & Data$fix_text != "yukon" &
                         Data$fix_text != "gaspesie" & Data$fix_text != "gaspsi" &  Data$fix_text != "gaspe" & 
                         Data$fix_text != "gaspsie" & Data$fix_text != "gatineau" & Data$fix_text != "gatineau ottawa" & 
                         Data$fix_text != "lac st jean" & Data$fix_text!= "les de la madeleine" & Data$fix_text != "le de la madeleine" &
                         Data$fix_text != "montreal" & Data$fix_text != "qubec" & Data$fix_text != "quebec" &
                         Data$fix_text != "quebec city" &  Data$fix_text != "canada"] <- 0
table(CleanData$visit_Canada)

CleanData$visit_Egypte <- NA 
CleanData$visit_Egypte[Data$fix_text == "gypte" | Data$fix_text == "egypt"] <- 1
CleanData$visit_Egypte[Data$fix_text != "gypte" & Data$fix_text != "egypt"] <- 0
table(CleanData$visit_Egypte)

CleanData$visit_Ecosse <- NA 
CleanData$visit_Ecosse[Data$fix_text == "scotland" | Data$fix_text == "cosse" | Data$fix_text == "ecosse"] <- 1
CleanData$visit_Ecosse[Data$fix_text != "scotland" & Data$fix_text != "cosse" & Data$fix_text != "ecosse"] <- 0
table(CleanData$visit_Ecosse)

CleanData$visit_Tahiti <- NA 
CleanData$visit_Tahiti[Data$fix_text == "tahiti"] <- 1
CleanData$visit_Tahiti[Data$fix_text != "tahiti"] <- 0
table(CleanData$visit_Tahiti)

CleanData$visit_Thailande <- NA 
CleanData$visit_Thailande[Data$fix_text == "thailande" | Data$fix_text == "thailand" | Data$fix_text == "thalande"] <- 1
CleanData$visit_Thailande[Data$fix_text != "thailande" & Data$fix_text != "thailand" & Data$fix_text != "thalande"] <- 0
table(CleanData$visit_Thailande)

CleanData$visit_Fiji <- NA 
CleanData$visit_Fiji[Data$fix_text == "fiji" | Data$fix_text == "fidji"] <- 1
CleanData$visit_Fiji[Data$fix_text != "fiji" & Data$fix_text != "fidji"] <- 0
table(CleanData$visit_Fiji)

CleanData$visit_France <- NA 
CleanData$visit_France[Data$fix_text == "france" | Data$fix_text == "la france" | Data$fix_text == "paris"] <- 1
CleanData$visit_France[Data$fix_text != "france" & Data$fix_text != "la france" & Data$fix_text != "paris"] <- 0
table(CleanData$visit_France)

CleanData$visit_Jamaique <- NA 
CleanData$visit_Jamaique[Data$fix_text == "jamaica" | Data$fix_text == "jamaique" | Data$fix_text == "jamaque"] <- 1
CleanData$visit_Jamaique[Data$fix_text != "jamaica" & Data$fix_text != "jamaique" & Data$fix_text != "jamaque"] <- 0
table(CleanData$visit_Jamaique)

CleanData$visit_Japon <- NA 
CleanData$visit_Japon[Data$fix_text == "japon" | Data$fix_text == "japan" | Data$fix_text == "le japon" | 
                   Data$fix_text == "tokyo" | Data$fix_text == "okyo japan"] <- 1
CleanData$visit_Japon[Data$fix_text != "japon" & Data$fix_text != "japan" & Data$fix_text != "le japon" & 
                 Data$fix_text != "tokyo" & Data$fix_text != "okyo japan"] <- 0
table(CleanData$visit_Japon)

CleanData$visit_Australie <- NA 
CleanData$visit_Australie[Data$fix_text == "australia" | Data$fix_text == "australie"] <- 1
CleanData$visit_Australie[Data$fix_text != "australia" & Data$fix_text != "australie"] <- 0
table(CleanData$visit_Australie)

######      

table(Data$O_C2)


text2 <- Data$O_C2
# Set the text to lowercase
text2 <- tolower(text2)
# Remove mentions, urls, emojis, numbers, punctuations, etc.
text2 <- gsub("@\\w+", "", text2)
text2 <- gsub("https?://.+", "", text2)
text2 <- gsub("\\d+\\w*\\d*", "", text2)
text2 <- gsub("#\\w+", "", text2)
text2 <- gsub("[^\x01-\x7F]", "", text2)
text2 <- gsub("[[:punct:]]", " ", text2)
# Remove spaces and newlines
text2 <- gsub("\n", " ", text2)
text2 <- gsub("^\\s+", "", text2)
text2 <- gsub("\\s+$", "", text2)
text2 <- gsub("[ |\t]+", " ", text2)
# Put the data to a new column
Data["fix_text2"] <- text2
# head(Data$fix_text2, 6000)

table(Data$fix_text2)

CleanData$car_VUS <- NA
CleanData$car_VUS[Data$fix_text2 == "acadia" | Data$fix_text2 == "acura mdx" | Data$fix_text2 == "acura rdx" |
                    Data$fix_text2 == "buick enclave" | Data$fix_text2 == "buick escape" | Data$fix_text2 == "buick encore" | 
                    Data$fix_text2 == "cadillac srx" | Data$fix_text2 == "chev equinox" | Data$fix_text2 == "chev trax" |
                    Data$fix_text2 == "chevrolet equinox" | Data$fix_text2 == "chevrolet equinox" | Data$fix_text2 == "chevy equinox" |
                    Data$fix_text2 == "dodge durango" | Data$fix_text2 == "dodge nitro" | Data$fix_text2 == "ford edge" |
                    Data$fix_text2 == "ford escape" | Data$fix_text2 == "ford explorer" | Data$fix_text2 == "gmc acadia" | 
                    Data$fix_text2 == "gmc terrain" | Data$fix_text2 == "honda crv" | Data$fix_text2 == "honda cr v" |
                    Data$fix_text2 == "honda hrv" | Data$fix_text2 == "hyundai kona" | Data$fix_text2 == "hyndai sante fe" |
                    Data$fix_text2 == "hyundai tucson" | Data$fix_text2 == "hyundai tuscon" | Data$fix_text2 == "infiniti qx" |
                    Data$fix_text2 == "jeep cherokee" | Data$fix_text2 == "jeep compass" | Data$fix_text2 == "jeep grand cherokee" |
                    Data$fix_text2 == "kia sorento" | Data$fix_text2 == "kia sorrento" | Data$fix_text2 == "kia soul" | 
                    Data$fix_text2 == "kia sportage" | Data$fix_text2 == "mazda cx" | Data$fix_text2 == "mitsubishi outlander" |
                    Data$fix_text2 == "nissan kicks" | Data$fix_text2 == "nissan qasqai" | Data$fix_text2 == "nissan rogue" |
                    Data$fix_text2 == "subaru forester" | Data$fix_text2 == "subaru outback" | Data$fix_text2 == "toyota highlander" |
                    Data$fix_text2 == "volkwagen tiguan" |Data$fix_text2 == "volvo xc"] <- 1
CleanData$car_VUS[Data$fix_text2 != "acadia" & 
                  Data$fix_text2 != "acura mdx" & 
                  Data$fix_text2 != "acura rdx" &
                  Data$fix_text2 != "buick enclave" &
                  Data$fix_text2 != "buick escape" &
                  Data$fix_text2 != "buick encore" &
                  Data$fix_text2 != "cadillac srx" &
                  Data$fix_text2 != "chev equinox" &
                  Data$fix_text2 != "chev trax" &
                  Data$fix_text2 != "chevrolet equinox"& 
                  Data$fix_text2 != "chevy equinox" &
                  Data$fix_text2 != "dodge durango" & 
                  Data$fix_text2 != "dodge nitro" & 
                  Data$fix_text2 != "ford edge" &
                  Data$fix_text2 != "ford escape" & 
                  Data$fix_text2 != "ford explorer" &
                  Data$fix_text2 != "gmc acadia" & 
                  Data$fix_text2 != "gmc terrain" &
                  Data$fix_text2 != "honda crv" & 
                  Data$fix_text2 != "honda cr v" &
                  Data$fix_text2 != "honda hrv" & 
                  Data$fix_text2 != "hyundai kona" &
                  Data$fix_text2 != "hyndai sante fe" &
                  Data$fix_text2 != "hyundai tucson" &
                  Data$fix_text2 != "hyundai tuscon" & 
                  Data$fix_text2 != "infiniti qx" &
                  Data$fix_text2 != "jeep cherokee" & 
                  Data$fix_text2 != "jeep compass" &
                  Data$fix_text2 != "jeep grand cherokee" &
                  Data$fix_text2 != "kia sorento" & 
                  Data$fix_text2 != "kia sorrento" &
                  Data$fix_text2 != "kia soul" & 
                  Data$fix_text2 != "kia sportage" & 
                  Data$fix_text2 != "mazda cx" & 
                  Data$fix_text2 != "mitsubishi outlander" &
                  Data$fix_text2 != "nissan kicks" &
                  Data$fix_text2 != "nissan qasqai" & 
                  Data$fix_text2 != "nissan rogue" &
                  Data$fix_text2 != "subaru forester" &
                  Data$fix_text2 != "subaru outback" & 
                  Data$fix_text2 != "toyota highlander" &
                  Data$fix_text2 != "volkwagen tiguan" &
                  Data$fix_text2 != "volvo xc"] <- 0
table(CleanData$car_VUS)

CleanData$car_smol <- NA
CleanData$car_smol[Data$fix_text2 == "accent" | Data$fix_text2 == "accent huyndai" | Data$fix_text2 == "accent hyundai" | 
                     Data$fix_text2 == "acusa csx " | Data$fix_text2 == "acura ilx" | Data$fix_text2 == "acura tl" |
                     Data$fix_text2 == "buick verano" | Data$fix_text2 == "cadillac ats" | Data$fix_text2 == "cadicallac xt" |
                     Data$fix_text2 == "chev spark" | Data$fix_text2 == "chevrolet cobalt" | Data$fix_text2 == "" |
                     Data$fix_text2 == "chevrolet colt" | Data$fix_text2 == "" | Data$fix_text2 == "chevrolet cruz" |
                     Data$fix_text2 == "chevrolet cruze" | Data$fix_text2 == "chevrolet impala" | Data$fix_text2 == "chevrolet malibu" |
                     Data$fix_text2 == "chevrolet sonic" | Data$fix_text2 == "chevrolet volt" | Data$fix_text2 == "ford fiesta" |
                     Data$fix_text2 == "ford focus" | Data$fix_text2 == "ford fusion" | Data$fix_text2 == "honda accord" |
                     Data$fix_text2 == "honda civic" | Data$fix_text2 == "honda civi" | Data$fix_text2 == "honda fit" |
                     Data$fix_text2 == "honda pilot" | Data$fix_text2 == "hyundai accent" | Data$fix_text2 == "hyundai elantra" |
                     Data$fix_text2 == "hyundai elentra" | Data$fix_text2 == "" | Data$fix_text2 == "hyundai sonata" |
                     Data$fix_text2 == "hyundai sonota" | Data$fix_text2 == "kia forte" | Data$fix_text2 == "" |
                     Data$fix_text2 == "kia optima" | Data$fix_text2 == "kia rio" | Data$fix_text2 == "kia rondo" | 
                     Data$fix_text2 == "mitsubishi lancer" | Data$fix_text2 == "nissan altima" | Data$fix_text2 == "nissan leaf" |
                     Data$fix_text2 == "nissan micra" | Data$fix_text2 == "nissan sentra" | Data$fix_text2 == "saturn ion" |
                     Data$fix_text2 == "subaru impreza" | Data$fix_text2 == "toyota camry" | Data$fix_text2 == "toyota corolla" |
                     Data$fix_text2 == "toyota matrix" | Data$fix_text2 == "" | Data$fix_text2 == "toyota prius" |
                     Data$fix_text2 == "toyota yaris" | Data$fix_text2 == "volkswagen golf"] <- 1
CleanData$car_smol[Data$fix_text2 != "accent" & 
                     Data$fix_text2 != "accent huyndai" & 
                     Data$fix_text2 != "accent hyundai" & 
                     Data$fix_text2 != "acusa csx " &
                     Data$fix_text2 != "acura ilx" &
                     Data$fix_text2 != "acura tl" &
                     Data$fix_text2 != "buick verano" &
                     Data$fix_text2 != "cadillac ats" & 
                     Data$fix_text2 != "cadicallac xt" &
                     Data$fix_text2 != "chev spark" &
                     Data$fix_text2 != "chevrolet cobalt" &
                     Data$fix_text2 != "chevrolet colt" & 
                     Data$fix_text2 != "chevrolet cruz" &
                     Data$fix_text2 != "chevrolet cruze" & 
                     Data$fix_text2 != "chevrolet impala" &
                     Data$fix_text2 != "chevrolet malibu" &
                     Data$fix_text2 != "chevrolet sonic" & 
                     Data$fix_text2 != "chevrolet volt" &
                     Data$fix_text2 != "ford fiesta" &
                     Data$fix_text2 != "ford focus" & 
                     Data$fix_text2 != "ford fusion" & 
                     Data$fix_text2 != "honda accord" &
                     Data$fix_text2 != "honda civic" &
                     Data$fix_text2 != "honda civi" & 
                     Data$fix_text2 != "honda fit" &
                     Data$fix_text2 != "honda pilot" & 
                     Data$fix_text2 != "hyundai accent" & 
                     Data$fix_text2 != "hyundai elantra" &
                     Data$fix_text2 != "hyundai elentra" & 
                     Data$fix_text2 != "hyundai sonata" &
                     Data$fix_text2 != "hyundai sonota" &
                     Data$fix_text2 != "kia forte" & 
                     Data$fix_text2 != "kia optima" &
                     Data$fix_text2 != "kia rio" &
                     Data$fix_text2 != "kia rondo" & 
                     Data$fix_text2 != "mitsubishi lancer" & 
                     Data$fix_text2 != "nissan altima" &
                     Data$fix_text2 != "nissan leaf" &
                     Data$fix_text2 != "nissan micra" & 
                     Data$fix_text2 != "nissan sentra" & 
                     Data$fix_text2 != "saturn ion" &
                     Data$fix_text2 != "subaru impreza" & 
                     Data$fix_text2 != "toyota camry" & 
                     Data$fix_text2 != "toyota corolla" &
                     Data$fix_text2 != "toyota matrix" & 
                     Data$fix_text2 != "toyota prius" &
                     Data$fix_text2 != "toyota yaris" & 
                     Data$fix_text2 != "volkswagen golf"] <- 0
table(CleanData$car_smol)

CleanData$car_pickup <- NA
CleanData$car_pickup[Data$fix_text2 == "camion gmc" | Data$fix_text2 == "chev silverado" | Data$fix_text2 == "chevrolet silverado" |
                       Data$fix_text2 == "dodge ram" | Data$fix_text2 == "gmc canyon" | Data$fix_text2 == "gmc sierra"] <- 1
CleanData$car_pickup[Data$fix_text2 != "camion gmc" &
                     Data$fix_text2 != "chev silverado" & 
                     Data$fix_text2 != "chevrolet silverado" &
                     Data$fix_text2 != "dodge ram" &
                     Data$fix_text2 != "gmc canyon" & 
                     Data$fix_text2 != "gmc sierra"] <- 0
table(CleanData$car_pickup)

CleanData$car_pickup2 <- NA
CleanData$car_pickup2[Data$fix_text2 == "camion gmc" | Data$fix_text2 == "chev silverado" | Data$fix_text2 == "chevrolet silverado" |
                       Data$fix_text2 == "dodge ram" | Data$fix_text2 == "gmc canyon" | Data$fix_text2 == "gmc sierra"] <- 2
CleanData$car_pickup2[Data$fix_text2 != "camion gmc" &
                       Data$fix_text2 != "chev silverado" & 
                       Data$fix_text2 != "chevrolet silverado" &
                       Data$fix_text2 != "dodge ram" &
                       Data$fix_text2 != "gmc canyon" & 
                       Data$fix_text2 != "gmc sierra"] <- 1
table(CleanData$car_pickup2)


CleanData$car_van <- NA
CleanData$car_van[Data$fix_text2 == "caravan" | Data$fix_text2 == "dodge caravan" | Data$fix_text2 == "dodge grand caravan" |
                    Data$fix_text2 == "dodge journey" | Data$fix_text2 == "ford f" | Data$fix_text2 == "onda odyssey"] <- 1
CleanData$car_van[Data$fix_text2 != "caravan" &
                    Data$fix_text2 != "dodge caravan" &
                    Data$fix_text2 != "dodge grand caravan" &
                    Data$fix_text2 != "dodge journey" &
                    Data$fix_text2 != "ford f" & 
                    Data$fix_text2 != "onda odyssey"] <- 0
table(CleanData$car_van)


############

Data$fav_music <- Data$O_G1
Data$fav_movie <- Data$O_G3


MusicPoncet <- Data %>%
  dplyr::select(CASEID,fav_movie,fav_music)

for (i in 1:nrow(MusicPoncet)) {
  if (is.na(MusicPoncet$fav_movie[i]) & !is.na(MusicPoncet$fav_music[i])) {
    MusicPoncet$fav_movie[i] <- "None"
  }
  if (is.na(MusicPoncet$fav_music[i]) & !is.na(MusicPoncet$fav_movie[i])) {
    MusicPoncet$fav_movie[i] <- "None"
  }
  print(i)
}

MusicPoncet <- MusicPoncet %>% na.omit()



MusicPoncet$text3 <- MusicPoncet$fav_movie
MusicPoncet$text3 <- tolower(MusicPoncet$text3)
# Re3ove mentions, urls, emojis, numbers, punctuations, etc.
MusicPoncet$text3 <- gsub("@\\w+", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("https?://.+", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("\\d+\\w*\\d*", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("#\\w+", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("[^\x01-\x7F]", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("[[:punct:]]", " ", MusicPoncet$text3)
MusicPoncet$# Re3ove spaces and newlines
MusicPoncet$text3 <- gsub("\n", " ", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("^\\s+", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("\\s+$", "", MusicPoncet$text3)
MusicPoncet$text3 <- gsub("[ |\t]+", " ", MusicPoncet$text3)
# Put the data to a new column
MusicPoncet["fix_text3"] <- MusicPoncet$text3

MusicPoncet$fav_movie_toClean <- MusicPoncet$text3

view(MusicPoncet$fav_movie_toClean)
table(MusicPoncet$fav_movie_toClean)
#options(max.print=1000)


# 
# key <- '8af30230' # à récupérer sur http://www.omdbapi.com/apikey.aspx
# # note: 1000 calls par jour seulement
# # Ne pas modifier ces paramètres
# url <- 'http://www.omdbapi.com/'
# search_movie <- function(search_term)
# {
#   result <- GET(url, query = list(
#     apikey = key,
#     s = search_term,
#     type = 'movie'
#   ))
#   return(result)
# }
# # Exemple d'utilisation
# die_hard_result <- search_movie('die hard')
# print(die_hard_result$status_code) # should be 200 or something didn't work
# data <- content(die_hard_result, "parsed")
# 

MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "a space oddysey"] <- "a space odyssey"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "a star is borne" | 
                              MusicPoncet$fav_movie_toClean == "a star is born with lady gaga"] <- "a star is born"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "al"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "alien" |
                              MusicPoncet$fav_movie_toClean == "aliens and riddick"] <- "aliens"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "all of them"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "all about my mother pedro almodovar"] <- "all about my mother"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "any of the james bond s"] <- "james bond"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "aucun"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "aucun en particulier"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "avendgers" | 
                                MusicPoncet$fav_movie_toClean == "avengers end game" | 
                                MusicPoncet$fav_movie_toClean == "avengers series" |
                                MusicPoncet$fav_movie_toClean == "avengers infinity war"] <- "avengers"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "beaucoup de bruit pour rien"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "big libowski"] <- "the big lebowski"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "bridemaids"] <- "bridesmaids"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "ca" | 
                                MusicPoncet$fav_movie_toClean == "can t choose" | 
                                MusicPoncet$fav_movie_toClean == "can t remember" | 
                                MusicPoncet$fav_movie_toClean == "can t recall " | 
                                MusicPoncet$fav_movie_toClean == "can t think " | 
                                MusicPoncet$fav_movie_toClean == "cant just pick one"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "cinma paradiso"] <- "cinema paradiso"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "danse lassive" | 
                                MusicPoncet$fav_movie_toClean == "danse lascive"] <- "dirty dancing"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "dd" |
                                MusicPoncet$fav_movie_toClean == "ddfhjfj" |
                                MusicPoncet$fav_movie_toClean == "depends" | 
                                MusicPoncet$fav_movie_toClean == "dit tu dancing" | 
                                MusicPoncet$fav_movie_toClean == "disterbia" | 
                                MusicPoncet$fav_movie_toClean == "do not watch" | 
                                MusicPoncet$fav_movie_toClean == "don t have on" | 
                                MusicPoncet$fav_movie_toClean == "don t have one" | 
                                MusicPoncet$fav_movie_toClean == "ddfhjfj" | 
                                MusicPoncet$fav_movie_toClean == "don t know" | 
                                MusicPoncet$fav_movie_toClean == "don t watch movies" |
                                MusicPoncet$fav_movie_toClean == "dr no" | 
                                MusicPoncet$fav_movie_toClean == "et" | 
                                MusicPoncet$fav_movie_toClean == "ewtn"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "back to the futur" | 
                                MusicPoncet$fav_movie_toClean == "retour vers le future trilogie" |
                                MusicPoncet$fav_movie_toClean == "retour vers le futur" |
                                MusicPoncet$fav_movie_toClean == "trilogie back to the future"] <- "back to the future"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "fast and ferouis" | 
                              MusicPoncet$fav_movie_toClean == "fast and furious series" ] <- "fast n furious"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "ff" | 
                                MusicPoncet$fav_movie_toClean == "fgu" | 
                                MusicPoncet$fav_movie_toClean == "film d actions"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "fifthy shades of grey"] <- "fifty shades of grey"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "field of dreams"] <- "fields of dreams"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "forest gump"] <- "forrest gump"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "gdg"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "ground hog day"] <- "groundhog day"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "harry poter" | 
                              MusicPoncet$fav_movie_toClean == "harry potter and the half blood prince" |
                              MusicPoncet$fav_movie_toClean == "harry potter et la coupe de feu" |
                                MusicPoncet$fav_movie_toClean == "harry potter series"] <- "harry potter"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "he" | 
                                MusicPoncet$fav_movie_toClean == "hj" | 
                                MusicPoncet$fav_movie_toClean == "horror" | 
                                MusicPoncet$fav_movie_toClean == "i don t care to watch movies but i recently saw the dig"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "i dont know" | 
                                MusicPoncet$fav_movie_toClean == "i don t have a single favorite" | 
                                MusicPoncet$fav_movie_toClean == "i don t have any favorites" | 
                                MusicPoncet$fav_movie_toClean == "i don t have one"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "ice age and sequels"] <- "ice age"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "idiots" | 
                                MusicPoncet$fav_movie_toClean == "idk" | 
                                MusicPoncet$fav_movie_toClean == "j en coute pas" | 
                                MusicPoncet$fav_movie_toClean == "j en regarde tres peu"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "il pleut des oisesux"] <- "il pleuvait des oiseaux"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "ice age and sequels"] <- "ice age"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "je connais pas le titre" | 
                                MusicPoncet$fav_movie_toClean == "je n ai pas un film prfr" | 
                                MusicPoncet$fav_movie_toClean == "je n coute pas de films" | 
                                MusicPoncet$fav_movie_toClean == "je naime pas les films"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "je ne sais pas"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "l odysse de l espace" |
                              MusicPoncet$fav_movie_toClean == "odissey de l espace" |
                                MusicPoncet$fav_movie_toClean == "odysse"] <- "2001, l'Odyssée de l'espace"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "la machine remonter le temps premire version"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "la saga star wars" | 
                                MusicPoncet$fav_movie_toClean == "trilogie star wars"] <- "star wars"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "labrinth"] <- "labyrinth"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lala land"] <- "la la land"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "les oiseaux the birds hithcok"] <- "les oiseaux"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "les pages de notre amour et ligne verte"] <- "les pages de notre amour"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lord of the rings"] <- "le seigneur des anneaux"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lord of the rings fellowship of the ring" |
                                MusicPoncet$fav_movie_toClean == "lots of the rings" | 
                                MusicPoncet$fav_movie_toClean == "seigneur des anneaux"] <- "le seigneur des anneaux"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lord of the rings" | 
                                MusicPoncet$fav_movie_toClean == " the lord of the rings"] <- "le seigneur des anneaux"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "live and let die james bond"] <- "james bond"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "marvel"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "marvel iron man" | 
                                MusicPoncet$fav_movie_toClean == "lhomme dacier"] <- "iron man"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "mile"] <- "8 mile"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "mommy de xavier dolan"] <- "mommy"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "mon fantme d amour"] <- "mon fantome d amour"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "n a" | 
                                MusicPoncet$fav_movie_toClean == "na" | 
                                MusicPoncet$fav_movie_toClean == "ne sait pas"| 
                                MusicPoncet$fav_movie_toClean == "ne sais pas" | 
                                MusicPoncet$fav_movie_toClean == "no" | 
                                MusicPoncet$fav_movie_toClean == "non" | 
                                MusicPoncet$fav_movie_toClean == "none" | 
                                MusicPoncet$fav_movie_toClean == "" | 
                                MusicPoncet$fav_movie_toClean == "no fav" | 
                                MusicPoncet$fav_movie_toClean == "no favourite" | 
                                MusicPoncet$fav_movie_toClean == "none in particular" | 
                                MusicPoncet$fav_movie_toClean == "not sure" | 
                                MusicPoncet$fav_movie_toClean == "nothing in particular"  | 
                                MusicPoncet$fav_movie_toClean == "plusieurs"  | 
                                MusicPoncet$fav_movie_toClean == "plusieres" | 
                                MusicPoncet$fav_movie_toClean == "rien en particulier"| 
                                MusicPoncet$fav_movie_toClean == "sais pas"
                              | MusicPoncet$fav_movie_toClean == "sfsf"
                              | MusicPoncet$fav_movie_toClean == "srhshr"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "notes book" | 
                                MusicPoncet$fav_movie_toClean == "notebook" | 
                                MusicPoncet$fav_movie_toClean == "the note book"] <- "the notebook"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "parain" | 
                              MusicPoncet$fav_movie_toClean == "the godfather" |
                                MusicPoncet$fav_movie_toClean == "the godfather part"] <- "godfather"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "philadelphia"] <- "philadelphia story"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "private ryan" | 
                                MusicPoncet$fav_movie_toClean == "saving private ryan"] <- "il faut sauver le soldat ryan"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "rocky iv" | 
                                MusicPoncet$fav_movie_toClean == "rocky horror"] <- "rocky"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "scare face"] <- "scarface"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "list schindler" | 
                                MusicPoncet$fav_movie_toClean == "liste schindler" | 
                                MusicPoncet$fav_movie_toClean == "schindler list" | 
                                MusicPoncet$fav_movie_toClean == "schindler s list" | 
                                MusicPoncet$fav_movie_toClean == "schindlers list" | 
                                MusicPoncet$fav_movie_toClean == "shindlers list"] <- "la liste de schindler"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "shaw shank redemption" | 
                                MusicPoncet$fav_movie_toClean == "shawshank" | 
                                MusicPoncet$fav_movie_toClean == "the shawshank redemption" |
                                MusicPoncet$fav_movie_toClean == "shawahank"] <- "shawshank redemption"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "slap shot"] <- "slapshot"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "rain man"] <- "rainman"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "sound of musical" |
                                MusicPoncet$fav_movie_toClean == "the sound of music"] <- "sound of music"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "spiderman a masterpiece in my opinion"] <- "spider man"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "star ward" | 
                                MusicPoncet$fav_movie_toClean == "starwars" | 
                                MusicPoncet$fav_movie_toClean == "stars wars" | 
                                MusicPoncet$fav_movie_toClean == "star wars episode the empire strikes back" | 
                                MusicPoncet$fav_movie_toClean == "star ward" | 
                                MusicPoncet$fav_movie_toClean == "star wars originale" | 
                                MusicPoncet$fav_movie_toClean == "return of the jedi" |
                                MusicPoncet$fav_movie_toClean == "star waes"] <- "star wars"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "star trek iv" | 
                                MusicPoncet$fav_movie_toClean == "startrek"] <- "star trek"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "sur la route de madisson"] <- "sur la route de madison"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "terminator le jugement dernier"] <- "terminator"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "the blindside"] <- "the blind side"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "the titanic"] <- "titanic"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "tom hanks wilson" | 
                                MusicPoncet$fav_movie_toClean == "too many to choose from" | 
                                MusicPoncet$fav_movie_toClean == "too many to list" |
                                MusicPoncet$fav_movie_toClean == "uk" |
                                MusicPoncet$fav_movie_toClean == "trump" |
                                MusicPoncet$fav_movie_toClean == "x" |
                                MusicPoncet$fav_movie_toClean == "xx" |
                                MusicPoncet$fav_movie_toClean == "xxx" |
                                MusicPoncet$fav_movie_toClean == "xxxx" |
                                MusicPoncet$fav_movie_toClean == "vhf" |
                                MusicPoncet$fav_movie_toClean == "unsure" |
                                MusicPoncet$fav_movie_toClean == "the mummy version" |
                                MusicPoncet$fav_movie_toClean == "yfofyo"|
                                MusicPoncet$fav_movie_toClean == "rien" |
                                MusicPoncet$fav_movie_toClean == "ric"|
                                MusicPoncet$fav_movie_toClean == "robin des bois hros en collants" |
                                MusicPoncet$fav_movie_toClean == "pas de film" |
                                MusicPoncet$fav_movie_toClean == "n a i don t like movies"  |
                                MusicPoncet$fav_movie_toClean == "lol"  |
                                MusicPoncet$fav_movie_toClean == "lion"|
                                MusicPoncet$fav_movie_toClean == "jwes" |
                                MusicPoncet$fav_movie_toClean == "home" |
                                MusicPoncet$fav_movie_toClean == "tr" |
                                MusicPoncet$fav_movie_toClean == "srie policiere" |
                                MusicPoncet$fav_movie_toClean == "je n'en ai pas" |
                                MusicPoncet$fav_movie_toClean == "je sais paa"|
                                MusicPoncet$fav_movie_toClean == "je sais pas"|
                                MusicPoncet$fav_movie_toClean == "je vais rarement au cinma"|
                                MusicPoncet$fav_movie_toClean == "indian movies" |
                                MusicPoncet$fav_movie_toClean == "il y en a plusieurs"|
                                MusicPoncet$fav_movie_toClean == "i dont have one"|
                                MusicPoncet$fav_movie_toClean == "i have no favorite"|
                                MusicPoncet$fav_movie_toClean == "i cant choose one" |
                                MusicPoncet$fav_movie_toClean == "i don t have a favorite" |
                                MusicPoncet$fav_movie_toClean == "hp" |
                                MusicPoncet$fav_movie_toClean == "enjoy all movies"|
                                MusicPoncet$fav_movie_toClean == "don t watch" |
                                MusicPoncet$fav_movie_toClean == "do not have one" |
                                MusicPoncet$fav_movie_toClean == "cant pick just one" |
                                MusicPoncet$fav_movie_toClean == "can t think"|
                                MusicPoncet$fav_movie_toClean == "ca dpend" |
                                MusicPoncet$fav_movie_toClean == "can t recall"|
                                MusicPoncet$fav_movie_toClean == "can t decide"] <- NA
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "wizard of oz"] <- "the wizard of oz"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "space ball"] <- "spaceballs"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lion kung"] <- "lion king"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lgende dautomne"] <- "lgendes dautomne"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "joker"] <- "le joker"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "parsite"] <- "parasite"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "my cousin vinnie"] <- "my cousin vinny"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "money heists"] <- "money heist"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lgende dautomne"] <- "lgende d automne"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "lala land" | 
                              MusicPoncet$fav_movie_toClean == "lanla land"] <- "la la land"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "le parain"] <- "le parrain"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "le parrain"] <- "godfather"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "jurasic park"] <- "jurassic park"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "indianna jones" | 
                                MusicPoncet$fav_movie_toClean == "indiana jones crusade"] <- "indiana jones"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "hang over"] <- "hangover"
MusicPoncet$fav_movie_toClean[MusicPoncet$fav_movie_toClean == "black phantere"] <- "black panther"


table(MusicPoncet$fav_movie_toClean)
##

MusicPoncet$fav_movie_Clean <- MusicPoncet$fav_movie_toClean

DDD <- MusicPoncet %>%
  dplyr::select(CASEID, fav_movie_Clean)

################


Data$fav_music <- Data$O_G1
Data$fav_movie <- Data$O_G3

MusicPoncet2 <- Data %>% 
  dplyr::select(CASEID,fav_movie,fav_music)

for (i in 1:nrow(MusicPoncet2)) {
  if (is.na(MusicPoncet2$fav_movie[i]) & !is.na(MusicPoncet2$fav_music[i])) {
    MusicPoncet2$fav_movie[i] <- "None"
  }
  if (is.na(MusicPoncet2$fav_music[i]) & !is.na(MusicPoncet2$fav_movie[i])) {
    MusicPoncet2$fav_movie[i] <- "None"
  }
  print(i)
}

MusicPoncet2 <- MusicPoncet %>% na.omit()



MusicPoncet2$text4 <- MusicPoncet2$fav_music
MusicPoncet2$text4 <- tolower(MusicPoncet2$text4)
# Re3ove mentions, urls, emojis, numbers, punctuations, etc.
MusicPoncet2$text4 <- gsub("@\\w+", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("https?://.+", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("\\d+\\w*\\d*", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("#\\w+", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("[^\x01-\x7F]", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("[[:punct:]]", " ", MusicPoncet2$text4)
MusicPoncet2$# Re3ove spaces and newlines
MusicPoncet2$text4 <- gsub("\n", " ", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("^\\s+", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("\\s+$", "", MusicPoncet2$text4)
MusicPoncet2$text4 <- gsub("[ |\t]+", " ", MusicPoncet2$text4)
# Put the data to a new column
MusicPoncet2["fix_text4"] <- MusicPoncet2$text4

MusicPoncet2$fav_music_toClean <- MusicPoncet2$text4

view(MusicPoncet2$fav_music_toClean)
table(MusicPoncet2$fav_music_toClean)


MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "acdc"] <- "ac dc"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "a dc"] <- "ac dc"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "a r rahman"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "adel"] <- "adele"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "aha"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "all"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "andrea boccelli" | 
                                MusicPoncet2$fav_music_toClean == "andrea botcelli"] <- "andrea bocelli"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "angie" | 
                                MusicPoncet2$fav_music_toClean == "arion" | 
                                MusicPoncet2$fav_music_toClean == "arj" | 
                                MusicPoncet2$fav_music_toClean == "angie" |
                                MusicPoncet2$fav_music_toClean == "atb" | 
                                MusicPoncet2$fav_music_toClean == "arrehman" | 
                                MusicPoncet2$fav_music_toClean == "aucun" | 
                                MusicPoncet2$fav_music_toClean == "aucun en particuleir j coute les annes" | 
                                MusicPoncet2$fav_music_toClean == "aucun en particulier" |
                                MusicPoncet2$fav_music_toClean == "aucune idee"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "back street boys" | 
                                MusicPoncet2$fav_music_toClean == "backstreetboys"] <- "backstreet boys"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "beatles" | 
                                MusicPoncet2$fav_music_toClean == "beetles"] <- "the beatles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "beegees"] <- "bee gees"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "biggie"] <- "biggie smalls"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bob seeger"] <- "bob seger"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bon iver queen" |
                                MusicPoncet2$fav_music_toClean == "freddy mercury queen"] <- "queen"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bonjovi"] <- "bon jovi"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "can t choose" | 
                              MusicPoncet2$fav_music_toClean == "can t remember" | 
                              MusicPoncet2$fav_music_toClean == "cant think of a favourite"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cline dion"] <- "celine dion"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cold play"] <- "coldplay"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cowboys fringuants" | 
                              MusicPoncet2$fav_music_toClean == "cowboy fringuants"] <- "les cowboys fringuants"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "david usher tiesto calvin harris" | 
                                MusicPoncet2$fav_music_toClean == "dd" | 
                                MusicPoncet2$fav_music_toClean == "ddt"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "don t have a favorite" | 
                                MusicPoncet2$fav_music_toClean == "don t have a favourite" | 
                                MusicPoncet2$fav_music_toClean == "don t have one"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "don t have one at least can t think of one" | 
                                MusicPoncet2$fav_music_toClean == "don t know favourite composer is bach" | 
                                MusicPoncet2$fav_music_toClean == "dont have one"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "don t really listen to music however i enjoy music in video games since there is no lyrics and it is calming" | 
                                MusicPoncet2$fav_music_toClean == "dont know" | 
                                MusicPoncet2$fav_music_toClean == "fnaire"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "ed sheean" | 
                                MusicPoncet2$fav_music_toClean == "ed sherran"] <- "ed sheeran"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "foo fighter"] <- "foo fighters"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "g" | 
                              MusicPoncet2$fav_music_toClean == "geeat" | 
                              MusicPoncet2$fav_music_toClean == "gg" | 
                                MusicPoncet2$fav_music_toClean == "gh" |
                                MusicPoncet2$fav_music_toClean == "good"] <- NA 
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "gordie lightfoot"] <- "gordon lightfoot"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "gun n roses"] <- "guns n roses"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "harrmonium"] <- "harmonium"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "harry chaplin now deceased"] <- "harry chaplin"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "have many" | 
                                MusicPoncet2$fav_music_toClean == "have lots" | 
                                MusicPoncet2$fav_music_toClean == "have none" | 
                                MusicPoncet2$fav_music_toClean == "him" |
                                MusicPoncet2$fav_music_toClean == "huhh"] <- NA 
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "i do t know" | 
                                MusicPoncet2$fav_music_toClean == "i don t have a favorite" | 
                                MusicPoncet2$fav_music_toClean == "i don t have a single favorite" | 
                                MusicPoncet2$fav_music_toClean == "i don t have any favorites" |
                                MusicPoncet2$fav_music_toClean == "i don t have one"] <- NA 
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "i don t have one in particular" | 
                                MusicPoncet2$fav_music_toClean == "i dont have any" | 
                                MusicPoncet2$fav_music_toClean == "i dont have one" | 
                                MusicPoncet2$fav_music_toClean == "i enjoy listening to music but i don t have a favorite"] <- NA 
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "imagine dragon"] <- "imagine dragons"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "impossible to choose"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "je n ai pas favoris" | 
                                MusicPoncet2$fav_music_toClean == "je n en ai pas la musique n est pas importante pour moi" | 
                                MusicPoncet2$fav_music_toClean == "je ne sais pas" | 
                                MusicPoncet2$fav_music_toClean == "je nen ai pas" |
                                MusicPoncet2$fav_music_toClean == "je sais pas"] <- NA 
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "justin beiber"] <- "justine bieber"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "kan"] <- "kain"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "kenny gee"] <- "kenny g"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "led zepplin" | 
                              MusicPoncet2$fav_music_toClean == "ledzeplin" | 
                                MusicPoncet2$fav_music_toClean == "led zephalin" | 
                                MusicPoncet2$fav_music_toClean == "led zep"] <- "led zeppelin"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les beatles"] <- "the beatles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les cowboys fringuants"] <- "les cowboys fringants"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "luke combs dixie chicks eric church"] <- "luke combs"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "lumineers and mumford and sons"] <- "mumford and sons"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "m sf"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "madona"] <- "madonna"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "many"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "michael bubbl" | 
                                MusicPoncet2$fav_music_toClean == "michael bubl" | 
                                MusicPoncet2$fav_music_toClean == "michel buble" | 
                                MusicPoncet2$fav_music_toClean == "michael bubble"] <- "michael buble"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "miheal jackson"] <- "michael jackson"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "motley cre"] <- "motley crue"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "musique douce et calme"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "n a" | 
                                MusicPoncet2$fav_music_toClean == "na" |
                                MusicPoncet2$fav_music_toClean == "nant" | 
                                MusicPoncet2$fav_music_toClean == "ne sait pas" | 
                                MusicPoncet2$fav_music_toClean == "ne sais pas" | 
                                MusicPoncet2$fav_music_toClean == "nf" | 
                                MusicPoncet2$fav_music_toClean == "no" |
                                MusicPoncet2$fav_music_toClean == "no clue" |
                                MusicPoncet2$fav_music_toClean == "no fav" |
                                MusicPoncet2$fav_music_toClean == "no preference" |
                                MusicPoncet2$fav_music_toClean == "no one comes to mind" |
                                MusicPoncet2$fav_music_toClean == "non" |
                                MusicPoncet2$fav_music_toClean == "none" |
                                MusicPoncet2$fav_music_toClean == "none in particular" |
                                MusicPoncet2$fav_music_toClean == "not sure" |
                                MusicPoncet2$fav_music_toClean == "nothing more" |
                                MusicPoncet2$fav_music_toClean == "pas un en particulier jen ai plusieurs" |
                                MusicPoncet2$fav_music_toClean == "opra bastille et autre maison d opera" |
                                MusicPoncet2$fav_music_toClean == "plusieurs"|
                                MusicPoncet2$fav_music_toClean == "pop"|
                                MusicPoncet2$fav_music_toClean == "question trop difficile"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "nickle back" | 
                                MusicPoncet2$fav_music_toClean == "nickleback"] <- "nickelback"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "pixies the smiths liminatas"] <- "pixies"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "ric lapointe"] <- "eric lapointe"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "rien en particulier" | 
                                MusicPoncet2$fav_music_toClean == "rock" | 
                                MusicPoncet2$fav_music_toClean == "rty" |
                                MusicPoncet2$fav_music_toClean == "srhgsr"|
                                MusicPoncet2$fav_music_toClean == "nothing" |
                                MusicPoncet2$fav_music_toClean == "notrodame de pari"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "th eagles"] <- "the eagles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "the luminers"] <- "the lumineers"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "the weekend" |
                                MusicPoncet2$fav_music_toClean == "weeknd"] <- "the weeknd"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "too many" | 
                                MusicPoncet2$fav_music_toClean == "too many times to list" | 
                                MusicPoncet2$fav_music_toClean == "too many to choose just one" |
                                MusicPoncet2$fav_music_toClean == "u"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "uk" | 
                                MusicPoncet2$fav_music_toClean == "ub" | 
                                MusicPoncet2$fav_music_toClean == "unknown" |
                                MusicPoncet2$fav_music_toClean == "unsure"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "varied" | 
                                MusicPoncet2$fav_music_toClean == "wd" | 
                                MusicPoncet2$fav_music_toClean == "x" |
                                MusicPoncet2$fav_music_toClean == "xxxx" | 
                                MusicPoncet2$fav_music_toClean == "yy" | 
                                MusicPoncet2$fav_music_toClean == "ykkyk" | 
                                MusicPoncet2$fav_music_toClean == "various artists no favorite bands" |
                                MusicPoncet2$fav_music_toClean == "trop de choix" | 
                                MusicPoncet2$fav_music_toClean == "un mix de musique" |
                                MusicPoncet2$fav_music_toClean == "szaszaszcavas zenekar" | 
                                MusicPoncet2$fav_music_toClean == "sais pas" | 
                                MusicPoncet2$fav_music_toClean == "plusieurs difficile de choisir" | 
                                MusicPoncet2$fav_music_toClean == "plamondon" | 
                                MusicPoncet2$fav_music_toClean == "no idea" | 
                                MusicPoncet2$fav_music_toClean == "no favourites" | 
                                MusicPoncet2$fav_music_toClean == "nkotb" | 
                                MusicPoncet2$fav_music_toClean == "no use for a name"| 
                                MusicPoncet2$fav_music_toClean == "nba"| 
                                MusicPoncet2$fav_music_toClean == "mkto" | 
                                MusicPoncet2$fav_music_toClean == "milow" | 
                                MusicPoncet2$fav_music_toClean == "lp" | 
                                MusicPoncet2$fav_music_toClean == "live" | 
                                MusicPoncet2$fav_music_toClean == "kpop"| 
                                MusicPoncet2$fav_music_toClean == "je n ai pas de prfer" |
                                MusicPoncet2$fav_music_toClean == "jaime beaucoup trop de style pour en avoir juste"|
                                MusicPoncet2$fav_music_toClean == "i have many" |
                                MusicPoncet2$fav_music_toClean == "i dont know" |
                                MusicPoncet2$fav_music_toClean == "i" |
                                MusicPoncet2$fav_music_toClean == "hdhs" |
                                MusicPoncet2$fav_music_toClean == "h"  |
                                MusicPoncet2$fav_music_toClean == "h beach"  |
                                MusicPoncet2$fav_music_toClean == "got" |
                                MusicPoncet2$fav_music_toClean == "fun" |
                                MusicPoncet2$fav_music_toClean == "fgl" |
                                MusicPoncet2$fav_music_toClean == "ffdp" |
                                MusicPoncet2$fav_music_toClean == "eve" |
                                MusicPoncet2$fav_music_toClean == "et"|
                                MusicPoncet2$fav_music_toClean == "enposib"|
                                MusicPoncet2$fav_music_toClean == "dunno" |
                                MusicPoncet2$fav_music_toClean == "dont have a fav" |
                                MusicPoncet2$fav_music_toClean == "don t have one anymore" |
                                MusicPoncet2$fav_music_toClean == "do not have a favourite"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "wu tand" | 
                                MusicPoncet2$fav_music_toClean == "wu tang clan"] <- "wutang"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "regga bob marley"] <- "bob marley"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "harris style"] <- "harry styles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "jcole"] <- "j cole"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "rene martel country"] <- "rene martel"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "queens"] <- "queen"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "pink floyid"] <- "pink floyd"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "nirvana kurt cobain"] <- "nirvana"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "neil dimond"] <- "neil diamond"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "mylne farmer"] <- "mylene farmer"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "muze"] <- "muse"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "metalica"] <- "metallica"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "luis thechild"] <- "louis the child"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les frres" |
                                MusicPoncet2$fav_music_toClean == "freres" | 
                                MusicPoncet2$fav_music_toClean == "frres"] <- "les deux frres"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les cowboys fringuant"] <- "les cowboys fringuants"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les beatels"] <- "the beatles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "lana del ray"] <- "lana del ray"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "justine bieber"] <- "justin bieber"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "earth wind fire"] <- "earth wind and fire"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cur de pirate"] <- "coeur de pirate"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "daniel blanger"] <- "daniel belanger"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cxxx"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bonne question"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cow boy fringants" | 
                                MusicPoncet2$fav_music_toClean == "cowboys fringants" | 
                                MusicPoncet2$fav_music_toClean == "cowboy fringant" | 
                                MusicPoncet2$fav_music_toClean == "cowboy fringants"] <- "les cowboys fringuants"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "contry" | 
                                MusicPoncet2$fav_music_toClean == "country" | 
                                MusicPoncet2$fav_music_toClean == "country music" |
                                MusicPoncet2$fav_music_toClean == "country music all"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "classic rock and jazz" | 
                                MusicPoncet2$fav_music_toClean == "classic" | 
                                MusicPoncet2$fav_music_toClean == "unknown" |
                                MusicPoncet2$fav_music_toClean == "chigaco"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "cnco" | 
                                MusicPoncet2$fav_music_toClean == "collective soul" | 
                                MusicPoncet2$fav_music_toClean == "can t decide" |
                                MusicPoncet2$fav_music_toClean == "buble"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "ccr"] <- "credence clearwater revival"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bsb" | 
                                MusicPoncet2$fav_music_toClean == "bto" | 
                                MusicPoncet2$fav_music_toClean == "bowie" |
                                MusicPoncet2$fav_music_toClean == "blink"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bruce springsteen s"] <- "bruce springsteen"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "brad pit" | 
                                MusicPoncet2$fav_music_toClean == "bigbang" | 
                                MusicPoncet2$fav_music_toClean == "bowie" |
                                MusicPoncet2$fav_music_toClean == "blink"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "billie elish" | 
                                MusicPoncet2$fav_music_toClean == "billie eillish"] <- "billie eilish"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "beyonc"] <- "beyonce"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "beatles rolling stones" | 
                                MusicPoncet2$fav_music_toClean == "beetle" | 
                                MusicPoncet2$fav_music_toClean == "beattles" |
                                MusicPoncet2$fav_music_toClean == "beatle"] <- "the beatles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "beau domage"] <- "beau dommage"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "be gees" | 
                                 MusicPoncet2$fav_music_toClean == "bee ges"] <- "bee ges"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "bach boys" | 
                                MusicPoncet2$fav_music_toClean == "abba beach boys"] <- "beach boys"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "barbra streisand"] <- "barbara streisand"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "archive" | 
                                MusicPoncet2$fav_music_toClean == "all country music" | 
                                MusicPoncet2$fav_music_toClean == "a" | 
                                MusicPoncet2$fav_music_toClean == " "] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "andr rieu" | 
                                MusicPoncet2$fav_music_toClean == "andr rieu et cie" | 
                                MusicPoncet2$fav_music_toClean == "andre riue"] <- "andre rieu"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "alanis morrisette" | 
                                MusicPoncet2$fav_music_toClean == "alain morisso"] <- "alanis morissette"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "alexandra strelinsky" | 
                                MusicPoncet2$fav_music_toClean == "alexandra strliski "] <- "alexandra streliski"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "accords" | 
                                MusicPoncet2$fav_music_toClean == "trois accords" | 
                                MusicPoncet2$fav_music_toClean == "trois"] <- "les trois accords"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "adle"] <- "adele"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "yea" | 
                                MusicPoncet2$fav_music_toClean == "vh" | 
                                MusicPoncet2$fav_music_toClean == "yea" | 
                                MusicPoncet2$fav_music_toClean == "what are you doing in this time" |
                                MusicPoncet2$fav_music_toClean == "wham"|
                                MusicPoncet2$fav_music_toClean == "variety"|
                                MusicPoncet2$fav_music_toClean == "various i have no favorite" |
                                MusicPoncet2$fav_music_toClean == "various no particular favourite"|
                                MusicPoncet2$fav_music_toClean == "there are many i like equally" |
                                MusicPoncet2$fav_music_toClean == "rnb" |
                                MusicPoncet2$fav_music_toClean == "rien de particulier" |
                                MusicPoncet2$fav_music_toClean == "pin" |
                                MusicPoncet2$fav_music_toClean == "pavement"|
                                MusicPoncet2$fav_music_toClean == "peggy winnipeg"|
                                MusicPoncet2$fav_music_toClean == "no favorites" |
                                MusicPoncet2$fav_music_toClean == "nct" |
                                MusicPoncet2$fav_music_toClean == "leon" |
                                MusicPoncet2$fav_music_toClean == "latin"|
                                MusicPoncet2$fav_music_toClean == "kvb" |
                                MusicPoncet2$fav_music_toClean == "jews" |
                                MusicPoncet2$fav_music_toClean == "jazz"|
                                MusicPoncet2$fav_music_toClean == "hip" |
                                MusicPoncet2$fav_music_toClean == "hurt" |
                                MusicPoncet2$fav_music_toClean == "groupe rock"|
                                MusicPoncet2$fav_music_toClean == "e" |
                                MusicPoncet2$fav_music_toClean == "do not have one"|
                                MusicPoncet2$fav_music_toClean == "day" |
                                MusicPoncet2$fav_music_toClean == "county"|
                                MusicPoncet2$fav_music_toClean == "can"|
                                MusicPoncet2$fav_music_toClean == "big bands jazz" |
                                MusicPoncet2$fav_music_toClean == "mac" |
                                MusicPoncet2$fav_music_toClean == "il y en a plusieurs"|
                                MusicPoncet2$fav_music_toClean == "i like live musicians" |
                                MusicPoncet2$fav_music_toClean == "i have several favorites" |
                                MusicPoncet2$fav_music_toClean == "can t think of any right now"|
                                MusicPoncet2$fav_music_toClean == "adtr"] <- NA
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "walk of the earth" | 
                                MusicPoncet2$fav_music_toClean == "walk pff the earth"] <- "walk off the earth"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "van morrision"] <- "van morrison"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "the beetles"] <- "the beatles"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "the bee gees"] <- "bee gees"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "van morrision"] <- "van morrison"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "springsteen" | 
                                MusicPoncet2$fav_music_toClean == "springstein"] <- "bruce springsteen"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "stevie ray vaughn"] <- "stevie ray vaughan"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "rolling stone"] <- "rolling stones"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "rihannah" | 
                                MusicPoncet2$fav_music_toClean == "rhiannah" ] <- "rihanna"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "rascall flatts"] <- "rascal flatts"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "pinkfloyd"] <- "pinkfloyd"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "phil colin"] <- "phil collins"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les cowboys fringant"] <- "les cowboys fringants"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "les accords"] <- "les trois accords"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "elvis"] <- "elvis presley"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "j s bach" | 
                                MusicPoncet2$fav_music_toClean == "j s jean sbastien bach bach"] <- "bach"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "niel diamond"] <- "neil diamond"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "maroon"] <- "maroon 5"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "korn slipknot mushroomhead"] <- "korn"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "neal yound"] <- "neil young"
MusicPoncet2$fav_music_toClean[MusicPoncet2$fav_music_toClean == "beach boys"] <- "the beach boys"


table(MusicPoncet2$fav_music_toClean)

MusicPoncet2$fav_music_Clean <- MusicPoncet2$fav_music_toClean

E <- MusicPoncet2 %>%
  dplyr::select(CASEID, fav_music_Clean)

################


## Blanc, Asiatique, Autres, Autochtones, Hispaniques

CleanData$ses_ethn <- NA
CleanData$ses_ethn[Data$SES6M1 == 1] <-   1   # "Aboriginal/First nations"
CleanData$ses_ethn[Data$SES6M1 == 2] <-   2   # "British"
CleanData$ses_ethn[Data$SES6M1 == 3] <-   3   # "Chinese"
CleanData$ses_ethn[Data$SES6M1 == 4] <-   4   # "Dutch"
CleanData$ses_ethn[Data$SES6M1 == 5] <-   5   # "English"
CleanData$ses_ethn[Data$SES6M1 == 6] <-   6   # "French"
CleanData$ses_ethn[Data$SES6M1 == 7] <-   7   # "French Canadian"
CleanData$ses_ethn[Data$SES6M1 == 8] <-   8   # "German"
CleanData$ses_ethn[Data$SES6M1 == 9] <-   9   # "Hispanic"
CleanData$ses_ethn[Data$SES6M1 == 10] <-  10   # "Indian"
CleanData$ses_ethn[Data$SES6M1 == 12] <-  11   # "Irish"
CleanData$ses_ethn[Data$SES6M1 == 13] <-  12   # "Italian"
CleanData$ses_ethn[Data$SES6M1 == 14] <-  13   # "Métis"
CleanData$ses_ethn[Data$SES6M1 == 15] <-  14   # "Polish"
CleanData$ses_ethn[Data$SES6M1 == 16] <-  15   # "Quebecer"
CleanData$ses_ethn[Data$SES6M1 == 17] <-  16   # "Scottish"
CleanData$ses_ethn[Data$SES6M1 == 18] <-  17   # "Ukrainian"
CleanData$ses_ethn[Data$SES6M1 == 19] <-  18   # "No other cultural group"
CleanData$ses_ethn[Data$SES6M1 == 20] <-  19   # "Other cultural group"
table(CleanData$ses_ethn)


# CATEGORY 1 # First Nations/Aboriginals

CleanData$ses_ethn_Aborig <- NA
CleanData$ses_ethn_Aborig[Data$SES6M1 == 1] <- 1
CleanData$ses_ethn_Aborig[Data$SES6M1 != 1] <- 0
table(CleanData$ses_ethn_Aborig)

CleanData$ses_ethn_Metis <- NA
CleanData$ses_ethn_Metis[Data$SES6M1 == 14] <- 1
CleanData$ses_ethn_Metis[Data$SES6M1 != 14] <- 0
table(CleanData$ses_ethn_Metis)

CleanData$ses_ethn_Aboriginals <- NA 
CleanData$ses_ethn_Aboriginals[Data$SES6M1 == 1 | Data$SES6M1 == 14 | Data$SES6M1 == 11] <- 1 
CleanData$ses_ethn_Aboriginals[Data$SES6M1 != 1 & Data$SES6M1 != 14 & Data$SES6M1 != 11] <- 0
table(CleanData$ses_ethn_Aboriginals)

# CATEGORY 2 # Whites  

CleanData$ses_ethn_Brit <- NA
CleanData$ses_ethn_Brit[Data$SES6M1 == 2] <- 1
CleanData$ses_ethn_Brit[Data$SES6M1 != 2] <- 0
table(CleanData$ses_ethn_Brit)

CleanData$ses_ethn_PaysBas <- NA
CleanData$ses_ethn_PaysBas[Data$SES6M1 == 4] <- 1
CleanData$ses_ethn_PaysBas[Data$SES6M1 != 4] <- 0
table(CleanData$ses_ethn_PaysBas)

CleanData$ses_ethn_Anglais <- NA
CleanData$ses_ethn_Anglais[Data$SES6M1 == 5] <- 1
CleanData$ses_ethn_Anglais[Data$SES6M1 != 5] <- 0
table(CleanData$ses_ethn_Anglais)

CleanData$ses_ethn_Francais <- NA
CleanData$ses_ethn_Francais[Data$SES6M1 == 6] <- 1
CleanData$ses_ethn_Francais[Data$SES6M1 != 6] <- 0
table(CleanData$ses_ethn_Francais)

CleanData$ses_ethn_Allemand <- NA
CleanData$ses_ethn_Allemand[Data$SES6M1 == 8] <- 1
CleanData$ses_ethn_Allemand[Data$SES6M1 != 8] <- 0
table(CleanData$ses_ethn_Allemand)

CleanData$ses_ethn_CanFR <- NA
CleanData$ses_ethn_CanFR[Data$SES6M1 == 7] <- 1
CleanData$ses_ethn_CanFR[Data$SES6M1 != 7] <- 0
table(CleanData$ses_ethn_CanFR)

CleanData$ses_ethn_Irish <- NA
CleanData$ses_ethn_Irish[Data$SES6M1 == 12] <- 1
CleanData$ses_ethn_Irish[Data$SES6M1 != 12] <- 0
table(CleanData$ses_ethn_Irish)

CleanData$ses_ethn_Italien <- NA
CleanData$ses_ethn_Italien[Data$SES6M1 == 13] <- 1
CleanData$ses_ethn_Italien[Data$SES6M1 != 13] <- 0
table(CleanData$ses_ethn_Italien)

CleanData$ses_ethn_Polish <- NA
CleanData$ses_ethn_Polish[Data$SES6M1 == 15] <- 1
CleanData$ses_ethn_Polish[Data$SES6M1 != 15] <- 0
table(CleanData$ses_ethn_Polish)

CleanData$ses_ethn_QC <- NA
CleanData$ses_ethn_QC[Data$SES6M1 == 16] <- 1
CleanData$ses_ethn_QC[Data$SES6M1 != 16] <- 0
table(CleanData$ses_ethn_QC)

CleanData$ses_ethn_Scottish <- NA
CleanData$ses_ethn_Scottish[Data$SES6M1 == 17] <- 1
CleanData$ses_ethn_Scottish[Data$SES6M1 != 17] <- 0
table(CleanData$ses_ethn_Scottish)

CleanData$ses_ethn_Ukraine <- NA
CleanData$ses_ethn_Ukraine[Data$SES6M1 == 18] <- 1
CleanData$ses_ethn_Ukraine[Data$SES6M1 != 18] <- 0
table(CleanData$ses_ethn_Ukraine)

CleanData$ses_ethn_White <- NA 
CleanData$ses_ethn_White[Data$SES6M1 == 2 | Data$SES6M1 == 4 | Data$SES6M1 == 5
                         | Data$SES6M1 == 6 | Data$SES6M1 == 7
                         | Data$SES6M1 == 8 | Data$SES6M1 == 12
                         | Data$SES6M1 == 13 | Data$SES6M1 == 15
                         | Data$SES6M1 == 16 | Data$SES6M1 == 17
                         | Data$SES6M1 == 18] <- 1 
CleanData$ses_ethn_White[Data$SES6M1 != 2 & Data$SES6M1 != 4 & Data$SES6M1 != 5
                         & Data$SES6M1 != 6 & Data$SES6M1 != 7
                         & Data$SES6M1 != 8 & Data$SES6M1 != 12
                         & Data$SES6M1 != 13 & Data$SES6M1 != 15
                         & Data$SES6M1 != 16 & Data$SES6M1 != 17
                         & Data$SES6M1 != 18] <- 0
table(CleanData$ses_ethn_White)

# CATEGORY 3 # Asiatique

CleanData$ses_ethn_Chine <- NA
CleanData$ses_ethn_Chine[Data$SES6M1 == 3] <- 1
CleanData$ses_ethn_Chine[Data$SES6M1 != 3] <- 0
table(CleanData$ses_ethn_Chine)

CleanData$ses_ethn_Inde <- NA
CleanData$ses_ethn_Inde[Data$SES6M1 == 10] <- 1
CleanData$ses_ethn_Inde[Data$SES6M1 != 10] <- 0
table(CleanData$ses_ethn_Inde)

CleanData$ses_ethn_Asiatique <- NA 
CleanData$ses_ethn_Asiatique[Data$SES6M1 == 3 | Data$SES6M1 == 10] <- 1 
CleanData$ses_ethn_Asiatique[Data$SES6M1 != 3 & Data$SES6M1 != 10] <- 0
table(CleanData$ses_ethn_Asiatique)

# CleanData$ses_ethn_Inuit <- NA
# CleanData$ses_ethn_Inuit[Data$SES6M1 == 11] <- 1
# CleanData$ses_ethn_Inuit[Data$SES6M1 != 11] <- 0
# table(CleanData$ses_ethn_Inuit)


# CleanData$ses_ethn_Hisp <- NA
# CleanData$ses_ethn_Hisp[Data$SES6M1 == 9] <- 1
# CleanData$ses_ethn_Hisp[Data$SES6M1 != 9] <- 0
# table(CleanData$ses_ethn_Hisp)

CleanData$ses_ethn_None <- NA
CleanData$ses_ethn_None[Data$SES6M1 == 19] <- 1
CleanData$ses_ethn_None[Data$SES6M1 != 19] <- 0
table(CleanData$ses_ethn_None)

CleanData$ses_ethn_Other <- NA
CleanData$ses_ethn_Other[Data$SES6M1 == 20 | Data$SES6M1 == 9] <- 1
CleanData$ses_ethn_Other[Data$SES6M1 != 20 & Data$SES6M1 != 9] <- 0
table(CleanData$ses_ethn_Other)


################
OO <- inner_join(CleanData, CleanData2, DDD, by = "CASEID")
OO2 <- inner_join(OO, E, by = "CASEID")

fastT19 = apply(OO2, 2, table)


write_csv(OO2, "../bav-2021/_SharedFolder_bav-2021/Data/Clean/CleanData-Lifestyle.csv")




