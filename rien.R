####################################################################################################
######################### Modèles prédictifs - BAV 2021 ############################################
####################################################################################################

#### Package ####

library(tidyverse)
library(tidyr)
library(caret)
library(kernlab)

# Load the data
#df <- read_csv("~/Dropbox/BaV-2021/_SharedFolder_bav-2021/Data/Clean/CleanData-SVM_21mars2021.csv") %>%
df <- read_csv("/Users/justinebechard/Dropbox/CLESSN/bav-2021/_SharedFolder_bav-2021/Data/Clean/CleanData-Lifestyle.csv") %>%
  select(ses_genderMale, ses_genderFemale, 
         ses_age1834, ses_age55Plus, 
         ses_languageFrench, ses_languageEnglish, 
         act_Gym, act_TeamSport, act_Walk, act_Run, act_Yoga, act_Swimming, act_Other, act_None, 
         act_Fishing, act_Hunting, act_WinterBoard, act_TeamSport, act_VisitsMuseumsGaleries, act_PerformingArts, act_PartiesAndSocial, act_ManualTasks, act_MotorizedOutdoorActivities, act_Outdoors, act_Volunteering, act_Arts, act_Worship, act_Travel, act_VideoGames, act_Books, 
         act_holidays_Beach, act_holidays_Nature, act_holidays_Sport, act_holidays_City, act_holidays_Culture, act_holidays_Cruise, act_holidays_Friends, act_holidays_Relax, act_holidays_Other, 
         act_reading_Sport1, act_reading_Politics1, act_reading_Travels1, act_reading_Arts1, act_reading_Cooking1, act_reading_Lifestyle1, act_reading_Photography1, act_reading_Fashion1, act_reading_Other1, act_reading_None1, 
         act_transport_Car, act_transport_SUV, act_transport_Moto, act_transport_Walk, act_transport_Bicycle, act_transport_PublicTransportation, act_transport_Taxi, 
         act_transportWhoIAm, act_transportAtoB, 
         act_occupField_Management, act_occupField_Business, act_occupField_Sciences, act_occupField_Health, act_occupField_Social, act_occupField_Recreation, act_occupField_Sales, act_occupField_TranspMachinery, act_occupField_NaturalRes, act_occupField_Manufact, act_occupField_Other, 
         act_work_Labour, act_work_Intermediate, act_work_Technical, act_work_Professional, act_work_Management, act_work_Other, 
         #simon_workingless35, simon_working35to45, simon_workingMore45, 
         #simon_positionPermanent, simon_positionContract, simon_positionOther, 
         #simon_controlSchedule, 
         #simon_union, 
         #simon_mattering_Important, simon_mattering_Attention,  simon_mattering_Miss,  simon_mattering_Interested, simon_mattering_Dependant,  
         #simon_MentalOptimist, simon_MentalUseful,  simon_Mentalrelaxed,  simon_MentalDeltProb,  simon_MentalThinkClear,  simon_MentalNearOthers,  simon_MentalDecided, simon_MentalNoFun, simon_MentalSad, 
         act_envDriveChoice, 
         cons_tryCultRecipes, cons_planMeals, cons_noTransFoods, cons_orgKitchen, 
         op_qcInd, op_moreCanThanQc, 
         op_LastTechBestMe, op_lifesIncertOk, op_changeEssential, op_destinyFixed, op_othAdmireMyThings, op_prefExperience, op_likes2BeDifferent, op_limitWordUse, 
         val_belonging1, val_belonging2, val_belonging3, 
         val_exitement1, val_exitement2, val_exitement3, 
         val_relationship1, val_relationship2, val_relationship3, 
         val_fulfillment1, val_fulfillment2, val_fulfillment3, 
         val_bRespected1, val_bRespected2, val_bRespected3, 
         val_fun1, val_fun2, val_fun3, 
         val_security1, val_security2, val_security3, 
         val_selfRespect1, val_selfRespect2, val_selfRespect3, 
         val_accomplish1, val_accomplish2, val_accomplish3, 
         val_kidsCreative, val_kidsDisciplinated, 
         val_kidsConformity, val_kidsFreethink, 
         cons_brand_MaR, cons_brand_BInd, cons_brand_ChainesB, cons_brand_GSurf, cons_brand_Frip, cons_brand_Other, 
         cons_fashion_o, 
         cons_Meat, cons_Vege, cons_Vegan, 
         cons_coffee_TimH, cons_coffee_Other, cons_coffee_Starbucks, cons_coffee_SC, cons_coffee_McDo, cons_coffee_place_ind, cons_coffee_place_noCoffee, 
         cons_coffee_type_Filtre, cons_coffee_type_Italien, cons_coffee_type_Perco, cons_coffee_type_PresseFR, cons_coffee_type_Capsules, cons_coffee_type_Expresso, cons_coffee_type_Instant, 
         cons_Smoke, cons_VapeNation, 
         cons_noDrink, cons_redWineDrink, cons_whiteWineDrink, cons_roseDrink, cons_sparklingDrink, cons_regBeers, cons_microBeers, cons_spiritDrink, cons_cocktailsDrink, 
         cons_socmedia_Facebook, cons_socmedia_Twitter, cons_socmedia_Insta, cons_socmedia_Snap, cons_socmedia_Tiktok, cons_socmedia_Pinterest, cons_socmedia_LinkedIn, cons_socmedia_YT, cons_socmedia_Other, 
         app_swag_Formel, app_swag_Classique, app_swag_Casual, app_swag_Sport, app_swag_Chic, app_swag_Vintage, app_swag_Hippie, app_swag_Boh, app_swag_Punk, app_swag_Rock, 
         op_turnout2019, 
         ses_educ_None, ses_educ_Prim, ses_educ_Sec, ses_educ_Coll, ses_educ_Bacc, ses_educ_Master, ses_educ_PhD, 
         ses_income_None, ses_income_i1to30, ses_income_i31to60, ses_income_i61to90, ses_income_i91to110, ses_income_i111to150, ses_income_i151to200, ses_income_i201toInf,  
         ses_prov_Alb, ses_prov_Bc, ses_prov_Manitoba, ses_prov_Nb, ses_prov_Nfl, ses_prov_Ns, ses_prov_NWT, ses_prov_Nunavut, ses_prov_Ont, ses_prov_Pei, ses_prov_Qc, ses_prov_Skt, ses_prov_Yukon, 
         ses_urbain, ses_sururbain, ses_rural, 
         ses_celib, ses_married, ses_commonlawRel, ses_widow, ses_divorced, ses_relationship, 
         ses_noHouseholdKids, ses_fewHouseholdKids, ses_manyHouseholdKids, 
         ses_hetero, ses_gai, ses_bisex, ses_sexOri_other, 
         ses_parentsBornCanada, ses_bornCanada, 
         ses_dwelling_app, ses_dwelling_loft, ses_dwelling_condo, ses_dwelling_tour, ses_dwelling_detachedHouse, ses_dwelling_townHouse, ses_dwelling_semiDetached, ses_dwelling_coop, ses_dwelling_HLM, ses_dwelling_mobile, ses_dwelling_other, ses_dwelling_house, 
         #visit_Hawaii, visit_Inde, visit_USA, visit_France, visit_NZ, visit_Martinique, visit_Mexique, visit_Maldives, visit_Allemagne, visit_Grece, visit_Ireland, visit_Islande, visit_Italie, visit_Israel, visit_UK, visit_Europe, visit_Quebec, visit_Canada, visit_Egypte, visit_Ecosse, visit_Tahiti, visit_Thailande, visit_Fiji, visit_France, visit_Jamaique, visit_Japon, visit_Australie, 
         car_VUS, car_smol, car_pickup, car_van, 
         ses_ethn_Aborig, ses_ethn_Brit, ses_ethn_Chine, ses_ethn_PaysBas, ses_ethn_Anglais, ses_ethn_Francais, ses_ethn_CanFR, ses_ethn_Allemand, ses_ethn_Hisp, ses_ethn_Inde, ses_ethn_Irish, ses_ethn_Italien, ses_ethn_Metis, ses_ethn_Polish, ses_ethn_QC, ses_ethn_Scottish, ses_ethn_Ukraine, ses_ethn_None, ses_ethn_Other
  )

