# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "22/08/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: This script does investigate the data, in order to understand how it can drive business value:
#      1: ...
#      2: ...

# Questions: 
#     - ...

# Assumptions

# Todo
#     - ...

# Resources
#     - http://topepo.github.io/caret/index.html

# Output:
#     - Basis for a report only, no data generation, formatting etc.


library(data.table)
library(caret)


################################################################################################################################
# INPUT
################################################################################################################################
raw_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/raw_homeCredit/application_train"
raw_filename <- "application_train.csv"

#Read data
raw_data <- read.csv(paste0(raw_path, "/", raw_filename),
                     sep = ",", dec=".", stringsAsFactors = F)
df <- raw_data

################################################################################################################################
# Data understanding
################################################################################################################################
#Quick QA if reading went ok, and give us a high level understanding of the data
dim(df)
head(df)
str(df) # give a quick overview - like head but state the type as well
#fix(df) #spend quite some time here - try to segment columns into different groups and have this sheet open when doing the below
utils::View(df) # Can also do View(), however not always View give out all the columns. I thin fix() is a bit nicer
table(df$TARGET) 
# Check if unique IDs!
sum(duplicated(df$SK_ID_CURR))

# Variable Identification: Understanding the columns
###
summary(df) # here lists NA's per column, max, min, mean, class (if character it says so).. Most important print out!
# Load of NA's --> cannot simply remove the rows, then we remove more than 2/3 of the data set..

# Checking categorical vars:
unique(df$NAME_CONTRACT_TYPE) 
unique(df$CODE_GENDER) 
unique(df$FLAG_OWN_CAR) 

# Get an idea of which variables that can explain TARGET
table(df$TARGET, df$NAME_HOUSING_TYPE) 
table(df$TARGET, df$NAME_CONTRACT_TYPE) 
table(df$TARGET, df$FLAG_OWN_CAR) 
table(df$TARGET, df$NAME_EDUCATION_TYPE) 
table(df$TARGET, df$OCCUPATION_TYPE) 
table(df$TARGET, df$REGION_RATING_CLIENT) # here see clear difference

# Group variables that belongs together - for understanding
house_vars <- c("NAME_HOUSING_TYPE", "APARTMENTS_AVG", "BASEMENTAREA_AVG",  "YEARS_BEGINEXPLUATATION_AVG" , 
                "YEARS_BUILD_AVG", "COMMONAREA_AVG"  ,"ELEVATORS_AVG"    ,            "ENTRANCES_AVG"               
                , "FLOORSMAX_AVG"      ,          "FLOORSMIN_AVG"          ,      "LANDAREA_AVG"                
                , "LIVINGAPARTMENTS_AVG"     ,    "LIVINGAREA_AVG"    ,           "NONLIVINGAPARTMENTS_AVG"     
                , "NONLIVINGAREA_AVG"         ,   "APARTMENTS_MODE"   ,           "BASEMENTAREA_MODE"           
                , "YEARS_BEGINEXPLUATATION_MODE", "YEARS_BUILD_MODE"   ,          "COMMONAREA_MODE"             
                , "ELEVATORS_MODE"            ,   "ENTRANCES_MODE"     ,          "FLOORSMAX_MODE"              
                , "FLOORSMIN_MODE"             ,  "LANDAREA_MODE"       ,         "LIVINGAPARTMENTS_MODE"       
                , "LIVINGAREA_MODE"             , "NONLIVINGAPARTMENTS_MODE"  ,   "NONLIVINGAREA_MODE"          
                , "APARTMENTS_MEDI"           ,   "BASEMENTAREA_MEDI"          ,  "YEARS_BEGINEXPLUATATION_MEDI"
                , "YEARS_BUILD_MEDI"           ,  "COMMONAREA_MEDI"            ,  "ELEVATORS_MEDI"              
                , "ENTRANCES_MEDI"              , "FLOORSMAX_MEDI"            ,   "FLOORSMIN_MEDI"              
                , "LANDAREA_MEDI"               , "LIVINGAPARTMENTS_MEDI"      ,  "LIVINGAREA_MEDI"             
                , "NONLIVINGAPARTMENTS_MEDI"    , "NONLIVINGAREA_MEDI"          , "FONDKAPREMONT_MODE"          
                , "HOUSETYPE_MODE"              , "TOTALAREA_MODE"               ,"WALLSMATERIAL_MODE"          
                , "EMERGENCYSTATE_MODE"    )

phone_mail_vars <- c("DAYS_LAST_PHONE_CHANGE","FLAG_MOBIL","FLAG_PHONE","FLAG_EMP_PHONE","FLAG_EMAIL" ,
                     "FLAG_CONT_MOBILE","FLAG_WORK_PHONE")

exit_source_vars <- c("EXT_SOURCE_1"  ,    "EXT_SOURCE_2" ,  "EXT_SOURCE_3" )

flag_vars <- c("FLAG_DOCUMENT_2"        ,      "FLAG_DOCUMENT_3"           ,   "FLAG_DOCUMENT_4"          ,    "FLAG_DOCUMENT_5"             
               , "FLAG_DOCUMENT_6"       ,       "FLAG_DOCUMENT_7"       ,       "FLAG_DOCUMENT_8"      ,        "FLAG_DOCUMENT_9"             
               ,"FLAG_DOCUMENT_10"      ,       "FLAG_DOCUMENT_11"        ,     "FLAG_DOCUMENT_12"       ,      "FLAG_DOCUMENT_13"            
               , "FLAG_DOCUMENT_14"    ,         "FLAG_DOCUMENT_15"        ,     "FLAG_DOCUMENT_16"       ,      "FLAG_DOCUMENT_17"            
               ,"FLAG_DOCUMENT_18"    ,         "FLAG_DOCUMENT_19"          ,   "FLAG_DOCUMENT_20"         ,    "FLAG_DOCUMENT_21"       )

personal_vars <- c("CODE_GENDER", "DAYS_BIRTH", "DAYS_EMPLOYED", "DAYS_REGISTRATION", "DAYS_ID_PUBLISH", 
                   "NAME_INCOME_TYPE", "NAME_TYPE_SUITE", "CNT_CHILDREN", "AMT_INCOME_TOTAL", "OCCUPATION_TYPE",
                   "CNT_FAM_MEMBERS", "NAME_FAMILY_STATUS", "NAME_EDUCATION_TYPE", "ORGANIZATION_TYPE"  )

CREDIT_BUREAU_vars <- c("AMT_REQ_CREDIT_BUREAU_HOUR" ,  "AMT_REQ_CREDIT_BUREAU_DAY"  ,  "AMT_REQ_CREDIT_BUREAU_WEEK"  ,
                        "AMT_REQ_CREDIT_BUREAU_MON" , "AMT_REQ_CREDIT_BUREAU_QRT"    ,"AMT_REQ_CREDIT_BUREAU_YEAR" )

bank_credit_vars <- c("NAME_CONTRACT_TYPE","FLAG_OWN_REALTY",
                      "AMT_CREDIT"   ,      "AMT_ANNUITY"     ,"AMT_GOODS_PRICE"  )
region_vars <- c("REGION_POPULATION_RELATIVE" , "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY",
                 "REG_REGION_NOT_LIVE_REGION",  "REG_REGION_NOT_WORK_REGION" , "LIVE_REGION_NOT_WORK_REGION", "REG_CITY_NOT_LIVE_CITY" ,
                 "REG_CITY_NOT_WORK_CITY"     , "LIVE_CITY_NOT_WORK_CITY"  )
social_circle_vars <- c("DEF_30_CNT_SOCIAL_CIRCLE" ,   "OBS_60_CNT_SOCIAL_CIRCLE"   ,
                        "DEF_60_CNT_SOCIAL_CIRCLE", "OBS_30_CNT_SOCIAL_CIRCLE")
car_vars <- c("OWN_CAR_AGE", "FLAG_OWN_CAR" )
process_start_vars <- c("WEEKDAY_APPR_PROCESS_START", "HOUR_APPR_PROCESS_START"   ) #probably irrelevant

irrelevant_vars <-  #e.g. stjernetegn
no_sense_vars <- #Those I dont understand what is
  
total_vars <- c(CREDIT_BUREAU_vars,personal_vars,flag_vars , exit_source_vars, 
                phone_mail_vars, house_vars, bank_credit_vars , car_vars, region_vars,
                social_circle_vars, process_start_vars)
length(names(df[,(names(df) %in%  total_vars)])) - length(total_vars) #check if get our same as total_vars
names(df[,!((names(df) %in%  total_vars))])




###########
# Univariate Analysis: explore variables one by one - Distribution and Extreme values
###########

# Small dataprep - Making a numerical dataframe, in order to do further analysis
###

# Datetime (a bit tricky)
##

# Make dummy
##
dfnum <- data.table(df) 
dmy <- dummyVars(" ~ .", data = dfnum) #from caret -> making dummy vars
dfnum <- data.frame(predict(dmy, newdata = dfnum))
dim(dfnum)
str(dfnum)
summary(dfnum) # don't do anything to NAs

# Must update our variable groups
summary(df[,personal_vars])
sort(names(dfnum))
personal_vars_num <- c("CODE_GENDERF" ,"CODE_GENDERM"  , "CODE_GENDERXNA", "DAYS_BIRTH" , "AMT_INCOME_TOTAL"  ,"DAYS_EMPLOYED", "CNT_CHILDREN" ,"CNT_FAM_MEMBERS" ,
                       "DAYS_REGISTRATION", "DAYS_ID_PUBLISH", "NAME_EDUCATION_TYPEAcademic.degree" ,
                       "NAME_EDUCATION_TYPEHigher.education",          
                        "NAME_EDUCATION_TYPEIncomplete.higher"     ,        "NAME_EDUCATION_TYPELower.secondary"              ,
                        "NAME_EDUCATION_TYPESecondary...secondary.special", "NAME_FAMILY_STATUSCivil.marriage"                ,
                        "NAME_FAMILY_STATUSMarried"                     ,   "NAME_FAMILY_STATUSSeparated"                     ,
                        "NAME_FAMILY_STATUSSingle...not.married"  ,         "NAME_FAMILY_STATUSUnknown"                       ,
                        "NAME_FAMILY_STATUSWidow" ,"NAME_INCOME_TYPEBusinessman",                     
                        "NAME_INCOME_TYPECommercial.associate"        ,     "NAME_INCOME_TYPEMaternity.leave"      ,           
                        "NAME_INCOME_TYPEPensioner"                 ,       "NAME_INCOME_TYPEState.servant"         ,          
                        "NAME_INCOME_TYPEStudent"                    ,      "NAME_INCOME_TYPEUnemployed"             ,         
                        "NAME_INCOME_TYPEWorking"                     ,     "NAME_TYPE_SUITE"                         ,        
                        "NAME_TYPE_SUITEChildren"                      ,    "NAME_TYPE_SUITEFamily"                    ,       
                        "NAME_TYPE_SUITEGroup.of.people"                ,   "NAME_TYPE_SUITEOther_A"                    ,      
                        "NAME_TYPE_SUITEOther_B"                         ,  "NAME_TYPE_SUITESpouse..partner"             ,     
                        "NAME_TYPE_SUITEUnaccompanied" , "OCCUPATION_TYPE"    ,                             
                        "OCCUPATION_TYPEAccountants"             ,          "OCCUPATION_TYPECleaning.staff"          ,         
                        "OCCUPATION_TYPECooking.staff"            ,         "OCCUPATION_TYPECore.staff"               ,        
                        "OCCUPATION_TYPEDrivers"                   ,        "OCCUPATION_TYPEHigh.skill.tech.staff"     ,       
                        "OCCUPATION_TYPEHR.staff"                   ,       "OCCUPATION_TYPEIT.staff"                   ,      
                        "OCCUPATION_TYPELaborers"                    ,      "OCCUPATION_TYPELow.skill.Laborers"          ,     
                        "OCCUPATION_TYPEManagers"                     ,     "OCCUPATION_TYPEMedicine.staff"               ,    
                        "OCCUPATION_TYPEPrivate.service.staff"         ,    "OCCUPATION_TYPERealty.agents"                 ,   
                        "OCCUPATION_TYPESales.staff"                    ,   "OCCUPATION_TYPESecretaries"                    ,  
                        "OCCUPATION_TYPESecurity.staff"                  ,  "OCCUPATION_TYPEWaiters.barmen.staff"            , 
                        "ORGANIZATION_TYPEAdvertising"                    , "ORGANIZATION_TYPEAgriculture"                    ,
                        "ORGANIZATION_TYPEBank"                            ,"ORGANIZATION_TYPEBusiness.Entity.Type.1"         ,
                        "ORGANIZATION_TYPEBusiness.Entity.Type.2",          "ORGANIZATION_TYPEBusiness.Entity.Type.3"         ,
                        "ORGANIZATION_TYPECleaning"               ,         "ORGANIZATION_TYPEConstruction"                   ,
                        "ORGANIZATION_TYPECulture"                 ,        "ORGANIZATION_TYPEElectricity"                    ,
                        "ORGANIZATION_TYPEEmergency"                ,       "ORGANIZATION_TYPEGovernment"                     ,
                        "ORGANIZATION_TYPEHotel"                     ,      "ORGANIZATION_TYPEHousing"                        ,
                        "ORGANIZATION_TYPEIndustry..type.1"           ,     "ORGANIZATION_TYPEIndustry..type.10"              ,
                        "ORGANIZATION_TYPEIndustry..type.11"           ,    "ORGANIZATION_TYPEIndustry..type.12"              ,
                        "ORGANIZATION_TYPEIndustry..type.13"            ,   "ORGANIZATION_TYPEIndustry..type.2"               ,
                        "ORGANIZATION_TYPEIndustry..type.3"              ,  "ORGANIZATION_TYPEIndustry..type.4"               ,
                        "ORGANIZATION_TYPEIndustry..type.5"               , "ORGANIZATION_TYPEIndustry..type.6"               ,
                        "ORGANIZATION_TYPEIndustry..type.7"                ,"ORGANIZATION_TYPEIndustry..type.8"               ,
                        "ORGANIZATION_TYPEIndustry..type.9" ,               "ORGANIZATION_TYPEInsurance"                      ,
                        "ORGANIZATION_TYPEKindergarten"      ,              "ORGANIZATION_TYPELegal.Services"                 ,
                        "ORGANIZATION_TYPEMedicine"           ,             "ORGANIZATION_TYPEMilitary"                       ,
                        "ORGANIZATION_TYPEMobile"              ,            "ORGANIZATION_TYPEOther"                          ,
                        "ORGANIZATION_TYPEPolice"               ,           "ORGANIZATION_TYPEPostal"                         ,
                        "ORGANIZATION_TYPERealtor"               ,          "ORGANIZATION_TYPEReligion"                       ,
                        "ORGANIZATION_TYPERestaurant"             ,         "ORGANIZATION_TYPESchool"                         ,
                        "ORGANIZATION_TYPESecurity"                ,        "ORGANIZATION_TYPESecurity.Ministries"            ,
                        "ORGANIZATION_TYPESelf.employed"            ,       "ORGANIZATION_TYPEServices"                       ,
                        "ORGANIZATION_TYPETelecom"                   ,      "ORGANIZATION_TYPETrade..type.1"                  ,
                        "ORGANIZATION_TYPETrade..type.2"              ,     "ORGANIZATION_TYPETrade..type.3"                  ,
                        "ORGANIZATION_TYPETrade..type.4"               ,    "ORGANIZATION_TYPETrade..type.5"                  ,
                        "ORGANIZATION_TYPETrade..type.6"                ,   "ORGANIZATION_TYPETrade..type.7"                  ,
                        "ORGANIZATION_TYPETransport..type.1"             ,  "ORGANIZATION_TYPETransport..type.2"              ,
                        "ORGANIZATION_TYPETransport..type.3"              , "ORGANIZATION_TYPETransport..type.4"              ,
                        "ORGANIZATION_TYPEUniversity"                      ,"ORGANIZATION_TYPEXNA"           )

summary(df[,house_vars]) #NAME_HOUSING_TYPE FONDKAPREMONT_MODE HOUSETYPE_MODE WALLSMATERIAL_MODE EMERGENCYSTATE_MODE 
sort(names(dfnum))
house_vars_num <- c("APARTMENTS_AVG", "BASEMENTAREA_AVG",  "YEARS_BEGINEXPLUATATION_AVG" , 
                "YEARS_BUILD_AVG", "COMMONAREA_AVG"  ,"ELEVATORS_AVG"    ,            "ENTRANCES_AVG"               
                , "FLOORSMAX_AVG"      ,          "FLOORSMIN_AVG"          ,      "LANDAREA_AVG"                
                , "LIVINGAPARTMENTS_AVG"     ,    "LIVINGAREA_AVG"    ,           "NONLIVINGAPARTMENTS_AVG"     
                , "NONLIVINGAREA_AVG"         ,   "APARTMENTS_MODE"   ,           "BASEMENTAREA_MODE"           
                , "YEARS_BEGINEXPLUATATION_MODE", "YEARS_BUILD_MODE"   ,          "COMMONAREA_MODE"             
                , "ELEVATORS_MODE"            ,   "ENTRANCES_MODE"     ,          "FLOORSMAX_MODE"              
                , "FLOORSMIN_MODE"             ,  "LANDAREA_MODE"       ,         "LIVINGAPARTMENTS_MODE"       
                , "LIVINGAREA_MODE"             , "NONLIVINGAPARTMENTS_MODE"  ,   "NONLIVINGAREA_MODE"          
                , "APARTMENTS_MEDI"           ,   "BASEMENTAREA_MEDI"          ,  "YEARS_BEGINEXPLUATATION_MEDI"
                , "YEARS_BUILD_MEDI"           ,  "COMMONAREA_MEDI"            ,  "ELEVATORS_MEDI"              
                , "ENTRANCES_MEDI"              , "FLOORSMAX_MEDI"            ,   "FLOORSMIN_MEDI"              
                , "LANDAREA_MEDI"               , "LIVINGAPARTMENTS_MEDI"      ,  "LIVINGAREA_MEDI"             
                , "NONLIVINGAPARTMENTS_MEDI"    , "NONLIVINGAREA_MEDI"                     
                ,"HOUSETYPE_MODE"  ,      "HOUSETYPE_MODEblock.of.flats"               ,      "HOUSETYPE_MODEspecific.housing"      ,            
                "HOUSETYPE_MODEterraced.house"             , "TOTALAREA_MODE"                        
                , "EMERGENCYSTATE_MODE","NAME_HOUSING_TYPECo.op.apartment"    ,            
                "NAME_HOUSING_TYPEHouse...apartment"       ,        "NAME_HOUSING_TYPEMunicipal.apartment"   ,         
                 "NAME_HOUSING_TYPEOffice.apartment"        ,        "NAME_HOUSING_TYPERented.apartment"  ,             
                 "NAME_HOUSING_TYPEWith.parents"  , "FONDKAPREMONT_MODE" ,                             
                 "FONDKAPREMONT_MODEnot.specified"           ,       "FONDKAPREMONT_MODEorg.spec.account"   ,           
                 "FONDKAPREMONT_MODEreg.oper.account"         ,      "FONDKAPREMONT_MODEreg.oper.spec.account"  ,
                "WALLSMATERIAL_MODE"            ,                  
                 "WALLSMATERIAL_MODEBlock"         ,                "WALLSMATERIAL_MODEMixed" ,                        
                 "WALLSMATERIAL_MODEMonolithic"     ,                "WALLSMATERIAL_MODEOthers" ,                       
                 "WALLSMATERIAL_MODEPanel"           ,               "WALLSMATERIAL_MODEStone..brick"   ,               
                 "WALLSMATERIAL_MODEWooden",  "EMERGENCYSTATE_MODENo"                           
                , "EMERGENCYSTATE_MODEYes")


summary(df[,bank_credit_vars]) # chars: NAME_CONTRACT_TYPE FLAG_OWN_REALTY 
sort(names(dfnum))
bank_credit_vars_num <- c(  "AMT_CREDIT"     ,    "AMT_ANNUITY"   ,     "AMT_GOODS_PRICE" ,
                            "NAME_CONTRACT_TYPECash.loans"       ,             "NAME_CONTRACT_TYPERevolving.loans"  ,
                            "FLAG_OWN_REALTYN"  ,                           
                             "FLAG_OWN_REALTYY")
                            

summary(df[,car_vars]) # chars in FLAG_OWN_CAR
sort(names(dfnum))
car_vars_num <- c("OWN_CAR_AGE", "FLAG_OWN_CARN" ,"FLAG_OWN_CARY")

summary(df[,process_start_vars]) # chars in WEEKDAY_APPR_PROCESS_START
sort(names(dfnum))
process_start_vars_num <- c("HOUR_APPR_PROCESS_START", "WEEKDAY_APPR_PROCESS_STARTFRIDAY"                
                            , "WEEKDAY_APPR_PROCESS_STARTMONDAY"        ,         "WEEKDAY_APPR_PROCESS_STARTSATURDAY"              
                            , "WEEKDAY_APPR_PROCESS_STARTSUNDAY"        ,         "WEEKDAY_APPR_PROCESS_STARTTHURSDAY"              
                            , "WEEKDAY_APPR_PROCESS_STARTTUESDAY"       ,         "WEEKDAY_APPR_PROCESS_STARTWEDNESDAY")

summary(df[,CREDIT_BUREAU_vars]) #no chars
summary(df[,flag_vars]) #no chars
summary(df[,exit_source_vars]) #no chars
summary(df[,phone_mail_vars]) #no chars
summary(df[,region_vars]) #no chars
summary(df[,social_circle_vars]) #no chars

#check
total_vars_num <- c(CREDIT_BUREAU_vars,flag_vars, exit_source_vars, phone_mail_vars, region_vars, social_circle_vars,
                    process_start_vars_num, car_vars_num, bank_credit_vars_num, house_vars_num
                    ,personal_vars_num)
length(names(dfnum[,(names(dfnum) %in%  total_vars_num)])) - length(total_vars_num) #check if get our same as total_vars
names(dfnum[,!((names(dfnum) %in%  total_vars_num))])
sub_num <- dfnum[sapply(dfnum, is.numeric)]

# Looking at distributions
##
# Extreme values?
sapply(sub_num, function(x) sum(is.na(x)))
sapply(sub_num, FUN = function(x) quantile(x, seq(0,1,0.1), na.rm = T)) #MÅ TA VEKK NON-numeric vars (går bare på disse)
sapply(sub_num, FUN = function(x) quantile(x, seq(0.99,1,0.001),na.rm = T))
table(sub_num$FLAG_DOCUMENT_10)

# Extreme value plot (can plot the distribution where it looks extreme): Box-plot, Histogram, Scatter Plot
# First play a bit around using table() - then plot if interesting
plot(sub_num$OBS_30_CNT_SOCIAL_CIRCLE)
boxplot(sub_num$OBS_30_CNT_SOCIAL_CIRCLE)
hist(sub_num$OBS_30_CNT_SOCIAL_CIRCLE)

# Visualization (see http://topepo.github.io/caret/visualizations.html for understanding below)
###

# Scatterplot Matrix
library(AppliedPredictiveModeling)
library(caret)
transparentTheme(trans = .4)

df_for_plot <- dfnum[, c("AMT_CREDIT","AMT_ANNUITY","REGION_POPULATION_RELATIVE")] #must do some formatting in order to use featurePlot
df_for_plot$TARGET <- dfnum[, 2]
df_for_plot$TARGET <- as.factor(df_for_plot$TARGET)

featurePlot(x = df_for_plot[,1:3], 
            y = df_for_plot[, 4], 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 2))

# Overlayed Density Plots
featurePlot(x = df_for_plot[,1:3], 
            y = df_for_plot[,4],
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 2))

# Box Plots : Here it is easier to see the mean and extremas. See now that the means are very close, but
# the variace is larger for target=0. So it is clear that this can be a good predictor, but not AS promising (due to not very large volume)
# as what we first discovered in the featureplot...
featurePlot(x = df_for_plot[,1:3], 
            y = df_for_plot[,4], 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))



# Histograms


################################################################################################################################
# Data quality 
################################################################################################################################
summary(df) # here lists NA's per column, max, min, mean, class (if character it says so).. Most important print out!
summary(dfnum)

# Check if unique IDs!
sum(duplicated(df$SK_ID_CURR))

# Look into spesific variables
unique(df$TARGET) 
table(df$TARGET) 

#################################
# Missing values
#################################
col.na.overview <- sapply(dfnum, function(x) sum(is.na(x)))
sort(col.na.overview)

#Rows
#sapply( df, function(f) nrow(complete.cases(df))) 
summary(complete.cases(dfnum))

# Volumes of NA:

# Trends of NA (is it random? Or make sence? Are they pairwise NAs cross vars? Has the NA correlation with target var?)
test_df <- dfnum#[1:10,]
library(plyr)
na.counter <- count(is.na(test_df)) 
na.counter$sum.na <- rowSums(na.counter[,1:ncol(na.counter)-1])
na.counter <- na.counter[order(na.counter$freq, decreasing=TRUE), ]
View(na.counter[, c("freq","sum.na")])
View(na.counter)
names(na.counter[1,])[apply(na.counter[1,], 1, function(i) which(i == TRUE))] # showing NA-columns of the most frequent row pattern

sum(na.counter$freq)
sum(na.counter$sum.na)
plot(na.counter$freq, na.counter$sum.na)

# Plot if not too large df
library(Amelia)
missmap(df) # or can use a code here if not good: https://njtierney.github.io/r/missing%20data/rbloggers/2015/12/01/ggplot-missing-data/

# Simple common sense relations (For example if age of car is NA -> you dont have a car?)
#   See per group: CREDIT_BUREAU_vars,flag_vars, exit_source_vars, phone_mail_vars, region_vars, social_circle_vars,
#   process_start_vars_num, car_vars_num, bank_credit_vars_num, house_vars_num
#   ,personal_vars_num

summary(dfnum[,CREDIT_BUREAU_vars]) # 41519 NA on each. Action: Think this can be good to do dummy 
# (most are zero, then equal ish amount is non-zero and na )
plot(dfnum$AMT_REQ_CREDIT_BUREAU_MON)
sum(dfnum$AMT_REQ_CREDIT_BUREAU_MON > 0, na.rm = TRUE)

sum(is.na(dfnum[,personal_vars_num])) # 2. Action: delete NA
summary(dfnum[,personal_vars_num]) # 2 in CNT_FAM_MEMBERS. 

summary(dfnum[,flag_vars]) #generally low mean but no NA. Action: keep

summary(dfnum[,exit_source_vars]) #quite some NA's, especially one of them
hist(dfnum$EXT_SOURCE_1) # Action: split as dummies where one dummy is NA, rest is intervals

summary(dfnum[,phone_mail_vars]) #only one NA's. Some very little variance . Action: delete NA

summary(dfnum[,house_vars_num]) #loads of NA (not all tho)
srtt <- sort(sapply(dfnum[,house_vars_num], function(x) sum(is.na(x))))
names(srtt)
srtt[27] #first non-zero
for(i in 27:length(srtt)){
  hist(dfnum[,i])
  title(sub = names(srtt[i]))}

table(dfnum$YEARS_BUILD_MODE > 0.81)
#Action: split as dummies where one dummy is NA, rest is intervals


summary(dfnum[,bank_credit_vars_num]) #little NA. Action: delete NAs

summary(dfnum[,car_vars_num]) #loads NA on car age. Action: make dummy intervals

summary(dfnum[,process_start_vars_num]) #no NA ok

summary(dfnum[,social_circle_vars]) #little NA. Action: delete NAs

# Missing values - What to do with them?
##

# Will depend on percentage of missing values, variables affected by missing values, 
# whether those missing values are a part of dependent or the independent variables, etc.
# See https://www.datasciencecentral.com/profiles/blogs/how-to-treat-missing-values-in-your-data-1

# Alt. 0: Making dummies of NA
# For example if age of car is NA -> you dont have a car? Then can make a binary dummy depending on NA or not
# But then must do dummy of the rest too - eg "has a car of age between x and y", etc.
# This is very relevant for columns with a very high percentage of NAs -> thus we transform it into NA/nonNA, and perhaps some intervals as previous 
# sentence suggests
df_clean <- dfnum

# CREDIT_BUREAU_vars
summary(df_clean[,CREDIT_BUREAU_vars])
#Check if all are NA at same time
table(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_YEAR), is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT))
table(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_MON), is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT))
table(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_WEEK), is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT))
table(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_DAY), is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT))
table(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_HOUR), is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT))
# all NA at same

table(df_clean$AMT_REQ_CREDIT_BUREAU_YEAR )
table(df_clean$AMT_REQ_CREDIT_BUREAU_MON )
table(df_clean$AMT_REQ_CREDIT_BUREAU_WEEK )
table(df_clean$AMT_REQ_CREDIT_BUREAU_DAY )
table(df_clean$AMT_REQ_CREDIT_BUREAU_HOUR)

df_clean$AMT_REQ_CREDIT_BUREAU_HOUR_NA <- ifelse(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_HOUR),1,0)
df_clean$AMT_REQ_CREDIT_BUREAU_HOUR_NULL <- ifelse(!is.na(df_clean$AMT_REQ_CREDIT_BUREAU_HOUR) & (df_clean$AMT_REQ_CREDIT_BUREAU_HOUR) == 0,1,0)
#here we skip making a third variable, since it will be derivated from NA and NULL
df_clean$AMT_REQ_CREDIT_BUREAU_HOUR <- NULL

df_clean$AMT_REQ_CREDIT_BUREAU_QRT <- ifelse(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT),1,0)
df_clean$AMT_REQ_CREDIT_BUREAU_QRT_NULL <- ifelse(!is.na(df_clean$AMT_REQ_CREDIT_BUREAU_QRT) & (df_clean$AMT_REQ_CREDIT_BUREAU_QRT) == 0,1,0)
#here we skip making a third variable, since it will be derivated from NA and NULL
df_clean$AMT_REQ_CREDIT_BUREAU_QRT <- NULL

df_clean$AMT_REQ_CREDIT_BUREAU_MON <- ifelse(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_MON),1,0)
df_clean$AMT_REQ_CREDIT_BUREAU_MON_NULL <- ifelse(!is.na(df_clean$AMT_REQ_CREDIT_BUREAU_MON) & (df_clean$AMT_REQ_CREDIT_BUREAU_MON) == 0,1,0)
#here we skip making a third variable, since it will be derivated from NA and NULL
df_clean$AMT_REQ_CREDIT_BUREAU_MON <- NULL

df_clean$AMT_REQ_CREDIT_BUREAU_WEEK <- ifelse(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_WEEK),1,0)
df_clean$AMT_REQ_CREDIT_BUREAU_WEEK_NULL <- ifelse(!is.na(df_clean$AMT_REQ_CREDIT_BUREAU_WEEK) & (df_clean$AMT_REQ_CREDIT_BUREAU_WEEK) == 0,1,0)
#here we skip making a third variable, since it will be derivated from NA and NULL
df_clean$AMT_REQ_CREDIT_BUREAU_MON <- NULL

df_clean$AMT_REQ_CREDIT_BUREAU_YEAR <- ifelse(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_YEAR),1,0)
df_clean$AMT_REQ_CREDIT_BUREAU_YEAR_NULL <- ifelse(!is.na(df_clean$AMT_REQ_CREDIT_BUREAU_YEAR) & (df_clean$AMT_REQ_CREDIT_BUREAU_YEAR) == 0,1,0)
#here we skip making a third variable, since it will be derivated from NA and NULL
df_clean$AMT_REQ_CREDIT_BUREAU_YEAR <- NULL

df_clean$AMT_REQ_CREDIT_BUREAU_DAY <- ifelse(is.na(df_clean$AMT_REQ_CREDIT_BUREAU_DAY),1,0)
df_clean$AMT_REQ_CREDIT_BUREAU_DAY_NULL <- ifelse(!is.na(df_clean$AMT_REQ_CREDIT_BUREAU_DAY) &(df_clean$AMT_REQ_CREDIT_BUREAU_DAY) == 0,1,0)
#here we skip making a third variable, since it will be derivated from NA and NULL
df_clean$AMT_REQ_CREDIT_BUREAU_DAY <- NULL

#personal_vars_num: delete na
dim(df_clean)
summary(df_clean[,personal_vars_num])
df_clean <- df_clean[!is.na(df_clean$CNT_FAM_MEMBERS),]
dim(df_clean)

#exit_source_vars : make dummy and remove NA in EXT_SOURCE_2
summary(df_clean[,exit_source_vars])
hist(df_clean$EXT_SOURCE_1)
df_clean$EXT_SOURCE_1_low <- ifelse(!is.na(df_clean$EXT_SOURCE_1) & (df_clean$EXT_SOURCE_1 < 0.5),1,0)
df_clean$EXT_SOURCE_1_high <- ifelse(!is.na(df_clean$EXT_SOURCE_1) & (df_clean$EXT_SOURCE_1 > 0.5),1,0)
head(df_clean[,c(exit_source_vars,"EXT_SOURCE_1_low", "EXT_SOURCE_1_high" )]) #ok
df_clean$EXT_SOURCE_1 <- NULL

summary(df_clean[,"EXT_SOURCE_3"])
hist(df_clean[,"EXT_SOURCE_3"])
df_clean$EXT_SOURCE_3_low <- ifelse(!is.na(df_clean$EXT_SOURCE_3) & (df_clean$EXT_SOURCE_3 < 0.5),1,0)
df_clean$EXT_SOURCE_3_high <- ifelse(!is.na(df_clean$EXT_SOURCE_3) & (df_clean$EXT_SOURCE_3 > 0.5),1,0)
head(df_clean[,c("EXT_SOURCE_3_low", "EXT_SOURCE_3_high", "EXT_SOURCE_3" )]) #ok
df_clean$EXT_SOURCE_3 <- NULL

dim(df_clean)
summary(df_clean[,"EXT_SOURCE_2"])
df_clean <- df_clean[!is.na(df_clean$EXT_SOURCE_2),]
dim(df_clean)




#phone_mail_vars: delete 1 na
dim(df_clean)
summary(df_clean[,phone_mail_vars]) #already gone :) 

#house_vars_num: make dummy
#now just as I am lazy I delete the columns having lot of na...
summary(df_clean[,house_vars_num])
house_list <- sort(sapply(df_clean[,house_vars_num], function(x) sum(is.na(x))))
house_list_move <- house_list[27:length(house_list)]
df_clean <- (subset(df_clean,select=(!colnames(df_clean)%in%(names(house_list_move)))))

#bank_credit_vars_num: delete na
summary(df_clean[,bank_credit_vars_num])
dim(df_clean)
df_clean <- df_clean[!is.na(df_clean$AMT_ANNUITY),]
df_clean <- df_clean[!is.na(df_clean$AMT_GOODS_PRICE),]
dim(df_clean)

#social_circle_vars: delete na
summary(df_clean[,social_circle_vars])
dim(df_clean)
df_clean <- df_clean[!is.na(df_clean$DEF_30_CNT_SOCIAL_CIRCLE),]
dim(df_clean)
#all na gone..

#,car_vars_num: make dummy
summary(df_clean[,car_vars_num])
hist(df_clean$OWN_CAR_AGE)
table(is.na(df_clean$OWN_CAR_AGE),df_clean$FLAG_OWN_CARN)
df_clean$OWN_CAR_AGE_0_10 <- ifelse(!is.na(df_clean$OWN_CAR_AGE) & (df_clean$OWN_CAR_AGE < 10),1,0)
df_clean$OWN_CAR_AGE_10_up <- ifelse(!is.na(df_clean$OWN_CAR_AGE) & (df_clean$OWN_CAR_AGE > 10),1,0)
df_clean$OWN_CAR_AGE <- NULL

#Grand check
sum(is.na(df_clean))
summary(df_clean)


# Alt. 1: Deletion
# - Unless the nature of missing data is 'Missing completely at random', the best 
#   avoidable method in many cases is deletion. Especially if ratio of NA is small...
# - Get information loss..
###

# Alt. 1: Imputation
##
# - Popular Averaging Techniques
# - Predictive Techniques (e.g. regression)
# See the libraries Amelia, MICE, missForest in https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17

#################################
# Look for 0 variance columns
#################################
# Looking for variables with none or little variance
nzv <- nearZeroVar(df_clean, saveMetrics= TRUE)

nzv[nzv$nzv,][,]
nzv_df <- nzv[nzv$nzv,][,] 
nzv_df[nzv_df$zeroVar == TRUE,]
table(df_clean$FLAG_MOBIL)


# Inspection of near-zero variance
for(i in 1:dim(nzv[nzv$nzv,][,])[1]){
  if(dim(table(df_clean[,rownames(nzv[nzv$nzv,][,])[i]]))[1] < 15){
    print(rownames(nzv[nzv$nzv,][,])[i])
    print(table(df_clean[,rownames(nzv[nzv$nzv,][,])[i]], df_clean$TARGET))
  }
}

# Describe what kind of variables have little variance - and an assessment if they would be removed 
# - at least we remove zeroVar (no brainer)
# - ok removing FLAG_DOCUMENT_7 etc -> doesnt make too much sence
# - lot of dummy vars that have nzv: make sence ok to let them go
# - some of them should we keep and transform (DAYS_EMPLOYED etc.). For now we let them go..
table(df_clean$NAME_INCOME_TYPEStudent)

hist(df_clean$DAYS_EMPLOYED) # should make dummy..
table(df_clean$DAYS_EMPLOYED>0)
hist(df_clean[df_clean$DAYS_EMPLOYED <0,]$DAYS_EMPLOYED) 
hist(df_clean[df_clean$DAYS_EMPLOYED >0,]$DAYS_EMPLOYED) 

df_clean <- data.table(df_clean)
filteredDescr <- df_clean[, -nzv]
dim(df)
dim(df_clean)
dim(filteredDescr) 
dim(filteredDescr[filteredDescr$nzv == -1,])
df_nzv <- subset(df_clean, select=(!colnames(df_clean)%in%(rownames(filteredDescr[filteredDescr$nzv == -1,]))))
df_nzv <- data.frame(df_nzv)  


#################################
# Identifying Correlated Predictors (http://topepo.github.io/caret/pre-processing.html#corr)
#################################
descrCor <-  cor(df_nzv)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75) #  Cant handle missing values! This function searches through a 
# correlation matrix and returns a vector of integers 
# corresponding to columns to remove to reduce pair-wise correlations.

#Removing
df_nzv_corr <- df_nzv[,-highlyCorDescr]
dim(df_nzv_corr)
colnames(df_nzv_corr)
descrCor_after <-  cor(df_nzv_corr)
summary(descrCor[upper.tri(descrCor_after)])
summary(descrCor[upper.tri(descrCor)])
#Was this an improvement??

# Plot to see if correlation make sense
# See https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/#one


#################################
# Linear Dependencies (http://topepo.github.io/caret/pre-processing.html#corr)
#################################
# with df_nzv_corr
comboInfo <- findLinearCombos(df_nzv_corr) #dont work with NA
df_nzv_corr <- data.frame(df_nzv_corr)
head(df_nzv_corr[,comboInfo$linearCombos[[1]]])
head(df_nzv_corr[,comboInfo$remove])
colnames(df_nzv_corr)[comboInfo$remove]
df_nzv_corr_lin <- df_nzv_corr[,-comboInfo$remove]
dim(df_nzv_corr_lin)
dim(df_nzv_corr)

descrCor_after2 <-  cor(df_nzv_corr_lin)
summary(descrCor[upper.tri(descrCor_after2)])


# with df_nzv
comboInfo <- findLinearCombos(df_nzv) #dont work with NA
df_nzv <- data.frame(df_nzv)
df_nzv_lin <- df_nzv[,-comboInfo$remove]
dim(df_nzv_lin)
dim(df_nzv)
descrCor_after2 <-  cor(df_nzv_lin)
summary(descrCor[upper.tri(descrCor_after2)])
summary(descrCor[upper.tri(descrCor)])
#improve?

####################################################################################################################################
# Data Exploration and Data Quality report  
####################################################################################################################################
# Here make a wrap up of the data: both the quality and what the data tell us already now
df_final <- df_nzv_corr_lin #or df_nzv?
View(df_final)

##########################
# Quality
##########################
# Make some graphs if applicable:

# NAs
####

# Little variance
####

# Bugs
####

# Meaningless columns / values (e.g. phone number, or something that is irrelevant or just dont make sense)
####

# Uniqueness
####


##########################
# Understanding
##########################
# Make some graphs if applicable:

# Skewness
####

# Hypothesis about important variables
####

# Segments
####
# Here group a few variables that seem interesting. E.g. histograms with age vs location vs target var

# Trends
####

################################################################################################################################
# OUTPUT
################################################################################################################################

# Graphs giving some insight in our population

# Quality report

# Grouping of columns (make star-like representation: personal info, building info, car info, etc)

# Suggest which columns that we might skip, at least in the first round. 

# Hypothesis (possible very important variables, as well as some other insight)

# Powerpoint presentation 

# We did here quite some prep - as especially NA treatment in easy tody together with data quality assessment etc..
# Thus we save the dataset here. This is okay as a first iteration - then move more over to data prep script. 
# Should work a bit agile and iterative =)
write_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/1dataUnderstanding/df_pre_prepped.csv"

write.csv(df_final,file = write_path, row.names=FALSE )



