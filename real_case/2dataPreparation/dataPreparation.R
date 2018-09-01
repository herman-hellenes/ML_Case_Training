# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "01/09/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: This script does the final data prep, in order to pass it to the modelling part

# Questions: 
#     - ...

# Todo
#     - ...

# Output:
#     - ...


library(data.table)
library(caret)
library(plyr)

################################################################################################################################
# INPUT
################################################################################################################################
raw_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/From Cecilia/Machine Learning Case_PowerCo/ml_case_data/"


ml_case_training_output <- read.csv(paste0(raw_path, "ml_case_training_output.csv"),
                                    sep = ",", dec=".", stringsAsFactors = F)

ml_case_training_hist_data <- read.csv(paste0(raw_path, "ml_case_training_hist_data.csv"),
                                       sep = ",", dec=".", stringsAsFactors = F)

ml_case_training_data <- read.csv(paste0(raw_path, "ml_case_training_data.csv"),
                                  sep = ",", dec=".", stringsAsFactors = F)

ml_case_test_hist_data <- read.csv(paste0(raw_path, "ml_case_test_hist_data.csv"),
                                   sep = ",", dec=".", stringsAsFactors = F)

ml_case_test_data <- read.csv(paste0(raw_path, "ml_case_test_data.csv"),
                              sep = ",", dec=".", stringsAsFactors = F)


################################################################################################################################
# Data Preparation 
################################################################################################################################
sum(ml_case_training_data$id %in% ml_case_test_data$id) #0

# Merging : add on churn var
traindfTarget <- join(ml_case_training_data, ml_case_training_output, by = "id", type = "left", match = "all")
dim(traindfTarget)
dim(ml_case_training_data)
head(traindfTarget)
sum(ml_case_training_data$id != ml_case_training_output$id) #already sorted - so not too much that can go wrong in join

#################################
# MISSING VALUES
#################################
# Delete campaign_disc_ele (only NA)
traindfTarget$campaign_disc_ele <- NULL
ml_case_test_data$campaign_disc_ele <- NULL

#NUMERICS
##
#Few NAs(delete in train, impute in test): net_margin, margin_gross_pow_ele, forecast_price_energy_p2

#TEST: for now just pick mean value and insert when missing
dfTest <- ml_case_test_data

dfTest$net_margin <- ifelse(is.na(dfTest$net_margin),mean(dfTest[!(is.na(dfTest$net_margin)),]$net_margin),dfTest$net_margin)
dfTest$margin_net_pow_ele <- ifelse(is.na(dfTest$margin_net_pow_ele),mean(dfTest[!(is.na(dfTest$margin_net_pow_ele)),]$margin_net_pow_ele),dfTest$margin_net_pow_ele)
dfTest$pow_max <- ifelse(is.na(dfTest$pow_max),mean(dfTest[!(is.na(dfTest$pow_max)),]$pow_max),dfTest$pow_max)
dfTest$margin_gross_pow_ele <- ifelse(is.na(dfTest$margin_gross_pow_ele),mean(dfTest[!(is.na(dfTest$margin_gross_pow_ele)),]$margin_gross_pow_ele),dfTest$margin_gross_pow_ele)
dfTest$forecast_price_pow_p1 <- ifelse(is.na(dfTest$forecast_price_pow_p1),mean(dfTest[!(is.na(dfTest$forecast_price_pow_p1)),]$forecast_price_pow_p1),dfTest$forecast_price_pow_p1)
dfTest$forecast_price_energy_p2 <- ifelse(is.na(dfTest$forecast_price_energy_p2),mean(dfTest[!(is.na(dfTest$forecast_price_energy_p2)),]$forecast_price_energy_p2),dfTest$forecast_price_energy_p2)
dfTest$forecast_price_energy_p1 <- ifelse(is.na(dfTest$forecast_price_energy_p1),mean(dfTest[!(is.na(dfTest$forecast_price_energy_p1)),]$forecast_price_energy_p1),dfTest$forecast_price_energy_p1)
dfTest$forecast_discount_energy <- ifelse(is.na(dfTest$forecast_discount_energy),mean(dfTest[!(is.na(dfTest$forecast_discount_energy)),]$forecast_discount_energy),dfTest$forecast_discount_energy)

summary(dfTest)

#Train
traindfTarget <- traindfTarget[!(is.na(traindfTarget$net_margin)),]
traindfTarget <- traindfTarget[!(is.na(traindfTarget$margin_gross_pow_ele)),]
traindfTarget <- traindfTarget[!(is.na(traindfTarget$net_margin)),]
traindfTarget <- traindfTarget[!(is.na(traindfTarget$forecast_discount_energy)),] #ok

#Many NAs: create dummies
traindfTarget$forecast_consNA <- ifelse(is.na(traindfTarget$forecast_cons) , 1,0)
traindfTarget$forecast_cons <- NULL
dfTest$forecast_consNA <- ifelse(is.na(dfTest$forecast_cons) , 1,0)
dfTest$forecast_cons <- NULL

traindfTarget$forecast_bill_12mNA <- ifelse(is.na(traindfTarget$forecast_bill_12m) , 1,0)
traindfTarget$forecast_bill_12m <- NULL
dfTest$forecast_bill_12mNA <- ifelse(is.na(dfTest$forecast_bill_12m) , 1,0)
dfTest$forecast_bill_12m <- NULL

traindfTarget$forecast_base_bill_eleNA <- ifelse(is.na(traindfTarget$forecast_base_bill_ele) , 1,0)
traindfTarget$forecast_base_bill_ele <- NULL
dfTest$forecast_base_bill_eleNA <- ifelse(is.na(dfTest$forecast_base_bill_ele) , 1,0)
dfTest$forecast_base_bill_ele <- NULL

traindfTarget$forecast_base_bill_yearNA <- ifelse(is.na(traindfTarget$forecast_base_bill_year) , 1,0)
traindfTarget$forecast_base_bill_year <- NULL
dfTest$forecast_base_bill_yearNA <- ifelse(is.na(dfTest$forecast_base_bill_year) , 1,0)
dfTest$forecast_base_bill_year <- NULL

dim(traindfTarget)
dim(dfTest)

sum(!(colnames(traindfTarget[,!(colnames(traindfTarget) %in% "churn")]) %in% colnames(dfTest)))
sort(colnames(traindfTarget[,!(colnames(traindfTarget) %in% "churn")]))
sort(colnames(dfTest))

# Categorical Missing values 
##
df.char.clean <- traindfTarget
test.char.clean <- dfTest

# date_end: 
sum(df.char.clean$date_end == "") #2
df.char.clean <- df.char.clean[!((df.char.clean$date_end) == ""),]
sum(dfTest$date_end == "") #0 - good

df.char.clean$date_end <- as.POSIXct(df.char.clean$date_end)
min(df.char.clean$date_end)
max(df.char.clean$date_end)
df.char.clean$date_end <- as.numeric(df.char.clean$date_end)
df.char.clean$date_end <- df.char.clean$date_end - min(df.char.clean$date_end)

dfTest$date_end <- as.POSIXct(dfTest$date_end)
min(dfTest$date_end)
max(dfTest$date_end)
dfTest$date_end <- as.numeric(dfTest$date_end)
dfTest$date_end <- dfTest$date_end - min(dfTest$date_end)

# date_activ: date of activation of the contract
sum(df.char.clean$date_activ == "") #0
sum(dfTest$date_activ == "") #0
df.char.clean$date_activ <- as.POSIXct(df.char.clean$date_activ)
df.char.clean$date_activ <- as.numeric(df.char.clean$date_activ)
df.char.clean$date_activ <- df.char.clean$date_activ - min(df.char.clean$date_activ)
dfTest$date_activ <- as.POSIXct(dfTest$date_activ)
dfTest$date_activ <- as.numeric(dfTest$date_activ)
dfTest$date_activ <- dfTest$date_activ - min(dfTest$date_activ)


# date_modif_prod: 
sum(df.char.clean$date_modif_prod == "") # 156
df.char.clean <- df.char.clean[!((df.char.clean$date_modif_prod) == ""),]
sum(dfTest$date_modif_prod == "") #45

df.char.clean$date_modif_prod <- as.POSIXct(df.char.clean$date_modif_prod)
min(df.char.clean$date_modif_prod)
max(df.char.clean$date_modif_prod)
df.char.clean$date_modif_prod <- as.numeric(df.char.clean$date_modif_prod)
df.char.clean$date_modif_prod <- df.char.clean$date_modif_prod - min(df.char.clean$date_modif_prod)

dfTest$date_modif_prod <- ifelse((dfTest$date_modif_prod)=="","2020-08-27",dfTest$date_modif_prod)
dfTest$date_modif_prod <- as.POSIXct(dfTest$date_modif_prod)
min(dfTest$date_modif_prod)
max(dfTest$date_modif_prod)
dfTest$date_modif_prod <- as.numeric(dfTest$date_modif_prod)
dfTest$date_modif_prod <- ifelse(dfTest$date_modif_prod == max(dfTest$date_modif_prod), 
                                 mean(dfTest[!(dfTest$date_modif_prod)==max(dfTest$date_modif_prod),]$date_modif_prod),
                                 dfTest$date_modif_prod) 
dfTest$date_modif_prod <- dfTest$date_modif_prod - min(dfTest$date_modif_prod)

#date_renewal
sum(df.char.clean$date_renewal == "") # 38
df.char.clean <- df.char.clean[!((df.char.clean$date_renewal) == ""),]
sum(dfTest$date_renewal == "") # 4

df.char.clean$date_renewal <- as.POSIXct(df.char.clean$date_renewal)
min(df.char.clean$date_renewal)
max(df.char.clean$date_renewal)
df.char.clean$date_renewal <- as.numeric(df.char.clean$date_renewal)
df.char.clean$date_renewal <- df.char.clean$date_renewal - min(df.char.clean$date_renewal)

dfTest$date_renewal <- ifelse((dfTest$date_renewal)=="","2020-08-27",dfTest$date_renewal)
dfTest$date_renewal <- as.POSIXct(dfTest$date_renewal)
min(dfTest$date_renewal)
max(dfTest$date_renewal)
dfTest$date_renewal <- as.numeric(dfTest$date_renewal)
dfTest$date_renewal <- ifelse(dfTest$date_renewal == max(dfTest$date_renewal), 
                                 mean(dfTest[!(dfTest$date_renewal)==max(dfTest$date_renewal),]$date_renewal),
                                 dfTest$date_renewal) 
dfTest$date_renewal <- dfTest$date_renewal - min(dfTest$date_renewal)

#date_first_activ
sum(df.char.clean$date_first_activ == "") # 12494
table(df.char.clean$date_first_activ=="")
df.char.clean$date_first_activNA <- ifelse(df.char.clean$date_first_activ=="",1,0)
df.char.clean$date_first_activ <- NULL
sum(dfTest$date_first_activ == "") # 12494
dfTest$date_first_activNA <- ifelse(dfTest$date_first_activ=="",1,0)
dfTest$date_first_activ <- NULL



summary(df.char.clean)
summary(dfTest)
dim(df.char.clean)
dim(dfTest)

sum(!(colnames(df.char.clean[,!(colnames(df.char.clean) %in% "churn")]) %in% colnames(dfTest)))
sort(colnames(df.char.clean[,!(colnames(df.char.clean) %in% "churn")]))
sort(colnames(dfTest))

### MAKE DUMMIES #### 
dfnum <- df.char.clean
dfTestNum <- dfTest
str(dfnum)

#activity_new
dfnum$activity_new.NA <- ifelse((df.char.clean$activity_new) =="" , 1,0)
dfnum$activity_new.apdekpcbwosbxepsfxclislboipuxpop <- ifelse((df.char.clean$activity_new) =="apdekpcbwosbxepsfxclislboipuxpop" , 1,0)
dfnum$activity_new.kkklcdamwfafdcfwofuscwfwadblfmce <- ifelse((df.char.clean$activity_new) =="kkklcdamwfafdcfwofuscwfwadblfmce" , 1,0)
dfnum$activity_new <- NULL

dfTestNum$activity_new.NA <- ifelse((dfTestNum$activity_new) =="" , 1,0)
dfTestNum$activity_new.apdekpcbwosbxepsfxclislboipuxpop <- ifelse((dfTestNum$activity_new) =="apdekpcbwosbxepsfxclislboipuxpop" , 1,0)
dfTestNum$activity_new.kkklcdamwfafdcfwofuscwfwadblfmce <- ifelse((dfTestNum$activity_new) =="kkklcdamwfafdcfwofuscwfwadblfmce" , 1,0)
dfTestNum$activity_new <- NULL

colnames(dfTestNum) %in% colnames(dfnum)
# rest of dummies
dfnum <- dfnum[,!(colnames(dfnum) %in% c("id"))]
dmy <- dummyVars(" ~ .", data = dfnum) #from caret -> making dummy vars
dfnum <- data.frame(predict(dmy, newdata = dfnum))
dim(dfnum)
#str(dfnum)

dfTestNum <- dfTestNum[,!(colnames(dfTestNum) %in% c("id"))]
dmy <- dummyVars(" ~ .", data = dfTestNum) #from caret -> making dummy vars
dfTestNum <- data.frame(predict(dmy, newdata = dfTestNum))
dim(dfTestNum)

# Must make sure get same dummies
colnames(dfTestNum[,!(colnames(dfTestNum) %in% colnames(dfnum))])
colnames(dfnum[,!(colnames(dfTestNum) %in% colnames(dfnum))])
table(dfTestNum$channel_salesfixdbufsefwooaasfcxdxadsiekoceaa)
table(dfTestNum$origin_upaabpopmuoobccoxasfsksebxoxffdcxs)
dfTestNum$origin_upaabpopmuoobccoxasfsksebxoxffdcxs <- NULL
dfTestNum$channel_salesfixdbufsefwooaasfcxdxadsiekoceaa <- NULL

colnames(dfnum[,!(colnames(dfnum) %in% colnames(dfTestNum))])
dfnum$origin_upusapbepcfoloekilkwsdiboslwaxobdp <- NULL
dfnum$origin_upewxeelcelemmiwuafmddpobolfuxioce <- NULL

sum(!(colnames(dfnum[,!(colnames(dfnum) %in% "churn")]) %in% colnames(dfTestNum)))
sort(colnames(dfnum[,!(colnames(dfnum) %in% "churn")]))
sort(colnames(dfTestNum))


dim(dfTestNum) #42
dim(dfnum) # 43
#str(dfTestNum)

#Grand check
sum(is.na(dfnum))
sum(is.na(dfTestNum))

df_clean <- dfnum
dim(df.char.clean)
dim(df_clean) 
df_clean$id <- df.char.clean$id

dfTest_clean <- dfTestNum
dim(dfTest)
dim(dfTest_clean) 
dfTest_clean$id <- dfTest$id

colnames(dfTest_clean[,!(colnames(dfTest_clean) %in% colnames(df_clean))])
colnames(dfTest_clean[,!(colnames(df_clean) %in% colnames(dfTest_clean))])
colnames(df_clean[,!(colnames(df_clean) %in% colnames(dfTest_clean))])
colnames(df_clean[,!(colnames(dfTest_clean) %in% colnames(df_clean))])


sum(!(colnames(df_clean[,!(colnames(df_clean) %in% "churn")]) %in% colnames(dfTest_clean)))
sort(colnames(df_clean[,!(colnames(df_clean) %in% "churn")]))
sort(colnames(dfTest_clean))


#################################
# Look for 0 variance columns
#################################
# Looking for variables with none or little variance
nzv <- nearZeroVar(df_clean, saveMetrics= TRUE)

nzv[nzv$nzv,][,]

# Inspection of near-zero variance vs churn
for(i in 1:dim(nzv[nzv$nzv,][,])[1]){
  if(dim(table(df_clean[,rownames(nzv[nzv$nzv,][,])[i]]))[1] < 15){
    print(rownames(nzv[nzv$nzv,][,])[i])
    print(table(df_clean[,rownames(nzv[nzv$nzv,][,])[i]], df_clean$churn))
  }
}


table(df_clean$channel_salesepumfxlbckeskwekxbiuasklxalciiuu) # remove
df_clean$channel_salesepumfxlbckeskwekxbiuasklxalciiuu <- NULL
dfTest_clean$channel_salesepumfxlbckeskwekxbiuasklxalciiuu <- NULL

table(df_clean$channel_salessddiedcslfslkckwlfkdpoeeailfpeds) # remove
df_clean$channel_salessddiedcslfslkckwlfkdpoeeailfpeds <- NULL
dfTest_clean$channel_salessddiedcslfslkckwlfkdpoeeailfpeds <- NULL

table(df_clean$forecast_discount_energy) # remove
df_clean$forecast_discount_energy <- NULL
dfTest_clean$forecast_discount_energy <- NULL

table(df_clean$origin_up) # remove
df_clean$origin_up <- NULL
dfTest_clean$origin_up <- NULL

table(df_clean$origin_upewxeelcelemmiwuafmddpobolfuxioce) # remove
df_clean$origin_upewxeelcelemmiwuafmddpobolfuxioce <- NULL
dfTest_clean$origin_upewxeelcelemmiwuafmddpobolfuxioce <- NULL

table(df_clean$origin_upusapbepcfoloekilkwsdiboslwaxobdp) # remove
df_clean$origin_upusapbepcfoloekilkwsdiboslwaxobdp <- NULL
dfTest_clean$origin_upusapbepcfoloekilkwsdiboslwaxobdp <- NULL

table(df_clean$activity_new.kkklcdamwfafdcfwofuscwfwadblfmce) # hold

df_clean$has_gast <- NULL #as this is captured
dfTest_clean$has_gast <- NULL #as this is captured


dim(df_clean)
dim(dfTest_clean)

sum(!(colnames(df_clean[,!(colnames(df_clean) %in% "churn")]) %in% colnames(dfTest_clean)))
sort(colnames(df_clean[,!(colnames(df_clean) %in% "churn")]))
sort(colnames(dfTest_clean))

#################################
# Linear Dependencies (http://topepo.github.io/caret/pre-processing.html#corr)
#################################
df_clean_corr <- df_clean
df_clean_corr$id <- NULL

comboInfo <- findLinearCombos(df_clean_corr) #dont work with NA
df_clean_corr <- data.frame(df_clean_corr)
head(df_clean_corr[,comboInfo$linearCombos[[1]]])
head(df_clean_corr[,comboInfo$remove])
colnames(df_clean_corr)[comboInfo$remove]
df_clean_corr_lin <- df_clean_corr[,-comboInfo$remove]
dim(df_clean_corr)
dim(df_clean_corr_lin)

colnames(df_clean_corr)[comboInfo$remove]
dfTest_clean_lin <- dfTest_clean
dfTest_clean_lin$forecast_bill_12mNA <- NULL
dfTest_clean_lin$forecast_base_bill_eleNA <- NULL
dfTest_clean_lin$forecast_base_bill_yearNA <- NULL
dfTest_clean_lin$date_first_activNA <- NULL

dim(dfTest_clean_lin)
dim(dfTest_clean)


df_clean_corr_lin$id <- df_clean$id

dim(dfTest_clean_lin)
dim(df_clean_corr_lin)

sum(!(colnames(df_clean_corr_lin[,!(colnames(df_clean_corr_lin) %in% "churn")]) %in% colnames(dfTest_clean_lin)))
sort(colnames(df_clean_corr_lin[,!(colnames(df_clean_corr_lin) %in% "churn")]))
sort(colnames(dfTest_clean_lin))

#################################
# Historical data:
#################################
trainHistdf <- ml_case_training_hist_data 
str(trainHistdf)
unique(trainHistdf$price_date)
trainHistdfEdit <- trainHistdf
trainHistdfEdit$price_date_num <- as.POSIXct(trainHistdfEdit$price_date)
trainHistdfEdit$price_date_num <- month(trainHistdfEdit$price_date_num)

testHistdf <- ml_case_test_hist_data 
str(testHistdf)
unique(testHistdf$price_date)
testHistdfEdit <- testHistdf
testHistdfEdit$price_date_num <- as.POSIXct(testHistdfEdit$price_date)
testHistdfEdit$price_date_num <- month(testHistdfEdit$price_date_num)


# the fixed(p1-p3) look very correlated. save for var. thus we sum them
trainHistdfEdit$price_tot_var <- trainHistdfEdit$price_p1_var + trainHistdfEdit$price_p2_var + trainHistdfEdit$price_p3_var
trainHistdfEdit$price_tot_fix <- trainHistdfEdit$price_p1_fix + trainHistdfEdit$price_p2_fix + trainHistdfEdit$price_p3_fix

testHistdfEdit$price_tot_var <- testHistdfEdit$price_p1_var + testHistdfEdit$price_p2_var + testHistdfEdit$price_p3_var
testHistdfEdit$price_tot_fix <- testHistdfEdit$price_p1_fix + testHistdfEdit$price_p2_fix + testHistdfEdit$price_p3_fix


#NA
sum(is.na(trainHistdfEdit$price_p3_fix))
sum(is.na(testHistdfEdit$price_p3_fix))

#cast
library(reshape2)
trainHistdfEdit.Var <- dcast(trainHistdfEdit, id  ~ price_date_num , value.var = "price_tot_var" )
sum(duplicated(trainHistdfEdit.Var$id)) #0
length(unique(trainHistdfEdit.Var$id)) #16096

testHistdfEdit.Var <- dcast(testHistdfEdit, id  ~ price_date_num , value.var = "price_tot_var" )
sum(duplicated(testHistdfEdit.Var$id)) #0
length(unique(testHistdfEdit.Var$id)) #4024

trainHistdfEdit.Fix <- dcast(trainHistdfEdit, id  ~ price_date_num , value.var = "price_tot_fix" )
sum(duplicated(trainHistdfEdit.Fix$id)) #0
length(unique(trainHistdfEdit.Fix$id)) #16096

testHistdfEdit.Fix <- dcast(testHistdfEdit, id  ~ price_date_num , value.var = "price_tot_fix" )
sum(duplicated(testHistdfEdit.Fix$id)) #0
length(unique(testHistdfEdit.Fix$id)) #4024

sum(!(trainHistdfEdit.Fix$id == trainHistdfEdit.Var$id)) #0 - can just merge
trainHistdfEdit.Done <- trainHistdfEdit.Fix[,colnames(trainHistdfEdit.Fix) %in% c("id", "1", "12")]
colnames(trainHistdfEdit.Done)[2] <- "price_tot_fix_1"
colnames(trainHistdfEdit.Done)[3] <- "price_tot_fix_12"
trainHistdfEdit.Done$price_tot_var_1 <- trainHistdfEdit.Var$`1`
trainHistdfEdit.Done$price_tot_var_12 <- trainHistdfEdit.Var$`12`

sum(!(testHistdfEdit.Fix$id == testHistdfEdit.Var$id)) #0 - can just merge
testHistdfEdit.Done <- testHistdfEdit.Fix[,colnames(testHistdfEdit.Fix) %in% c("id", "1", "12")]
colnames(testHistdfEdit.Done)[2] <- "price_tot_fix_1"
colnames(testHistdfEdit.Done)[3] <- "price_tot_fix_12"
testHistdfEdit.Done$price_tot_var_1 <- testHistdfEdit.Var$`1`
testHistdfEdit.Done$price_tot_var_12 <- testHistdfEdit.Var$`12`

#what happened to prices??
var_change <- trainHistdfEdit.Done$price_tot_var_12 - trainHistdfEdit.Done$price_tot_var_1 
plot(var_change)
fix_change <- trainHistdfEdit.Done$price_tot_fix_12 - trainHistdfEdit.Done$price_tot_fix_1 
plot(fix_change)

#NA
summary(trainHistdfEdit.Done) #some NAs --> remove
trainHistdfEdit.Done <- trainHistdfEdit.Done[!is.na(trainHistdfEdit.Done$price_tot_fix_1),]
trainHistdfEdit.Done <- trainHistdfEdit.Done[!is.na(trainHistdfEdit.Done$price_tot_fix_12),]
summary(trainHistdfEdit.Done) # no NA

summary(testHistdfEdit.Done) #some NAs --> impute
testHistdfEdit.Done$price_tot_fix_12 <- ifelse(is.na(testHistdfEdit.Done$price_tot_fix_12),
                                               mean(testHistdfEdit.Done[!(is.na(testHistdfEdit.Done$price_tot_fix_12)),]$price_tot_fix_12),
                                               testHistdfEdit.Done$price_tot_fix_12)
testHistdfEdit.Done$price_tot_fix_1 <- ifelse(is.na(testHistdfEdit.Done$price_tot_fix_1),
                                               mean(testHistdfEdit.Done[!(is.na(testHistdfEdit.Done$price_tot_fix_1)),]$price_tot_fix_1),
                                               testHistdfEdit.Done$price_tot_fix_1)
testHistdfEdit.Done$price_tot_var_1 <- ifelse(is.na(testHistdfEdit.Done$price_tot_var_1),
                                              mean(testHistdfEdit.Done[!(is.na(testHistdfEdit.Done$price_tot_var_1)),]$price_tot_var_1),
                                              testHistdfEdit.Done$price_tot_var_1)
testHistdfEdit.Done$price_tot_var_12 <- ifelse(is.na(testHistdfEdit.Done$price_tot_var_12),
                                              mean(testHistdfEdit.Done[!(is.na(testHistdfEdit.Done$price_tot_var_12)),]$price_tot_var_12),
                                              testHistdfEdit.Done$price_tot_var_12)
summary(testHistdfEdit.Done)#no NA

# MERGE
df_final_temp <- join(df_clean_corr_lin, trainHistdfEdit.Done, by = "id", type = "inner", match = "all")
dim(trainHistdfEdit.Done)
dim(df_clean_corr_lin)
dim(df_final_temp)
summary(df_final_temp) #No NA

test_final_temp <- join(dfTest_clean_lin, testHistdfEdit.Done, by = "id", type = "inner", match = "all")
dim(testHistdfEdit.Done)
dim(dfTest_clean_lin)
dim(test_final_temp)
summary(df_final_temp)#No NA



dim(test_final_temp) #38
dim(df_final_temp) #39
sum(!(colnames(df_final_temp[,!(colnames(df_final_temp) %in% "churn")]) %in% colnames(test_final_temp)))
sort(colnames(test_final_temp[,!(colnames(test_final_temp) %in% "churn")]))
sort(colnames(df_final_temp))


####
# Remove extreme values (only from training)
####
sapply(df_final_temp[,!(colnames(df_final_temp) %in% c("id"))], FUN = function(x) quantile(x, seq(0.99,1,0.001),na.rm = T))
# Potential issues: net_margin, imp_cons, forecast_cons_12m forecast_cons_year forecast_meter_rent_12m cons_12m cons_last_month
#net margin
plot(df_final_temp$net_margin)
View(df_final_temp[df_final_temp$net_margin == max(df_final_temp$net_margin),]) #has churned
df_final_extreme <- df_final_temp[!(df_final_temp$net_margin == max(df_final_temp$net_margin)),]
plot(df_final_extreme$net_margin)
df_final_extreme <- df_final_extreme[!(df_final_extreme$net_margin == max(df_final_extreme$net_margin)),]

plot(df_final_extreme$imp_cons)
df_final_extreme <- df_final_extreme[!(df_final_extreme$imp_cons == min(df_final_extreme$imp_cons)),]

plot(df_final_extreme$forecast_cons_12m)
plot(df_final_extreme$forecast_cons_year)
plot(df_final_extreme$forecast_meter_rent_12m)
df_final_extreme <- df_final_extreme[!(df_final_extreme$forecast_meter_rent_12m == max(df_final_extreme$forecast_meter_rent_12m)),]

plot(df_final_extreme$cons_12m)
df_final_extreme <- df_final_extreme[!(df_final_extreme$cons_12m == max(df_final_extreme$cons_12m)),]

plot(df_final_extreme$cons_last_month)

dim(df_final_extreme)
dim(df_final_temp)

############################################
# Preprocess / Format Data
############################################
# Look here if have to prepare for xgboost (e.g. Xgboost manages only numeric vectors, 
# so must convert categorical variables to numeric one etc.):
#   - https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html
#   - https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
str(df)

# Centering and Scaling (http://topepo.github.io/caret/pre-processing.html#the-preprocess-function)
##
preProcValues <- preProcess((df[,!(colnames(df) %in% c("churn"))]), method = c("center", "scale"))
trainTransformed <- predict(preProcValues, df)
summary(trainTransformed)
# Imputation
##

# Transforming Predictors
##
#In some cases, there is a need to use principal component analysis (PCA) to transform 
#the data to a smaller sub-space where the new variable are uncorrelated with one another
# See: http://topepo.github.io/caret/pre-processing.html#the-preprocess-function


############################################
# Data Splitting
############################################
# See http://topepo.github.io/caret/data-splitting.html

#UST SPLIT THE TRAINING SET INTO A TRAIN AND TEST (as we dont have churn var in test set )

# Simple Splitting Based on the Outcome
set.seed(3456)
trainIndex <- createDataPartition(df_final_extreme$churn, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dfTrain <- df_final_extreme[ trainIndex,]
dfTest  <- df_final_extreme[-trainIndex,]
dim(dfTrain)
dim(dfTest)
table(dfTrain$churn)
table(dfTest$churn)

trainTransformedIndex <- createDataPartition(trainTransformed$churn, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dfTrainTransformed <- trainTransformed[ trainIndex,]
dfTestTransformed  <- trainTransformed[-trainIndex,]
dim(dfTrainTransformed)
dim(dfTestTransformed)
table(dfTrainTransformed$churn)
table(dfTestTransformed$churn)




# Splitting Based on the Predictors
# - We may want to create a sub-sample from B that is diverse when compared to A.

# Data Splitting for Time Series

# Simple Splitting with Important Groups

################################################################################################################################
# OUTPUT
################################################################################################################################

# TRAIN
###

# Setting path and current file
path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/2dataPreparation/"
filename_train <- "prepared_data_train"
filename_test <- "prepared_data_test"

filename_full_train <- paste0(path, filename_train, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")
filename_full_test <- paste0(path, filename_test, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")

write.csv(dfTrain,file = filename_full_train, row.names=FALSE )
write.csv(dfTest,file = filename_full_test, row.names=FALSE )

#Transformed
filename_trainTr <- "prepared_data_train_trans"
filename_testTr <- "prepared_data_test_trans"

filename_full_train_trans <- paste0(path, filename_trainTr, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")
filename_full_test_trans <- paste0(path, filename_testTr, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")

write.csv(dfTrainTransformed,file = filename_full_train_trans, row.names=FALSE )
write.csv(dfTestTransformed,file = filename_full_test_trans, row.names=FALSE )

# TEST
###

# Setting path and current file
path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/2dataPreparation/"
filename_testset <- "TESTSET_prepared"

filename_full_testset <- paste0(path, filename_testset, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")

write.csv(test_final_temp,file = filename_full_testset, row.names=FALSE )
