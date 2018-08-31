# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "31/08/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: This script does investigate the data, in order to understand how it can drive business value:
#      1: We want some descriptives on the data set, to see what "is there" 
#      2: Make some hypothesis of driving variables
#      3: See some connections with customer data (e.g. age distribution etc)
#      4: Make up an action plan on data quality and prep

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
raw_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/From Cecilia/Machine Learning Case_PowerCo/ml_case_data/"

#Read data
ml_case_test_output_template <- read.csv(paste0(raw_path, "ml_case_test_output_template.csv"),
                     sep = ",", dec=".", stringsAsFactors = F)

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
# Data understanding
################################################################################################################################
testdf <- ml_case_test_data
traindf <- ml_case_training_data
testHistdf <- ml_case_test_hist_data
trainHistdf <- ml_case_training_hist_data
trainTarget <- ml_case_training_output

############
# High level - what are the data sets?
############

#Quick QA if reading went ok, and give us a high level understanding of the data
dim(traindf) #16096    32
head(traindf)
str(traindf) 

dim(testdf) #4024   32
head(testdf)
str(testdf) 

dim(trainHistdf) # 193002      8
head(trainHistdf)
str(trainHistdf) 

dim(testHistdf) # 48236     8
head(testHistdf)
str(testHistdf) 

dim(trainTarget) # 16096     2 (as many rows as in traindf)
head(trainTarget)
str(trainTarget) 

# Compare colnames test and training
colnames(testHistdf) == colnames(trainHistdf) #all true!
colnames(testdf) == colnames(traindf) #all true!

# Comments:
# _ trainTarget only holds churn
# - trainHistdf has the price_ variables, historical. Assume since it is historical AND 
#   it contains far more rows than traindf, that it is many periods per consumer. Thus we 
#   have a time series.. This would need some work and transformation in order to merge in the "master set", 
#   where we have unique customers. Assume this set is traindf
# - traindf : the master set
# - ml_case_test_output_template : what I will hand in
# - test sets: assume equivalent to the training

# Check if unique IDs!
sum(duplicated(trainTarget$id)) #0
sum(duplicated(testHistdf$id)) #44212, ok since time seriees
sum(duplicated(trainHistdf$id)) #176906, ok since time seriees
sum(duplicated(testdf$id)) #0
sum(duplicated(traindf$id)) #0
# Comment - looks great!

# Look at skewness on target var churn
table(trainTarget$churn) # 14501  1595 -> 0.09909294 (10%) has churned. So skewed but not very extreme
# PowerCo experiencing above market customer churn (from case text) and 
# it is around 10% of customer has churned from January to March 2016.

#########
# # Univariate Analysis: explore variables one by one - Distribution and Extreme values
#########
summary(traindf) # a lot of non-numeric vars -> must convert/transform (dummy). 
# On the numeric it is quite some NA in some forecast vars, else not many. Might be more in the chars of course. 
# -> thus do the NA analysis (make dummies or some other action

summary(testdf) # same as for traindf

summary(trainHistdf) # looks okay - 1359 NAs (probably same rows) across the price-columns
summary(testHistdf) # looks okay - 302 NAs (probably same rows) across the price-columns

summary(trainTarget) # looks okay - no NAs

# Checking categorical vars:
#activity_new
activity_new.info <- data.frame(table(traindf$activity_new), stringsAsFactors = FALSE)
activity_new.info <- activity_new.info[order(activity_new.info$Freq, decreasing = T),]
acCodes <- c("NA", "apdekpcbwosbxepsfxclislboipuxpop", "kkklcdamwfafdcfwofuscwfwadblfmce", "other")
Freq <- c(9545, 1577,422, 16096-9545-1577-422)
activity.groups <- data.frame(acCodes,Freq )
pie(activity.groups$Freq, activity.groups$acCodes)

#campaign_disc_ele
table(is.na(traindf$campaign_disc_ele)) # ALL NA -> should delete.. 
table(is.na(testdf$campaign_disc_ele)) # ALL NA -> should delete.. 

#channel_sales
table(traindf$channel_sales)
channel_sales.info <- data.frame(table(traindf$channel_sales), stringsAsFactors = FALSE)
pie(channel_sales.info$Freq, channel_sales.info$Var1)

#has_gas 
table(traindf$has_gas) #f=13132  t=2964 --> only 0.1841451 (18.4%) has gas
 
#origin_up: code of the electricity campaign the customer first subscribed to
table(traindf$origin_up) 
origin_up.info <- data.frame(table(traindf$origin_up), stringsAsFactors = FALSE)
pie(origin_up.info$Freq, origin_up.info$Var1)


# Get an idea of which variables that can explain TARGET
# Merging traindf and trainTarget
library(plyr)
traindfTarget <- join(traindf, trainTarget, by = "id", type = "left", match = "all")
dim(traindfTarget)
dim(traindf)
head(traindfTarget)
sum(traindf$id != trainTarget$id) #already sorted - so not too much that can go wrong in join

table(traindfTarget$churn, traindfTarget$has_gas) #pretty same churn ratio

# churn.vs.activity
churn.vs.activity <- data.frame(table(traindfTarget$churn, traindfTarget$activity_new) )
churn.vs.activity <- churn.vs.activity[order(churn.vs.activity$Freq, decreasing = T),]
head(churn.vs.activity,20)
churn.vs.activity[churn.vs.activity$Var2 == "",] # 10% churn
churn.vs.activity[churn.vs.activity$Var2 == "apdekpcbwosbxepsfxclislboipuxpop",] # 0.05897273 = 6% churn 
churn.vs.activity[churn.vs.activity$Var2 == "kkklcdamwfafdcfwofuscwfwadblfmce",] # 9% churn

#channel_sales
churn.channel_sales <- data.frame(table(traindfTarget$churn, traindfTarget$channel_sales) )
churn.channel_sales <- churn.channel_sales[order(churn.channel_sales$Freq, decreasing = T),]
# NA: ca 10% churn
# foosdfpfkusacimwkcsosbicdxkicaua: 0.1249831
# lmkebamcaaclubfxadlmueccxoimlema: 0.05595755
# usilxuppasemubllopkaafesmlibmsdf: 0.1038781
# ewpakwlliwisiwduibdlfmalxowmwpci: 0.08488613
# others : very small..


# Looking at profitability
summary(traindfTarget$net_margin)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-4149.00    51.97   119.70   218.00   275.80 24570.00       15 

hist(traindfTarget$net_margin)

#Looking at usage
summary(traindfTarget$cons_12m)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -125300     5906    15330   194800    50220 16100000
summary(traindfTarget$pow_max)

summary(traindfTarget$nb_prod_act)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.348   1.000  32.000 

summary(traindfTarget$margin_net_pow_ele)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -615.70   11.95   20.97   21.46   29.64  374.60      13 

# mange nye kunder?
table(traindfTarget$num_years_antig) #nesten ingen på 1-2 : (1 -> 1, 2 -> 11) altså 12 new the last 2 years


# Looking at distributions (numerical)
##
categorical.cols <- c("origin_up","has_gas", "channel_sales", "campaign_disc_ele", "activity_new")
date.cols <- c("date_renewal", "date_modif_prod", "date_first_activ" , "date_end", "date_activ")  


# Extreme values?
traindfTarget$churn <- as.numeric(traindfTarget$churn)

sapply(traindfTarget[,!(colnames(traindfTarget) %in% c(categorical.cols,date.cols, "id"))], function(x) sum(is.na(x)))
sapply(traindfTarget[,!(colnames(traindfTarget) %in% c(categorical.cols,date.cols, "id"))], FUN = function(x) quantile(x, seq(0,1,0.1), na.rm = T)) 
sapply(traindfTarget[,!(colnames(traindfTarget) %in% c(categorical.cols,date.cols,"id"))], FUN = function(x) quantile(x, seq(0.99,1,0.001),na.rm = T))

# Extreme value plot (can plot the distribution where it looks extreme): Box-plot, Histogram, Scatter Plot
# First play a bit around using table() - then plot if interesting
plot(traindfTarget$net_margin) #extreme val
plot(traindfTarget$imp_cons)
plot(traindfTarget$forecast_meter_rent_12m) ##extreme val
plot(traindfTarget$cons_12m) ##extreme val

#######################################
# Multivariate
#######################################
temp.traindfTarget <- traindfTarget
temp.traindfTarget <- temp.traindfTarget[!is.na(temp.traindfTarget$net_margin),]
# CHURN vs most interesting vars
library(ggplot2)
traindfTarget.plot <- temp.traindfTarget
traindfTarget.plot$churn <- as.factor(temp.traindfTarget$churn)

ggplot(traindfTarget.plot, aes(x=net_margin, y=cons_12m, color=churn)) + 
  geom_point() + xlim(-1000, 5000) + ylim(-10, 7500000)

ggplot(traindfTarget.plot, aes(x=net_margin, y=cons_12m, color=churn)) + 
  geom_point() + xlim(-1000, 5000) + ylim(-10, 7500000)


# Churn percentage per quantile margin
sapply(temp.traindfTarget[,(colnames(temp.traindfTarget) %in% c("net_margin","churn"))], FUN = function(x) quantile(x, seq(0.99,1,0.001),na.rm = T))
library(dplyr)
temp.traindfTarget$ntile.margin <- ntile(temp.traindfTarget$net_margin, 10)
View(temp.traindfTarget[,c("net_margin","ntile.margin")])
totChurn <- c(1:10)*0
ntile.margin <- c(1:10)
totNoChurn <- c(1:10)*0
churn.prc <- data.frame(ntile.margin, totChurn, totNoChurn)

for (i in 1:10){
  churn.prc[i,"totChurn"] <- sum(temp.traindfTarget$churn == 1 & temp.traindfTarget$ntile.margin == i )
  churn.prc[i,"totNoChurn"] <- sum(temp.traindfTarget$churn == 0 & temp.traindfTarget$ntile.margin == i )
}
churn.prc$churn.prc <- churn.prc$totChurn/(churn.prc$totChurn + churn.prc$totNoChurn)
ggplot(data=churn.prc, aes(x=ntile.margin, y=churn.prc, color = as.factor(ntile.margin) )) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(churn.prc,3)), vjust=-0.3, size=3.5)+
  theme_minimal()

# Churn percentage per quantile imp_cons (current paid consumption)
temp.traindfTarget$ntile.imp_cons <- ntile(temp.traindfTarget$imp_cons, 10)
churn.prc.imp_cons <- churn.prc
colnames(churn.prc.imp_cons)[1] <- "ntile.imp_cons"
for (i in 1:10){
  churn.prc.imp_cons[i,"totChurn"] <- sum(temp.traindfTarget$churn == 1 & temp.traindfTarget$ntile.imp_cons == i )
  churn.prc.imp_cons[i,"totNoChurn"] <- sum(temp.traindfTarget$churn == 0 & temp.traindfTarget$ntile.imp_cons == i )
}

# Churn percentage per quantile forecast_cons_year 

temp.traindfTarget$ntile.forecast_cons_year <- ntile(temp.traindfTarget$forecast_cons_year, 10)
churn.prc.forecast_cons_year <- churn.prc
colnames(churn.prc.forecast_cons_year)[1] <- "ntile.forecast_cons_year"
for (i in 1:10){
  churn.prc.forecast_cons_year[i,"totChurn"] <- sum(temp.traindfTarget$churn == 1 & temp.traindfTarget$ntile.forecast_cons_year == i )
  churn.prc.forecast_cons_year[i,"totNoChurn"] <- sum(temp.traindfTarget$churn == 0 & temp.traindfTarget$ntile.forecast_cons_year == i )
}

# Churn percentage per quantile cons_last_month 
temp.traindfTarget$ntile.cons_last_month <- ntile(temp.traindfTarget$cons_last_month, 10)
churn.prc.cons_last_month <- churn.prc
colnames(churn.prc.cons_last_month)[1] <- "ntile.cons_last_month "
for (i in 1:10){
  churn.prc.cons_last_month[i,"totChurn"] <- sum(temp.traindfTarget$churn == 1 & temp.traindfTarget$ntile.cons_last_month == i )
  churn.prc.cons_last_month[i,"totNoChurn"] <- sum(temp.traindfTarget$churn == 0 & temp.traindfTarget$ntile.cons_last_month == i )
}

# Churn percentage per quantile cons_last_month 
temp.traindfTarget$ntile.num_years_antig <- ntile(temp.traindfTarget$num_years_antig, 10)
churn.prc.num_years_antig <- churn.prc
colnames(churn.prc.num_years_antig)[1] <- "ntile.num_years_antig "
for (i in 1:10){
  churn.prc.num_years_antig[i,"totChurn"] <- sum(temp.traindfTarget$churn == 1 & temp.traindfTarget$ntile.num_years_antig == i )
  churn.prc.num_years_antig[i,"totNoChurn"] <- sum(temp.traindfTarget$churn == 0 & temp.traindfTarget$ntile.num_years_antig == i )
}


margin_net_pow_ele
num_years_antig
time.since.active

# Profitability vs most interesting vars


# Visualization (see http://topepo.github.io/caret/visualizations.html for understanding below)
###

# Scatterplot Matrix
library(AppliedPredictiveModeling)
library(caret)
transparentTheme(trans = .4)

df_for_plot <- traindfTarget[, c("net_margin","cons_12m","num_years_antig")] #must do some formatting in order to use featurePlot
df_for_plot$TARGET <- traindfTarget$churn
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
df <- traindfTarget 
summary(df) # here lists NA's per column, max, min, mean, class (if character it says so).. Most important print out!

# Check if unique IDs!
sum(duplicated(df$id)) #0 ok

# Look into spesific variables
unique(df$churn)  # 0 1
table(df$churn) #14501  1595 

#################################
# Missing values
#################################

#### NUMERICS ######
col.na.overview <- sapply(df, function(x) sum(is.na(x)))
sort(col.na.overview)

# Delete campaign_disc_ele (only NA)
df$campaign_disc_ele <- NULL

# Check out the ones having very low NA
df[is.na(df$margin_gross_pow_ele),] #looks ok to delete
df <- df[!(is.na(df$margin_gross_pow_ele)),]
col.na.overview.new <- sapply(df, function(x) sum(is.na(x)))
sort(col.na.overview.new)
df <- df[!(is.na(df$net_margin)),]

df[is.na(df$forecast_price_energy_p2),] #looks ok to delete - mixed churn results so not a great predictor
df <- df[!(is.na(df$forecast_price_energy_p2)),]
col.na.overview.new2 <- sapply(df, function(x) sum(is.na(x)))
sort(col.na.overview.new2)



#Rows
#sapply( df, function(f) nrow(complete.cases(df))) 
summary(complete.cases(df))
# Mode   FALSE    TRUE    NA's 
# logical   12494    3462       0 

# 12494 is the NAs in 4 columns - and they are NA at the same time, ok!
plot(df$forecast_cons)
# Decision: for now we skip those columns: make dummy!
df$forecast_consNA <- ifelse(is.na(df$forecast_cons) , 1,0)
df$forecast_cons <- NULL

df$forecast_bill_12mNA <- ifelse(is.na(df$forecast_bill_12m) , 1,0)
df$forecast_bill_12m <- NULL

df$forecast_base_bill_eleNA <- ifelse(is.na(df$forecast_base_bill_ele) , 1,0)
df$forecast_base_bill_ele <- NULL

df$forecast_base_bill_yearNA <- ifelse(is.na(df$forecast_base_bill_year) , 1,0)
df$forecast_base_bill_year <- NULL
dim(df)


# Categorical Missing values 
##

# Dates
## Convert dates
# Check nas
sum(df$date_end == "") #2
df.char.clean <- df

df.char.clean <- df.char.clean[!((df.char.clean$date_end) == ""),]

sum(df$date_activ == "") #0

sum(df$date_modif_prod == "") # 156
df.char.clean <- df.char.clean[!((df.char.clean$date_modif_prod) == ""),]

sum(df$date_renewal == "") # 40
df.char.clean <- df.char.clean[!((df.char.clean$date_renewal) == ""),]
dim(df.char.clean)
dim(df)

sum(df$date_first_activ == "") # 12494
table(df$date_first_activ=="")
df.char.clean$date_first_activNA <- ifelse(df.char.clean$date_first_activ=="",1,0)
df.char.clean$date_first_activ <- NULL

# date_activ: date of activation of the contract
df.char.clean$date_activ <- as.POSIXct(df.char.clean$date_activ)
min(df.char.clean$date_activ)
max(df.char.clean$date_activ)
df.char.clean$date_activ <- as.numeric(df.char.clean$date_activ)
df.char.clean$date_activ <- df.char.clean$date_activ - min(df.char.clean$date_activ)

# date_end: 
df.char.clean$date_end <- as.POSIXct(df.char.clean$date_end)
min(df.char.clean$date_end)
max(df.char.clean$date_end)
df.char.clean$date_end <- as.numeric(df.char.clean$date_end)
df.char.clean$date_end <- df.char.clean$date_end - min(df.char.clean$date_end)

# date_modif_prod: 
df.char.clean$date_modif_prod <- as.POSIXct(df.char.clean$date_modif_prod)
min(df.char.clean$date_modif_prod)
max(df.char.clean$date_modif_prod)
df.char.clean$date_modif_prod <- as.numeric(df.char.clean$date_modif_prod)
df.char.clean$date_modif_prod <- df.char.clean$date_modif_prod - min(df.char.clean$date_modif_prod)

# date_renewal: 
df.char.clean$date_renewal <- as.POSIXct(df.char.clean$date_renewal)
min(df.char.clean$date_renewal)
max(df.char.clean$date_renewal)
df.char.clean$date_renewal <- as.numeric(df.char.clean$date_renewal)
df.char.clean$date_renewal <- df.char.clean$date_renewal - min(df.char.clean$date_renewal)


str(df.char.clean)



# Make dummies! 
dfnum <- df.char.clean
str(dfnum)

dfnum$activity_new.NA <- ifelse((df.char.clean$activity_new) =="" , 1,0)
dfnum$activity_new.apdekpcbwosbxepsfxclislboipuxpop <- ifelse((df.char.clean$activity_new) =="apdekpcbwosbxepsfxclislboipuxpop" , 1,0)
dfnum$activity_new.kkklcdamwfafdcfwofuscwfwadblfmce <- ifelse((df.char.clean$activity_new) =="kkklcdamwfafdcfwofuscwfwadblfmce" , 1,0)
dfnum$activity_new <- NULL

dfnum <- dfnum[,!(colnames(dfnum) %in% c("id"))]

dmy <- dummyVars(" ~ .", data = dfnum) #from caret -> making dummy vars
dfnum <- data.frame(predict(dmy, newdata = dfnum))
dim(dfnum)
str(dfnum)

#Grand check
sum(is.na(dfnum))
summary(dfnum)

df_clean <- dfnum
dim(df.char.clean)
dim(df_clean) #15761    46
df_clean$id <- df.char.clean$id

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

table(df_clean$channel_salessddiedcslfslkckwlfkdpoeeailfpeds) # remove
df_clean$channel_salessddiedcslfslkckwlfkdpoeeailfpeds <- NULL

table(df_clean$forecast_discount_energy) # remove
df_clean$forecast_discount_energy <- NULL

table(df_clean$origin_up) # remove
df_clean$origin_up <- NULL

table(df_clean$origin_upewxeelcelemmiwuafmddpobolfuxioce) # remove
df_clean$origin_upewxeelcelemmiwuafmddpobolfuxioce <- NULL

table(df_clean$origin_upusapbepcfoloekilkwsdiboslwaxobdp) # remove
df_clean$origin_upusapbepcfoloekilkwsdiboslwaxobdp <- NULL

table(df_clean$activity_new.kkklcdamwfafdcfwofuscwfwadblfmce) # hold

  


#################################
# Identifying Correlated Predictors (http://topepo.github.io/caret/pre-processing.html#corr)
#################################
df_clean_corr <- (df_clean)
df_clean_corr$id <- NULL 
descrCor <-  cor(df_clean_corr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75) #  Cant handle missing values! This function searches through a 
# correlation matrix and returns a vector of integers 
# corresponding to columns to remove to reduce pair-wise correlations.

#Removing
df_clean_corr_move <- df_clean_corr[,-highlyCorDescr]
dim(df_clean_corr_move)
dim(df_clean_corr)

colnames(df_clean_corr[,!(colnames(df_clean_corr) %in% colnames(df_clean_corr_move))])
descrCor_after <-  cor(df_clean_corr_move)
summary(descrCor[upper.tri(descrCor_after)])
summary(descrCor[upper.tri(descrCor)])
#Was this an improvement?? No - save for later

# Take out the obious
df_clean_corr$has_gast <- NULL #as this is captured

# Plot to see if correlation make sense
# See https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/#one


#################################
# Linear Dependencies (http://topepo.github.io/caret/pre-processing.html#corr)
#################################
comboInfo <- findLinearCombos(df_clean_corr) #dont work with NA
df_clean_corr <- data.frame(df_clean_corr)
head(df_clean_corr[,comboInfo$linearCombos[[1]]])
head(df_clean_corr[,comboInfo$remove])
colnames(df_clean_corr)[comboInfo$remove]
df_clean_corr_lin <- df_clean_corr[,-comboInfo$remove]
dim(df_clean_corr)
dim(df_clean_corr_lin)

descrCor_after2 <-  cor(df_clean_corr_lin)
summary(descrCor[upper.tri(descrCor_after2)])

df_clean_corr_lin$id <- df_clean$id


#################################
# Historical data:
#################################
str(trainHistdf)
unique(trainHistdf$price_date)
trainHistdfEdit <- trainHistdf
trainHistdfEdit$price_date_num <- as.POSIXct(trainHistdfEdit$price_date)
trainHistdfEdit$price_date_num <- month(trainHistdfEdit$price_date_num)
# the fixed(p1-p3) look very correlated. save for var. thus we sum them
trainHistdfEdit$price_tot_var <- trainHistdfEdit$price_p1_var + trainHistdfEdit$price_p2_var + trainHistdfEdit$price_p3_var
trainHistdfEdit$price_tot_fix <- trainHistdfEdit$price_p1_fix + trainHistdfEdit$price_p2_fix + trainHistdfEdit$price_p3_fix

#NA
sum(is.na(trainHistdfEdit$price_p3_fix))

#cast
library(reshape2)
trainHistdfEdit.Var <- dcast(trainHistdfEdit, id  ~ price_date_num , value.var = "price_tot_var" )
sum(duplicated(trainHistdfEdit.Var$id)) #0
length(unique(trainHistdfEdit.Var$id)) #16096

trainHistdfEdit.Fix <- dcast(trainHistdfEdit, id  ~ price_date_num , value.var = "price_tot_fix" )
sum(duplicated(trainHistdfEdit.Fix$id)) #0
length(unique(trainHistdfEdit.Fix$id)) #16096

sum(!(trainHistdfEdit.Fix$id == trainHistdfEdit.Var$id)) #0 - can just merge
trainHistdfEdit.Done <- trainHistdfEdit.Fix[,colnames(trainHistdfEdit.Fix) %in% c("id", "1", "12")]
colnames(trainHistdfEdit.Done)[2] <- "price_tot_fix_1"
colnames(trainHistdfEdit.Done)[3] <- "price_tot_fix_12"
trainHistdfEdit.Done$price_tot_var_1 <- trainHistdfEdit.Var$`1`
trainHistdfEdit.Done$price_tot_var_12 <- trainHistdfEdit.Var$`12`

#what happened to prices??
var_change <- trainHistdfEdit.Done$price_tot_var_12 - trainHistdfEdit.Done$price_tot_var_1 
plot(var_change)
fix_change <- trainHistdfEdit.Done$price_tot_fix_12 - trainHistdfEdit.Done$price_tot_fix_1 
plot(fix_change)

summary(trainHistdfEdit.Done) #some NAs --> remove

trainHistdfEdit.Done <- trainHistdfEdit.Done[!is.na(trainHistdfEdit.Done$price_tot_fix_1),]
trainHistdfEdit.Done <- trainHistdfEdit.Done[!is.na(trainHistdfEdit.Done$price_tot_fix_12),]
trainHistdfEdit.Done <- trainHistdfEdit.Done[!is.na(trainHistdfEdit.Done$price_tot_fix_1),]

CONT HERE -> merge 
df_final_temp <- join(trainHistdfEdit.Done, df_clean_corr_lin, by = "id", type = "left", match = "all")

dim(trainHistdfEdit.Done)

left 

####
# Remove extreme values
####


####################################################################################################################################
# Data Exploration and Data Quality report  
####################################################################################################################################
# Here make a wrap up of the data: both the quality and what the data tell us already now
df_final <- df_final_temp 
View(df_final)

#Temp save
write_path_temp <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/1dataUnderstanding/temp.csv"
write.csv(df_final,file = write_path_temp, row.names=FALSE )





PARETO- Gjør 8020 på kundene - hvilke kunder tjener vi penger på? Fokus på de!!!
  net_margin total net margin -> se eirik sin analyse slide 2 --> The Apdekpcbwosbxepsfxclislboipuxpop activity group shows high consumption and net margins

# volum
eirik: Lite nye kunder, bare 1 som kom inn I fjor, og 11 som kom inn for 2 aar siden.

#Interessante forskjeller I kategorivariabler: mtp CHURN

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



