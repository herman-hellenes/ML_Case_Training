# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "01/08/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: This script does investigate the prepared data, in order to understand how it can drive business value:
#      1: We want some descriptives on the data set, to see what "is there" 
#      2: Make some hypothesis of driving variables
#      3: See some connections with customer data (e.g. age distribution etc)

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
raw_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/2dataPreparation/"

#Read data
traindf <- read.csv(paste0(raw_path, "prepared_data_train2018-09-01_181353_.csv"),
                     sep = ",", dec=".", stringsAsFactors = F)
traintestdf <- read.csv(paste0(raw_path, "prepared_data_test2018-09-01_181353_.csv"),
                    sep = ",", dec=".", stringsAsFactors = F)
testdf <- read.csv(paste0(raw_path, "TESTSET_prepared2018-09-01_181518_.csv"),
                                         sep = ",", dec=".", stringsAsFactors = F)


################################################################################################################################
# Data understanding
################################################################################################################################

#Quick QA if reading went ok, and give us a high level understanding of the data
dim(traindf) # 12477    39
str(traindf) 

dim(traintestdf) #3119   39
str(traintestdf) 

dim(testdf) # 4024   38
str(testdf) 

# Check if unique IDs!
sum(duplicated(testdf$id)) #0
sum(duplicated(traintestdf$id)) #0
sum(duplicated(traindf$id)) #0


# Look at skewness on target var churn
table(traintestdf$churn) 
table(traindf$churn) 

# Merge traindf and traintestdf
dftrain <- rbind(traindf,traintestdf)
dim(dftrain) #15596
sum(duplicated(dftrain$id)) #0


################################################################################################################################
# Looking at CHURN
################################################################################################################################
table(dftrain$churn)


################################################################################################################################
# Looking at how many new customers, and how profitable
################################################################################################################################
plot(testdf$cons_12m )
plot(dftrain[dftrain$churn == 1,]$cons_12m )
summary(dftrain[dftrain$churn == 1,]$cons_12m )
plot(dftrain[dftrain$churn == 0,]$cons_12m )
summary(dftrain[dftrain$churn == 0,]$cons_12m )
summary(dftrain[dftrain$churn == 0,]$margin_net_pow_ele )
summary(dftrain[dftrain$churn == 1,]$margin_net_pow_ele )

summary(dftrain[dftrain$churn == 0,]$net_margin )
summary(dftrain[dftrain$churn == 1,]$net_margin )

library(AppliedPredictiveModeling)

featurePlot(x = dftrain[, c("cons_12m","margin_net_pow_ele", "net_margin", "num_years_antig")], 
            y = as.factor(dftrain$churn), 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

featurePlot(x = dftrain[, c("activity_new.NA","margin_net_pow_ele", "net_margin", "num_years_antig")], 
            y = as.factor(dftrain$churn), 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

table(dftrain$activity_new.NA, dftrain$churn)
table(dftrain$activity_new.apdekpcbwosbxepsfxclislboipuxpop, dftrain$churn)
table(dftrain$activity_new.kkklcdamwfafdcfwofuscwfwadblfmce, dftrain$churn)

table(dftrain$channel_sales, dftrain$churn)
table(dftrain$channel_salesewpakwlliwisiwduibdlfmalxowmwpci, dftrain$churn)
table(dftrain$channel_saleslmkebamcaaclubfxadlmueccxoimlema, dftrain$churn)


plot(testdf$margin_net_pow_ele)
plot(dftrain$margin_net_pow_ele)

plot(dftrain$margin_net_pow_ele*dftrain$cons_12m)

table(dftrain$has_gasf)
table(testdf$num_years_antig)

ggplot(dftrain, aes(x=num_years_antig, y=margin_net_pow_ele, color=churn)) + 
  geom_point() 

ggplot(dftrain, aes(x=date_modif_prod, y=num_years_antig, color=churn)) + 
  geom_point() 

ggplot(dftrain, aes(x=date_modif_prod, y=cons_12m, color=churn)) + 
  geom_point() 

ggplot(dftrain, aes(x=pow_max, y=cons_12m, color=as.factor(churn))) + 
  geom_point(alpha = 1/2) + xlim(0,350)

ggplot(dftrain, aes(x=date_activ, y=net_margin, color=as.factor(churn))) + 
  geom_point(alpha = 1/2) + ylim(-1000,4000)

ggplot(dftrain, aes(x=date_modif_prod, y=imp_cons, color=churn)) + 
  geom_point() 

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


