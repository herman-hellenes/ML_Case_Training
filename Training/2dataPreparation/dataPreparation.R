# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "22/08/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: This script does ..., in order to:
#      1: ...
#      2: ...

# Questions: 
#     - ...

# Todo
#     - ...

# Output:
#     - ...


library(data.table)
library(caret)

################################################################################################################################
# INPUT
################################################################################################################################
input_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/1dataUnderstanding/df_pre_prepped.csv"

#Read data
input_data <- read.csv(input_path,
                     sep = ",", dec=".", stringsAsFactors = F)
df <- input_data
df <- data.frame(df)
#Quick QA if reading went ok (make sure its the same as in Data Understanding)
dim(df)
head(df)
View(df)
str(df)


################################################################################################################################
# Data Preparation 
################################################################################################################################
df <- as.data.frame(lapply(df, as.numeric))
str(df)
############################################
# Data cleaning 
############################################

# Check if unique ID - if not do something!
unique(df$ID)

###########
# Treat missing values
###########
# table(is.na(train))
# sapply(train, function(x) sum(is.na(x))/length(x))*100
#set all missing value as "Missing" 
# train[is.na(train)] <- "Missing" 

###########
# Remove intuitively correlated and irrelevant variables 
###########

###########
# Remove 0 variance columns
###########

# Looking for variables with none or little variance
nzv <- nearZeroVar(df.validbanks.segment, saveMetrics= TRUE)
nzv[nzv$nzv,][,]

# No-variance is deleted (only one value in the whole set)
df.validbanks.segment[, rownames(nzv[nzv$zeroVar,][,])] <- NULL 
rownames(nzv[nzv$zeroVar,][,])

# Inspection of near-zero variance
for(i in 1:dim(nzv[nzv$nzv,][,])[1]){
  if(dim(table(df.validbanks.segment[,rownames(nzv[nzv$nzv,][,])[i]]))[1] < 15){
    print(rownames(nzv[nzv$nzv,][,])[i])
    print(table(df.validbanks.segment[,rownames(nzv[nzv$nzv,][,])[i]], df.validbanks.segment$target_var_save))
  }
}

###########
# Remove extreme values 
###########


###########
# Check in on Volume
###########
dim(df.loaded) #874536
dim(df.validbanks) #157010
dim(df.validbanks.segment.store) # 140213 take away Fødselsnummer
dim(df.validbanks.segment) #114119 (takes away "Fødselsnummer" and "Alder")
table(df.validbanks.segment$target_var_save) #104049  10070 


# Ok, dim above is correct 


############################################
# Construct Data
############################################

##################
# Make new features from existing
##################
# Can make for example discrete age:
# df[,AgeDiscret := as.factor(round(Age/10,0))]
# df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))]
# Note that these are very correlated. However when using decision threes, this is not that big issue

# See https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/#one

##################
# Encoding categorical features (transform the categorical data to dummy variables etc.)
##################
# Example:
# sparse_matrix <- sparse.model.matrix(TargetVar ~ ., data = df)[,-1] (see https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html#preparation-of-the-dataset)
# head(sparse_matrix)


############################################
# Integrate (merge) Data
############################################
# Pivot
# require(reshape2)
# pivot <- dcast(df, Country + Ans.y ~ Ans.x, function(x) 1, fill = 0)



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
preProcValues <- preProcess((df[,!(colnames(df) %in% c("SK_ID_CURR", "TARGET"))]), method = c("center", "scale"))
trainTransformed <- predict(preProcValues, df)

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

# Simple Splitting Based on the Outcome
set.seed(3456)
trainIndex <- createDataPartition(df$TARGET, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]
dim(dfTrain)
dim(dfTest)
table(dfTrain$TARGET)
table(dfTest$TARGET)

# Splitting Based on the Predictors
# - We may want to create a sub-sample from B that is diverse when compared to A.

# Data Splitting for Time Series

# Simple Splitting with Important Groups

################################################################################################################################
# OUTPUT
################################################################################################################################
# Setting path and current file
path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/2dataPreparation/"
filename_train <- "prepared_data_train"
filename_test <- "prepared_data_test"

# setwd(path)
# getwd()
# #system('pwd -P')
# 
# # Loading versions
# library(git2r)
# current.commit <- system("git rev-parse HEAD",  intern = TRUE)
# current.branch <- system(paste("git branch --contains ", current.commit),  intern = TRUE)
# #current.r.version <- system("r --version", , intern = TRUE)[1]
# #current.git.version <- system("git --version", intern = TRUE)

# filename_full <- paste0(path, filename, current.commit, current.branch, format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
filename_full_train <- paste0(path, filename_train, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")
filename_full_test <- paste0(path, filename_test, format(Sys.time(), "%Y-%m-%d_%H%M%S_"),".csv")

write.csv(dfTrain,file = filename_full_train, row.names=FALSE )
write.csv(dfTest,file = filename_full_test, row.names=FALSE )

