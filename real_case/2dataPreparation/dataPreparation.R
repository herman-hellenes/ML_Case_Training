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

################################################################################################################################
# INPUT
################################################################################################################################
input_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/real_case/1dataUnderstanding/prepped_data_2018-09-01_111925_.csv"

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
id.col <- df$id
df$id <- NULL
# Check if unique ID - if not do something!
sum(duplicated(id.col))

df <- as.data.frame(lapply(df, as.numeric))
str(df)

############################################
# Data cleaning 
############################################

# Check if unique ID - if not do something!
#unique(df$ID)

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
# nzv <- nearZeroVar(df.validbanks.segment, saveMetrics= TRUE)
# nzv[nzv$nzv,][,]
# 
# # No-variance is deleted (only one value in the whole set)
# df.validbanks.segment[, rownames(nzv[nzv$zeroVar,][,])] <- NULL 
# rownames(nzv[nzv$zeroVar,][,])
# 
# # Inspection of near-zero variance
# for(i in 1:dim(nzv[nzv$nzv,][,])[1]){
#   if(dim(table(df.validbanks.segment[,rownames(nzv[nzv$nzv,][,])[i]]))[1] < 15){
#     print(rownames(nzv[nzv$nzv,][,])[i])
#     print(table(df.validbanks.segment[,rownames(nzv[nzv$nzv,][,])[i]], df.validbanks.segment$target_var_save))
#   }
# }

###########
# Remove extreme values 
###########


###########
# Check in on Volume
###########


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
trainIndex <- createDataPartition(df$churn, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]
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


