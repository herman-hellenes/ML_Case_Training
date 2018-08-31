# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "22/08/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: This script carry out the predictive analysis, in order to:
#      1: Score clients, as who will be most likely to churn
#      2: ...

# Questions: 
#     - ...

# Todo
#     - ...

# Output:
#     - Trained model and its params. This will be used on the test set in the evaluation script


library(data.table)
library(caret)
library(xgboost)


################################################################################################################################
# INPUT
################################################################################################################################
pathTrain <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/2dataPreparation/prepared_data_train2018-08-30_102224_.csv"
pathTest <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/2dataPreparation/prepared_data_test2018-08-30_102225_.csv"

#Read data
input_dataTrain <- read.csv(pathTrain,
                     sep = ",", dec=".", stringsAsFactors = F)
df <- input_dataTrain

input_dataTest <- read.csv(pathTest,
                            sep = ",", dec=".", stringsAsFactors = F)
dfTest <- input_dataTest


###################
# Quick formatting
###################
#Quick QA if reading went ok
dim(df)
head(df)
View(df)
str(df)

# Quick formatting
##
df <- as.data.frame(lapply(df, as.numeric))
str(df)
class(df)

dfTest <- as.data.frame(lapply(dfTest, as.numeric))
str(dfTest)
class(dfTest)

# Take out ID-column
idTrain <- df$SK_ID_CURR
df$SK_ID_CURR <- NULL

idTest <- dfTest$SK_ID_CURR
dfTest$SK_ID_CURR <- NULL



################################################################################################################################
# Train model
################################################################################################################################

###################################
# Which performance metrics to choose ?
###################################
# Depends on skewness(imbalanced data) and business objective:

# Skewness assessment:
# See in analysis_savingsmodel_modelling.R techniques for treathing skewed data (Undersampling, Oversampling, 
# syntethic, cost sensitive learning)
table(df$TARGET) # how skewed?

# Business objective:
# is it e.g. vital not to have any false positives??

  # ROC
  # There may be situations when ROC fails to deliver trustworthy performance. It has few shortcomings such as:
  # - It may provide overly optimistic performance results of highly skewed data.
  # - It does not provide confidence interval on classifier's performance
  # - It fails to infer the significance of different classifier performance.
  
  # Precision: 
  # It is a measure of correctness achieved in positive prediction i.e. of observations labeled as positive, 
  # how many are actually labeled positive.
  # Precision = TP / (TP + FP)
  
  # Recall: 
  # It is a measure of actual observations which are labeled (predicted) correctly i.e. 
  # how many observations of positive class are labeled correctly. It is also known as 'Sensitivity'.
  # Recall = TP / (TP + FN)

performance_metric <- "auc"

###################################
# Do we need methods to treat imbalanced datasets? See analysis_savingsmodel_modelling.R
###################################

# See for xgboost:
# https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html#build-the-model

# bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
#                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")


###################################
# Quick & dirty training and evaluation
###################################
# Here get a hunch of how a plain algo will perform, before doing more advanced stuff

# Here make a test and training set of the training set --> so we can look a bit on the confusion matrix
set.seed(3456)
trainQuickIndex <- createDataPartition(df$TARGET, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dfQuickTrain <- df[ trainQuickIndex,]
dfQuickTest  <- df[-trainQuickIndex,]
dim(dfQuickTrain)

dfQuickTrainMat <- as.matrix(dfQuickTrain[,!(colnames(dfQuickTrain) %in% "TARGET")])
dfQuickTestMat <- as.matrix(dfQuickTest)

# Train a xgboost model
start_time <- Sys.time()
set.seed(825)
bst <- xgboost(data = dfQuickTrainMat, label = dfQuickTrain$TARGET, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 20,objective = "binary:logistic",
               eval_metric = performance_metric)
time_end <- (Sys.time()-start_time)

# Importance
# - Gain is the improvement in accuracy brought by a feature to the branches it is on.
# - Cover measures the relative quantity of observations concerned by a feature.
# - Frequency is a simpler way to measure the Gain. It just counts the number of times 
#   a feature is used in all 
#   generated trees. You should not use it (unless you know why you want to use it).
importance <- xgb.importance(feature_names = colnames(dfQuickTrainMat), model = bst)
head(importance)

# Predict and confusion matrix
predQuick <- predict(bst, dfQuickTestMat)
pred.classQuick <- ifelse(predQuick >= 0.5, 1,0)
library(e1071)
confusionMatrix(data = as.factor(pred.classQuick), reference = as.factor(dfQuickTest$TARGET))


######################################################################
# Param default tuning
######################################################################
#preparing matrix 
labels <- df$TARGET 
labels <- as.numeric(labels)
new_tr <- model.matrix(~.+0,data = df[,!(colnames(df) %in% "TARGET")], with = F) 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

ts_label <- dfTest$TARGET
ts_label <- as.numeric(ts_label)
new_ts <- model.matrix(~.+0,data = dfTest[,!(colnames(dfTest) %in% "TARGET")], with = F) 
dtest  <- xgb.DMatrix(data = new_ts,label = ts_label) 




#default parameters
params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, 
               gamma=0.5, 
               max_depth=6, 
               min_child_weight=1, 
               subsample=1, 
               colsample_bytree=1, 
               eval_metric = "auc",
               scale_pos_weight = 0.1
               )

# Cross validation 
##
# Using the inbuilt xgb.cv function, let's calculate the best nround for 
# this model. In addition, this function also returns CV error, which
# is an estimate of test error.
set.seed(846456)
start_time_cv <- Sys.time()
print(start_time_cv)
xgbcv <- xgb.cv( params = params, 
                 data = dtrain, 
                 nrounds = 200, 
                 nfold = 5, 
                 showsd = T, 
                 stratified = T, 
                 print.every.n = 10, 
                 early.stop.round = 20, 
                 maximize = F)
time_end_cv <- (Sys.time()-start_time_cv)
xgbcv #is test_error_mean low?
 
# Plot error vs iterations with test and train (if want AUC must change train_error_mean
# to train_auc_mean etc.)
ggplot(xgbcv$evaluation_log, aes(iter)) + 
   geom_line(aes(y = train_auc_mean, colour = "train_auc_mean")) + 
   geom_line(aes(y = test_auc_mean, colour = "test_auc_mean"))
 
#first default - model training
# should have nrounds = xgbcv$best_iteration IF ERROR used in params - not sure if AUC
xgb1 <- xgb.train( params = params, data = dtrain, nrounds = 100, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print.every.n = 10, early.stop.round = 10,
                   maximize = F )

xgb2 <- xgb.train( params = params, data = dtrain, nrounds = 100)

#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.2,1,0)
xgbpred2 <- predict (xgb2,dtest)
xgbpred2 <- ifelse (xgbpred2 > 0.05,1,0)

#confusion matrix
confusionMatrix (as.factor(xgbpred), as.factor(ts_label))
confusionMatrix(as.factor(xgbpred2), as.factor(ts_label))

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 

# ROC
# - The ROC curve is created by plotting the true positive rate (TPR) against 
# - the false positive rate (FPR) at various threshold settings.
library("ROCR")
# True positive vs flase positive
pred.roc <- ROCR::prediction(xgbpred2, ts_label, 
                             label.ordering = c("0","1")) #ok takes Positive Class = 1
preformance.roc <- performance(pred.roc, "tpr","fpr")
plot(preformance.roc, col="black", lty=3, lwd=3)
auc <- performance(pred.roc,"auc")
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 4))
maxauc<-max(round(auc, digits = 4))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.35,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.4,box.col = "white")


###################################
# HypeerParam tuning in Caret
###################################
# Set training grid
xgb.grid = expand.grid(
  nrounds = 100,
  eta = c(0.3, 0.2, 0.1, 0.05, 0.01),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
  )

# Calibrate trainControl
xgb.trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all", # save losses across all models
  classProbs = T,       # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# Re-format target_var to factor (due to caret-package)
lab_cv_tuning <- factor(ifelse(labels == 0, "noChurn", "Churn"))
df_caretTrain <- as.data.frame(new_tr)
str(df_caretTrain)

# Train model
set.seed(686815384)
start_time_cv_tuning <- Sys.time()
print(start_time_cv_tuning)
full.train = train(
  x = df_caretTrain,
  y = lab_cv_tuning,
  trControl = xgb.trcontrol,
  tuneGrid = xgb.grid,
  method = "xgbTree",
  metric = "ROC"
)
time_end_cv_tuning <- (Sys.time()-start_time_cv_tuning)
print(time_end_cv_tuning) # Time difference of 1.090792 hours

#Should also try metric = "Kappa"

# Check result
full.train
# Scatter plot of AUC as function of max_depth and eta
ggplot(full.train$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

trellis.par.set(caretTheme())
densityplot(full.train, pch = "|")

# Distribution of ROC in the resamples
histogram(full.train$resample$ROC)

# ROC as function of tuning params
plot(full.train)


#####
# Check between models
#####
resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps
summary(resamps)
dotplot(
  resamps,
  scales=list(x=list(relation="free"))
)

###########
# Choosing the Final Model
###########
whichTwoPct <- tolerance(full.train$results, metric = "ROC", 
                         tol = 2, maximize = TRUE)  
cat("best model within 2 pct of best:\n")
full.train$results[whichTwoPct,1:6]
full.train$finalModel
final.model <- full.train$finalModel


# Test final model
finalpred <- predict (final.model,dtest)
finalpred_bin <- ifelse (finalpred > 0.4,1,0)
confusionMatrix (as.factor(finalpred_bin), as.factor(ts_label))

final.pred.roc <- ROCR::prediction(finalpred, ts_label, 
                             label.ordering = c("0","1")) #ok takes Positive Class = 1
preformance.roc.final <- performance(final.pred.roc, "tpr","fpr")
plot(preformance.roc.final, col="black", lty=3, lwd=3)
auc <- performance(final.pred.roc,"auc")
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 4))
maxauc<-max(round(auc, digits = 4))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.35,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.4,box.col = "white")



################################################################################################################################
# Model assessment 
################################################################################################################################


#######################
#Measure feature importance
#######################
# Check if make sense! The column Gain provide the information we are looking for. 
# Cover measures the relative quantity of observations concerned by a feature.
  # Gain is the improvement in accuracy brought by a feature to the 
  # branches it is on. The idea is that before adding a new split on a feature X 
  # to the branch there was some wrongly classified elements, after adding the split 
  # on this feature, there are two new branches, and each of these branch is more accurate 
  # (one branch saying if your observation is on this branch then it should be classified as 1, 
  # and the other branch saying the exact opposite).
# https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html#build-the-model
# importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
# head(importance)


#######################
#Plot feature importance
#######################
#xgb.plot.importance(importance_matrix = importance)


#######################
# Do these results make sense?
#######################
# Let's check some Chi2 between each of these features and the label.
# Higher Chi2 means better correlation.
# c2 <- chisq.test(df$Age, output_vector)
# print(c2)
# c2 <- chisq.test(df$AgeDiscret, output_vector)
# print(c2)


#######################
# Try other methods 
#######################
# If you want to try Random ForestsT algorithm, you can tweak Xgboost parameters! CHECK THIS: Just use 0.5 factor on sampling rows and columns:
# bst <- xgboost(data = train$data, label = train$label, max_depth = 4, num_parallel_tree = 1000, subsample = 0.5, colsample_bytree =0.5, nrounds = 1, objective = "binary:logistic")

  

################################################################################################################################
# OUTPUT
################################################################################################################################
save.path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/3model/"

# Save full.train: param search full model - caret
filename.full.train <- paste0(save.path,
                               "fullTrain",
                               format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
saveRDS(full.train, filename.full.train) #caret
# Backup
saveRDS(full.train, paste0(filename.full.train,".rds")) #caret


# Save final.model: the optimal model
filename.finalModel <- paste0(save.path,
                               "finalModel",
                               format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
xgb.save(final.model, filename.finalModel)
# Backup
saveRDS(final.model, paste0(filename.finalModel,".rds")) #caret

# Save Params
save.params <- list()
save.params$xgb.grid <- xgb.grid
save.params$filename.finalModel <- filename.finalModel
save.params$full.train <- full.train

filename.save.params <- paste0(save.path,"params",
                               format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
saveRDS(save.params, file=filename.save.params) # Load it with readRDS(filename.save.params)















