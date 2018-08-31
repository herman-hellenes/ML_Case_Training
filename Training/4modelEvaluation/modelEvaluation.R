# __author__ = "Herman Hellenes"
# __version__ = "1.0"
# __maintainer__ = "Herman Hellenes"
# __email__ = "herman.hellenes@gmail.com"
# __creation__ = "22/08/2018"
# __status__ = "Production"

############################################
# Mission description
############################################
# Goal: Conclude on performance
#       Here we show and evaluate the model's performance, discuss the findings and
#       conclude. 

# Questions: 
#     - ...

# Todo
#     - ...

# Output:
#     - Plots


require(data.table)


################################################################################################################################
# INPUT
################################################################################################################################

# Load data
pathTrain <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/2dataPreparation/prepared_data_train2018-08-30_102224_.csv"
pathTest <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/2dataPreparation/prepared_data_test2018-08-30_102225_.csv"

#Read data
input_dataTrain <- read.csv(pathTrain,
                            sep = ",", dec=".", stringsAsFactors = F)
dfTrain <- input_dataTrain

input_dataTest <- read.csv(pathTest,
                           sep = ",", dec=".", stringsAsFactors = F)
dfTest <- input_dataTest


# Load Models
path_model <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/3model/"
final.xgb.loaded <- readRDS(paste0(path_model,"finalModel2018-08-30_163857_.rds"))
xgb.load

###################
# Quick formatting
###################
#Quick QA if reading went ok
dim(dfTrain)
head(dfTrain)
str(dfTrain)

dim(dfTest)
head(dfTest)
str(dfTest)

# Quick formatting
##
dfTrain <- as.data.frame(lapply(dfTrain, as.numeric))
str(dfTrain)
class(dfTrain)

dfTest <- as.data.frame(lapply(dfTest, as.numeric))
str(dfTest)
class(dfTest)

# Take out ID-column
idTrain <- dfTrain$SK_ID_CURR
dfTrain$SK_ID_CURR <- NULL

idTest <- dfTest$SK_ID_CURR
dfTest$SK_ID_CURR <- NULL

#preparing matrix 
labels <- dfTrain$TARGET 
labels <- as.numeric(labels)
new_tr <- model.matrix(~.+0,data = dfTrain[,!(colnames(dfTrain) %in% "TARGET")], with = F) 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

ts_label <- dfTest$TARGET
ts_label <- as.numeric(ts_label)
new_ts <- model.matrix(~.+0,data = dfTest[,!(colnames(dfTest) %in% "TARGET")], with = F) 
dtest  <- xgb.DMatrix(data = new_ts,label = ts_label) 

################################################################################################################################
# Main
################################################################################################################################
# Predict
model.pred <- predict(final.xgb.loaded, dtest)


###########
# AUC
###########
# True positive vs flase positive
pred.roc <- ROCR::prediction(model.pred, ts_label, 
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

###########
# Prediktiv kraft gjennom Cutoff-sweep (from Eika slide)
###########
## Performance and Cutoffs

cutoff_optimalizer <- function(cutmin, cutmax, steps, pred, target_lable){
  
  P <- list()
  df.conf <- list()
  sensitivity <- list()
  precision <- list()
  times.precision.better.than.random <- list()
  cuts <- seq(cutmin,cutmax,length=steps)
  for(i in 1:steps ){
    # Classify with class probabilities
    pred.temp <- ifelse(pred >= cuts[i], 1,0)
    # confusionMatrix
    confusion.optimal <- confusionMatrix(pred.temp, target_lable, positive = levels(as.factor(target_lable))[2])
    
    df.conf[[i]] <- data.frame(confusion.optimal$table)
    df.conf[[i]]$category <- paste(df.conf[[i]] $Prediction,df.conf[[i]] $Reference)
    df.conf[[i]]$category <- ifelse(df.conf[[i]] $category == "0 0", "TN" , df.conf[[i]] $category)
    df.conf[[i]]$category <- ifelse(df.conf[[i]] $category == "1 0", "FP" , df.conf[[i]] $category)
    df.conf[[i]]$category <- ifelse(df.conf[[i]] $category == "0 1", "FN" , df.conf[[i]] $category)
    df.conf[[i]]$category <- ifelse(df.conf[[i]] $category == "1 1", "TP" , df.conf[[i]] $category)
    df.conf[[i]]$Prediction <- NULL
    df.conf[[i]]$Reference <- NULL
    TN <- df.conf[[i]][df.conf[[i]]$category == "TN",]$Freq
    FP <- df.conf[[i]][df.conf[[i]]$category == "FP",]$Freq
    FN <- df.conf[[i]][df.conf[[i]]$category == "FN",]$Freq
    TP <- df.conf[[i]][df.conf[[i]]$category == "TP",]$Freq
    P[i] <- TP + FP
    # Important to report:
    # Recall ( Sensitivity )  = TP / (TP + FN): Get it from confusion.optimal: 0.064718 
    # Says to what extent we are reaching potential customers: 100% means we get all the 
    # customers -> nobody are missed. So this is like opportunity cost
    sensitivity[i] <- TP/(TP + FN)
    
    #Precision: It is a measure of correctness achieved in positive prediction 
    # i.e. of observations labeled as positive, how many are actually labeled positive.
    # Precision = TP / (TP + FP)
    # -> so this will be the hit rate when picking up the phone (of course if statistics/model are
    # correct in real life, which is not the case)
    # Precision = Pos Pred Value (confusion matrix) = 0.575597
    precision[i] = TP / (TP + FP)
    
    # Weighted against the total -> meaning given we would just call everyone!
    # Precision / ((TP + FN) / (TP + FN + TN + FP) )
    times.precision.better.than.random[i] <- precision[[i]] / ((TP + FN) / (TP + FN + TN + FP) )
    
  }
  return(list("P" = P,"times.precision.better.than.random" = times.precision.better.than.random, "precision" = precision, "sensitivity" = sensitivity, "df.conf" = df.conf, "cuts" =cuts))
}  

# Generate cut off sweep
cutoff.data <- cutoff_optimalizer(cutmin = 0.2, cutmax = 0.8, steps = 100, pred = model.pred, target_lable = ts_label)

# Plot the cutt of sweep
plot(unlist(cutoff.data$P), unlist(cutoff.data$precision), 
     xlab = "Predicted positive: TP + FP", ylab ="Positive predictive value: TP / (TP + FP)")
plot(unlist(cutoff.data$P), unlist(cutoff.data$sensitivity),
     xlab = "Total positive population: P", ylab ="Sensitivity : TP/(TP + FN)")
plot(unlist(cutoff.data$P), unlist(cutoff.data$times.precision.better.than.random),
     xlab = "Predicted positive: TP + FP", ylab ="Lift from random ")
plot(unlist(cutoff.data$sensitivity), unlist(cutoff.data$times.precision.better.than.random),
     xlab = "Sensitivity : TP/(TP + FN)", ylab ="Lift from random : Precision / (P / Total )")
plot(unlist(cutoff.data$precision), unlist(cutoff.data$sensitivity), 
     xlab = "Precision : TP / (TP + FP)", ylab ="Sensitivity : TP/(TP + FN)")
plot(unlist(cutoff.data$P), unlist(cutoff.data$cuts), 
     xlab = "Predicted positive: TP + FP", ylab ="Cut-off probability")
plot(unlist(cutoff.data$precision), unlist(cutoff.data$cuts), 
     xlab = "Precision : TP / (TP + FP)", ylab ="Cutoff probability")
plot(unlist(cutoff.data$sensitivity), unlist(cutoff.data$cuts), 
     xlab = "Sensitivity : TP/(TP + FN)", ylab ="Cutoff probability")
plot(unlist(cutoff.data$times.precision.better.than.random), unlist(cutoff.data$cuts), 
     xlab = "Lift from random : Precision / (P / Total )", ylab ="Cutoff probability")

###########
# Lift from random (from Eika slide) - spesielt når skewed 
###########
TopDecileLift(model.optimal.pred, test$target_var_save)
plotLift(model.optimal.pred, test$target_var_save)
plotLift(model.optimal.pred, test$target_var_save, cumulative = F)
plotLift(predicted = model.optimal.pred, labels = test$target_var_save, cumulative = T, n.buckets = 100)
plotLift(predicted = model.optimal.pred, labels = test$target_var_save, cumulative = F, n.buckets = 100)

lift.optimal <- caret::lift(as.factor(test$target_var_save)~model.optimal.pred)
xyplot(lift.optimal)
###########
# Volume analysis through cut-off sweep (from Eika slide)
###########

###########
# Importance analysis (from model.R)
###########

## Variable importance
importance_matrix_optimal <- xgb.importance(colnames(train.matrix),model = xgb.optimal.loaded)
#print(importance_matrix_optimal)
xgb.plot.importance(importance_matrix = importance_matrix_optimal, top_n = 15)

###########
# Robusthet i parameter tuning (se Eika-slide 39-41)
###########
#* https://web.stanford.edu/~hastie/Papers/Ecology/ELH_appendixs1-s2.pdf
#** https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
#*** https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python


###########
# More
###########
# http://topepo.github.io/caret/measuring-performance.html



################################################################################################################################
# OUTPUT
################################################################################################################################
