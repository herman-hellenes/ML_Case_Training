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


require(data.table)


################################################################################################################################
# INPUT
################################################################################################################################
path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/1dataUnderstanding"


################################################################################################################################
# Train model
################################################################################################################################
# See for xgboost:
# https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html#build-the-model

# bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
#                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")



################
# Param tuning
################


################
# Cross validation 
################
# Cross validation
# xgb.params <- list(
#   objective = "binary:logistic",
#   eta = 0.015,
#   max.depth = 5,
#   eval_metric = "auc"
# )
# 
# set.seed(846456)
# start_time_cv <- Sys.time()
# print(start_time_cv)
# xgb.cv <- xgb.cv(data = train.matrix,
#                  label = lab,
#                  params = xgb.params,
#                  nrounds = 1500,
#                  verbose = T,
#                  early.stop.round = 10,
#                  prediction = TRUE,
#                  showsd = TRUE,
#                  nfold = 10,
#                  stratified = TRUE
# )
# time_end_cv <- (Sys.time()-start_time_cv)
# print(time_end_cv) #20.79372 mins
# 
# # Plot AUC vs iterations with test and train
# ggplot(xgb.cv$evaluation_log, aes(iter)) + 
#   geom_line(aes(y = train_auc_mean, colour = "train_auc_mean")) + 
#   geom_line(aes(y = test_auc_mean, colour = "test_auc_mean"))
# 



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

# Save OPT_Res (for evaluation)
filename.OPT_Res <- paste0("C:/Users/h803499/Documents",
                           format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
saveRDS(OPT_Res, file=filename.OPT_Res) # Load it with readRDS(filename.OPT_Res)


# Save the optimal model
filename.xgb.optimal <- paste0("C:/Users/h803499/Documents",
                               format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
xgb.save(model.xgb.optimal, filename.xgb.optimal)


# Save Params
save.params <- list()
save.params$input.params <- parameter.lib
save.params$output.params <- output.lib
save.params$output.params$filename.xgb.optimal <- filename.xgb.optimal
save.params$output.params$filename.OPT_Res <- filename.OPT_Res

filename.save.params <- paste0("C:/Users/h803499/Documents/",
                               format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
saveRDS(save.params, file=filename.save.params) # Load it with readRDS(filename.save.params)















