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
raw_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/raw_homeCredit/application_train"
raw_filename <- "application_train.csv"

#Read data
raw_data <- read.csv(paste0(raw_path, "/", raw_filename),
                     sep = ",", dec=".", stringsAsFactors = F)

#Quick QA if reading went ok-ish (make sure its the same as in Data Understanding)
dim(raw_data)
head(raw_data)
View(raw_data)


################################################################################################################################
# Data Preparation 
################################################################################################################################


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
# Format Data
############################################
# Look here if have to prepare for xgboost (e.g. Xgboost manages only numeric vectors, 
# so must convert categorical variables to numeric one etc.):
#   - https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html
#   - https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
str(df)


################################################################################################################################
# OUTPUT
################################################################################################################################
# Setting path and current file
path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Training/raw_homeCredit/2dataPreparation/"
filename <- "prepared_data.R"
setwd(path)
system('pwd -P')

# Loading versions
library(git2r)
current.commit <- system("git rev-parse HEAD",  intern = TRUE)
current.branch <- system(paste("git branch --contains ", current.commit),  intern = TRUE)
#current.r.version <- system("r --version", , intern = TRUE)[1]
#current.git.version <- system("git --version", intern = TRUE)

filename_full <- paste0(path, filename, current.commit, current.branch, format(Sys.time(), "%Y-%m-%d_%H%M%S_"))
