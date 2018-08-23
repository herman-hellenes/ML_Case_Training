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
# raw_path <- "C:/Users/herman.a.hellenes/Desktop/Case/QuantCase/Fra eirik/drive-download-20180823T005147Z-001"
# raw_filename <- "ml_case_test_hist_data.csv"
# raw_filename <- "ml_case_test_data.csv"

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
fix(df) #spend quite some time here - try to segment columns into different groups and have this sheet open when doing the below
utils::View(df) # Can also do View(), however not always View give out all the columns. I thin fix() is a bit nicer
table(df$TARGET) 


#Understanding the columns
###
summary(df) # here lists NA's per column, max, min, mean, class (if character it says so).. Most important print out!

# Checking categorical vars:
unique(df$NAME_CONTRACT_TYPE) 
unique(df$CODE_GENDER) 
unique(df$FLAG_OWN_CAR) 

###########
# Small dataprep - Making a numerical dataframe, in order to do further analysis
###########
dfnum <- data.table(df) 
dmy <- dummyVars(" ~ .", data = dfnum) #from caret -> making dummy vars
dfnum <- data.frame(predict(dmy, newdata = dfnum))
dim(dfnum)
str(dfnum)


###########
# Distribution and Extreme values
###########
sub_num <- dfnum[sapply(dfnum, is.numeric)]
sapply(sub_num, function(x) sum(is.na(x)))
sapply(sub_num, FUN = function(x) quantile(x, seq(0,1,0.1), na.rm = T)) #MÅ TA VEKK NON-numeric vars (går bare på disse)
sapply(sub_num, FUN = function(x) quantile(x, seq(0.99,1,0.001),na.rm = T))
#looks ok

# Visualization (see http://topepo.github.io/caret/visualizations.html for understanding below)
###
# Scatterplot Matrix
library(AppliedPredictiveModeling)
library(caret)
transparentTheme(trans = .4)

df_for_plot <- dfnum[1:1000, c("AMT_CREDIT","AMT_ANNUITY","REGION_POPULATION_RELATIVE")] #must do some formatting in order to use featurePlot
df_for_plot$TARGET <- dfnum[1:1000, 2]
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

# Box Plots
featurePlot(x = df_for_plot[,1:3], 
            y = df_for_plot[,4], 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

################################################################################################################################
# Data quality 
################################################################################################################################
summary(df) # here lists NA's per column, max, min, mean, class (if character it says so).. Most important print out!

# Check if unique IDs!
sum(duplicated(df$SK_ID_CURR))

# Look into spesific variables
unique(df$TARGET) 
table(df$TARGET) 

###########
# Look for missing values
###########
summary(df)

###########
# Look for 0 variance columns
###########
# Looking for variables with none or little variance
nzv <- nearZeroVar(df, saveMetrics= TRUE)

nzv[nzv$nzv,][,]

# Inspection of near-zero variance
for(i in 1:dim(nzv[nzv$nzv,][,])[1]){
  if(dim(table(df[,rownames(nzv[nzv$nzv,][,])[i]]))[1] < 15){
    print(rownames(nzv[nzv$nzv,][,])[i])
    print(table(df[,rownames(nzv[nzv$nzv,][,])[i]], df$TARGET))
  }
}

# df <- data.table(df)
# filteredDescr <- df[, -nzv]
# dim(df)
# dim(filteredDescr) 

###########
# Identifying Correlated Predictors
###########
descrCor <-  cor(dfnum)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
summary(descrCor[upper.tri(descrCor)])



############################################
# Data Exploration and Data Quality report  
############################################
# Here make a wrap up of the data: both the quality and what the data tell us already now

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