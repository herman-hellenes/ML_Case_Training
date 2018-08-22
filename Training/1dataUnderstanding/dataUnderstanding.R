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
df <- raw_data
################################################################################################################################
# Data understanding
################################################################################################################################
#Quick QA if reading went ok, and give us a high level understanding of the data
dim(df)
head(df)
str(df) # give a quick overview - like head but state the type as well
View(df) # spend quite some time here - try to segment columns into different groups and have this sheet open when doing the below
table(df$TARGET) 


#Understanding the columns
###
summary(df) # here lists NA's per column, max, min, mean, class (if character it says so).. Most important print out!

# Checking categorical vars:
unique(df$NAME_CONTRACT_TYPE) 
unique(df$CODE_GENDER) 
unique(df$FLAG_OWN_CAR) 


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

###########
# Look for 0 variance columns
###########
# Looking for variables with none or little variance
nzv <- nearZeroVar(df.validbanks.segment, saveMetrics= TRUE)
nzv[nzv$nzv,][,]

# Inspection of near-zero variance
for(i in 1:dim(nzv[nzv$nzv,][,])[1]){
  if(dim(table(df.validbanks.segment[,rownames(nzv[nzv$nzv,][,])[i]]))[1] < 15){
    print(rownames(nzv[nzv$nzv,][,])[i])
    print(table(df.validbanks.segment[,rownames(nzv[nzv$nzv,][,])[i]], df.validbanks.segment$target_var_save))
  }
}

###########
# Extreme values
###########
sapply(sub_num, function(x) sum(is.na(x)))
sub_num <- df.validbanks.segment[sapply(df.validbanks.segment, is.numeric)]
sapply(sub_num, FUN = function(x) quantile(x, seq(0,1,0.1))) #MÅ TA VEKK NON-numeric vars (går bare på disse)
sapply(sub_num, FUN = function(x) quantile(x, seq(0.99,1,0.001)))
#looks ok

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