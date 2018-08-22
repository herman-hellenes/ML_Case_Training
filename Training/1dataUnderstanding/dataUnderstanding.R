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

################################################################################################################################
# Data understanding
################################################################################################################################
#Quick QA if reading went ok-ish
dim(raw_data)
head(raw_data)
View(raw_data)

summary(raw_data)
str(df)

hadde noen enda kulere summary funcsjons hos eika --> lese ut NA, hvilken datatype det er osv... Legg inn mange slike. Samt gode gamle table


# Look into spesific variables
levels(df[,Treatment])
table(is.na(df.loaded$Kjonn))
table(df.validbanks$target_var_save)


# Check if unique IDs!
unique(df$ID)

###########
# Look for missing values
###########

###########
# Extreme values
###########
sapply(sub_num, function(x) sum(is.na(x)))
sub_num <- df.validbanks.segment[sapply(df.validbanks.segment, is.numeric)]
sapply(sub_num, FUN = function(x) quantile(x, seq(0,1,0.1))) #MÅ TA VEKK NON-numeric vars (går bare på disse)
sapply(sub_num, FUN = function(x) quantile(x, seq(0.99,1,0.001)))
#looks ok


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

############################################
# Data Exploration and Data Quality report  
############################################

################################################################################################################################
# OUTPUT
################################################################################################################################
