## Begin by reading the RDS files
setwd("/Users/KCUser/ExMachina/Kagglespace/Springleaf_Marketing_Response")
train_df <- readRDS(file = "./data/train_data.rds")
test_df <- readRDS(file = "./data/test_data.rds")
train_test_df <- readRDS(file = "./data/train_test.rds")

## Convert ZIP column to Character
train_test_df$VAR_0241 <- as.character(train_test_df$VAR_0241)

## Get only NUMERIC columns
numeric_columns <- sapply(train_test_df, is.numeric)
train_test_numeric_df <- train_test_df[, numeric_columns]

## Get only CHAR columns
char_columns <- sapply(train_test_df, is.character)
train_test_char_df <- train_test_df[, char_columns]


## Lets investigate the NUMERIC columns first
## How many and NZV ??
nzv_numeric_cols <- nearZeroVar(train_test_numeric_df)
train_test_numeric_nzv_df <- train_test_numeric_df[, -nzv_numeric_cols]

## Try PCA on the numeric columns
set.seed(100)
preProcValues <- preProcess(train_test_numeric_nzv_df, method = c("center", "scale"))
## preProcValues <- preProcess(train_test_numeric_nzv_df, method = "pca")
## Using the pre-processed values, transform the data set
train_test_numeric_nzv_xformed_df <- predict(preProcValues, train_test_numeric_nzv_df)
## Replace the ID column with the original values
train_test_numeric_nzv_xformed_df$ID <- train_test_numeric_nzv_df$ID


## There are some NAs in the data set - carried over from the original data set
## Since these are NUMERIC predictors only, its will be easy to apply IMPUTATION on these predictors
## Instead of a straight replacement by a dummy value not present in the data set like -9898989
set.seed(100)
preProcValues <- preProcess(train_test_numeric_nzv_xformed_df, method = c("knnImpute"))
## Using the pre-processed values, transform the data set
train_test_numeric_nzv_xformed_df_backup <- train_test_numeric_nzv_xformed_df
train_test_numeric_nzv_xformed_df <- predict(preProcValues, 
                                             train_test_numeric_nzv_xformed_df, 
                                             na.action=na.omit)

## The above IMPUTATION attempt failed with some error code
## So doing the old fashioned way - setting NA to a value not in the data set
train_test_numeric_nzv_xformed_df[is.na(train_test_numeric_nzv_xformed_df)] <- -989898.00



## Now work on the FACTOR variables

## Next Tag column names with something more meaningful, wherever possible
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0200"] <- "VAR_0200_CITY"
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0237"] <- "VAR_0237_STATE"
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0274"] <- "VAR_0274_STATE"
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0404"] <- "VAR_0404_TITLE" ## >> "NO JOB TITLE DEFINED" >> Indicates this is a TITLE column
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0466"] <- "VAR_0466_I"
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0467"] <- "VAR_0467_DISCH"
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_0493"] <- "VAR_0493_PROFESSION"
colnames(train_test_char_df)[colnames(train_test_char_df) == "VAR_1934"] <- "VAR_1934_CHANNEL"

# ## Time to impute
# install.packages("Amelia")
# library("Amelia")
# #Running Amelia on this Free Trade dataset
# amelia_out <- amelia(x=train_test_df, m=5)
 
# ## There are some columns in the data set that are completely missing
# ## These columns - features have no predictive power - these are OK to be dropped
# ## Checking them on H2O also tells me the same
# ## Amelia says:
# ## >>> Amelia Error Code:  4 
# ## >>> The data has a column that is completely missing or only has one,observation.  Remove these columns: VAR_0207, VAR_0213, VAR_0840 
# train_test_df <- subset(train_test_df, select = -c(VAR_0044, VAR_0207, VAR_0213, VAR_0840))
# 
# ## Running Amelia again
# amelia_out <- amelia(x=train_test_df, m=5)

## Another error, something to do with Data types
# Amelia Error Code:  37 
# The following variable(s) are 'factors': 
#   VAR_0001, VAR_0005, VAR_0008, VAR_0009, VAR_0010, VAR_0011, VAR_0012, VAR_0043, VAR_0044, VAR_0073, VAR_0075, VAR_0156, 
#   VAR_0157, VAR_0158, VAR_0159, VAR_0166, VAR_0167, VAR_0168, VAR_0169, VAR_0176, VAR_0177, VAR_0178, VAR_0179, VAR_0196, 
#   VAR_0200, VAR_0202, VAR_0204, VAR_0214, VAR_0216, VAR_0217, VAR_0222, VAR_0226, VAR_0229, VAR_0230, VAR_0232, VAR_0236, 
#   VAR_0237, VAR_0239, VAR_0274, VAR_0283, VAR_0305, VAR_0325, VAR_0342, VAR_0352, VAR_0353, VAR_0354, VAR_0404, VAR_0466, 
#   VAR_0467, VAR_0493, VAR_1934
# You may have wanted to set this as a ID variable to remove it
# from the imputation model or as an ordinal or nominal
# variable to be imputed.  Please set it as either and
# try again. 


## So we need to change the data types for the datetime stamp columns 
## >>> I dont believe that TIME stamp is predictive
train_test_char_df$VAR_0073 <- sapply(train_test_char_df$VAR_0073, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0075 <- sapply(train_test_char_df$VAR_0075, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0156 <- sapply(train_test_char_df$VAR_0156, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0157 <- sapply(train_test_char_df$VAR_0157, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0158 <- sapply(train_test_char_df$VAR_0158, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0159 <- sapply(train_test_char_df$VAR_0159, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0166 <- sapply(train_test_char_df$VAR_0166, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0167 <- sapply(train_test_char_df$VAR_0167, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0168 <- sapply(train_test_char_df$VAR_0168, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0169 <- sapply(train_test_char_df$VAR_0169, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0176 <- sapply(train_test_char_df$VAR_0176, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0177 <- sapply(train_test_char_df$VAR_0177, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0178 <- sapply(train_test_char_df$VAR_0178, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0179 <- sapply(train_test_char_df$VAR_0179, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0204 <- sapply(train_test_char_df$VAR_0204, function(x) substr(x, 0, 7))
train_test_char_df$VAR_0217 <- sapply(train_test_char_df$VAR_0217, function(x) substr(x, 0, 7))

## After the time stamps are cut out, convert the dates into >>> Week of Year
train_test_char_df$VAR_0073_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0073, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0073_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0073, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0073_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0073, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0073)

train_test_char_df$VAR_0075_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0075, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0075_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0075, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0075_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0075, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0075)

train_test_char_df$VAR_0156_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0156, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0156_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0156, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0156_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0156, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0156)

train_test_char_df$VAR_0157_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0157, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0157_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0157, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0157_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0157, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0157)

train_test_char_df$VAR_0158_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0158, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0158_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0158, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0158_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0158, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0158)

train_test_char_df$VAR_0159_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0159, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0159_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0159, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0159_YER<- as.character(format(as.Date(strptime(train_test_char_df$VAR_0159, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0159)

train_test_char_df$VAR_0166_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0166, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0166_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0166, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0166_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0166, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0166)

train_test_char_df$VAR_0167_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0167, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0167_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0167, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0167_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0167, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0167)

train_test_char_df$VAR_0168_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0168, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0168_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0168, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0168_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0168, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0168)

train_test_char_df$VAR_0169_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0169, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0169_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0169, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0169_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0169, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0169)

train_test_char_df$VAR_0176_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0176, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0176_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0176, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0176_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0176, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0176)

train_test_char_df$VAR_0177_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0177, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0177_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0177, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0177_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0177, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0177)

train_test_char_df$VAR_0178_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0178, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0178_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0178, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0178_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0178, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0178)

train_test_char_df$VAR_0179_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0179, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0179_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0179, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0179_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0179, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0179)

train_test_char_df$VAR_0204_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0204, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0204_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0204, format = "%d%b%y")), "%m"))
train_test_char_df$VAR_0204_YER <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0204, format = "%d%b%y")), "%Y"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0204)

train_test_char_df$VAR_0217_WOY <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0217, format = "%d%b%y")), "%U"))
train_test_char_df$VAR_0217_MON <- as.character(format(as.Date(strptime(train_test_char_df$VAR_0217, format = "%d%b%y")), "%m"))
train_test_char_df <- subset(train_test_char_df, select=-VAR_0217)


######## I think converting into LOGICAL does not make sense; they can stay FACTORS
######## After all, TRUE/FALSE are only 2 states of these variables; categorical is fine

# ## There are bunch of TRUE/FALSE columns
# ## TRUE is always missing 
# train_test_char_df$VAR_0008 <- as.logical(train_test_char_df$VAR_0008)
# train_test_char_df$VAR_0009 <- as.logical(train_test_char_df$VAR_0009)
# train_test_char_df$VAR_0010 <- as.logical(train_test_char_df$VAR_0010)
# train_test_char_df$VAR_0011 <- as.logical(train_test_char_df$VAR_0011)
# train_test_char_df$VAR_0012 <- as.logical(train_test_char_df$VAR_0012)
# train_test_char_df$VAR_0043 <- as.logical(train_test_char_df$VAR_0043)
# train_test_char_df$VAR_0196 <- as.logical(train_test_char_df$VAR_0196)
# train_test_char_df$VAR_0226 <- as.logical(train_test_char_df$VAR_0226)
# train_test_char_df$VAR_0229 <- as.logical(train_test_char_df$VAR_0229)
# train_test_char_df$VAR_0230 <- as.logical(train_test_char_df$VAR_0230)
# train_test_char_df$VAR_0232 <- as.logical(train_test_char_df$VAR_0232)
# train_test_char_df$VAR_0236 <- as.logical(train_test_char_df$VAR_0236)
# train_test_char_df$VAR_0239 <- as.logical(train_test_char_df$VAR_0239)

## Ignoring the STATE columns - there are 2 of these and they do NOT agree with each other for a given User ID
## Ignoring the CITY columns - these need to be normalized and its better to use ZIP instead
## So instead using the Zipcodes - splitting them into 3- and 5-digit zips and treating them as FACTORS
## They must be factors since these are LABELS not continuous variables
train_test_char_df$VAR_0241_ZIP3 <- as.character(sapply(train_test_char_df$VAR_0241, function(x) substr(x, 0, 3)))
train_test_char_df$VAR_0241_ZIP5 <- as.character(train_test_char_df$VAR_0241)                                        
## Then DROP the Original ZIP column
train_test_char_df$VAR_0241 <- NULL


## Then lets drop these columns
## Drop the STATE as well as some other columns. 
## HYPOTHESIS is ZIP is a better indicator than STATE
## Also, there are 2 STATE columns, and most of the times they do NOT agree with each other for a customer ID
## Also, there is another Zip column VAR_0212; dropping that as well since no need
## VAR_0214 >> has values like HRE-Home Phone-4717 etc. - these are usually some SSN etc.
train_test_char_df$VAR_0200_CITY <- NULL
train_test_char_df$VAR_0214 <- NULL
train_test_char_df$VAR_0237_STATE <- NULL
train_test_char_df$VAR_0274_STATE <- NULL
## Also DROP the TITLE column - I do not have the juice to normalize it now
## TODO: If I have some time later, I will revert it back
train_test_char_df$VAR_0404_TITLE <- NULL
train_test_char_df$VAR_0044 <- NULL ## >>> ZERO VARIANCE here - All NA


## Get the Starting FACTOR LEVELS in this merged data
## var_0404_levels <- levels(train_test_df$VAR_0404_TITLE)
var_0493_levels <- levels(as.factor(train_test_char_df$VAR_0493_PROFESSION))


# ## Work on the DESIGNATION column >>> TODO : Come back to it later; this is very time consuming. >>>> Lets proceed with the modeling
# ## First convert into Character - for easy replace
# train_test_df$VAR_0404_TITLE <- as.character(train_test_df$VAR_0404_TITLE)
# 
# ## Then start replacing
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("^VICE", var_0404_levels)]] <- "VP"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("^DIRECTOR", var_0404_levels)]] <- "DIRECTOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CONSULTANT", var_0404_levels)]] <- "CONSULTANT"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("PROFESSOR", var_0404_levels)]] <- "PROFESSOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CHIEF", var_0404_levels)]] <- "CHIEF"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("DIRECTOR", var_0404_levels)]] <- "DIRECTOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("MANAGER", var_0404_levels)]] <- "MANAGER"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("SUPERVISOR", var_0404_levels)]] <- "SUPERVISOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("VICE PRESIDENT", var_0404_levels)]] <- "VP"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("PRESIDENT", var_0404_levels)]] <- "PRESIDENT"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("DESIGNER", var_0404_levels)]] <- "DESIGNER"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CLERK", var_0404_levels)]] <- "CLERK"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CHAIRMAN", var_0404_levels)]] <- "CHAIRMAN"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("TEACHER", var_0404_levels)]] <- "TEACHER"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CEO", var_0404_levels)]] <- "CHIEF"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("JOURNAL", var_0404_levels)]] <- "JOURNALIST"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("DOCTOR", var_0404_levels)]] <- "DOCTOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CFO", var_0404_levels)]] <- "CHIEF"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("WEB", var_0404_levels)]] <- "IT PERSONNEL"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("SOFTWARE", var_0404_levels)]] <- "IT PERSONNEL"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("SYSTEM", var_0404_levels)]] <- "IT PERSONNEL"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("EDITOR", var_0404_levels)]] <- "EDITOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("DBA", var_0404_levels)]] <- "IT PERSONNEL"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("RECEPTIONIST", var_0404_levels)]] <- "RECEPTIONIST"
# 
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("DEALER", var_0404_levels)]] <- "DEALER"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("COORDINATOR", var_0404_levels)]] <- "COORDINATOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("STAFF", var_0404_levels)]] <- "STAFF"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("PASTOR", var_0404_levels)]] <- "PASTOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("ACCOUNTANT", var_0404_levels)]] <- "ACCOUNTANT"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("ACCOUNTING", var_0404_levels)]] <- "ACCOUNTANT"
# 
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("EXECUTIVE", var_0404_levels)]] <- "EXECUTIVE"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("CONTRACTOR", var_0404_levels)]] <- "CONTRACTOR"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("AIDE", var_0404_levels)]] <- "AIDE"
# train_test_df$VAR_0404_TITLE[train_test_df$VAR_0404_TITLE %in% var_0404_levels[grepl("ENGINEER", var_0404_levels)]] <- "ENGINEER"
# 
# ## The final factor levels are... reduced from 3145 levels to 57 levels
# var_0404_levels <- levels(factor(train_test_df$VAR_0404_TITLE))
# ## Convert the column back t factor
# train_test_df$VAR_0404_TITLE <- factor(train_test_df$VAR_0404_TITLE)
# 

## Then start replacing
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NURSE", var_0493_levels)]] <- "NURSE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("^RN", var_0493_levels)]] <- "NURSE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NURSING", var_0493_levels)]] <- "NURSE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ACCOUNTANT", var_0493_levels)]] <- "ACCOUNTANT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("C.P.A.", var_0493_levels)]] <- "ACCOUNTANT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SOCIAL WORK", var_0493_levels)]] <- "SOCIAL WORKER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ENGINEER", var_0493_levels)]] <- "ENGINEER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("COUNSEL", var_0493_levels)]] <- "COUNSELOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TRAINER", var_0493_levels)]] <- "TRAINER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SPEECH", var_0493_levels)]] <- "SPEECH PATHOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ARCHITECT", var_0493_levels)]] <- "ARCHITECT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CPA", var_0493_levels)]] <- "ACCOUNTANT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NURSING ASSISTANT", var_0493_levels)]] <- "CLINICAL ASSISTANT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("COSMETO", var_0493_levels)]] <- "COSMETOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("VET", var_0493_levels)]] <- "VET"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHYSICIAN ASSISTANT", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHYSICIAN", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MEDICINE", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CONTRACTOR", var_0493_levels)]] <- "CONTRACTOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHYSICIAN", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NAIL", var_0493_levels)]] <- "MANICURIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("THERAPIST", var_0493_levels)]] <- "THERAPIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("THERAPY", var_0493_levels)]] <- "THERAPIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHARMACY TECHNICIAN", var_0493_levels)]] <- "CLINICAL TECH"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("EMERGENCY MEDICAL TECHNICIAN", var_0493_levels)]] <- "CLINICAL TECH"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CARDIAC TECHNICIAN", var_0493_levels)]] <- "CLINICAL TECH"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SMOG", var_0493_levels)]] <- "SMOG TECH"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PEDIATRICS", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PODIATRIST", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PODIATRY", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OBSTETRICS & GYNECOLOGY", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MD", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PARAMEDIC", var_0493_levels)]] <- "PARAMEDIC"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SURGICAL", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ONCOLOGY", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OSTEOPATH", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MANICURIST", var_0493_levels)]] <- "MANICURIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("WREST", var_0493_levels)]] <- "WRESTLER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SLP", var_0493_levels)]] <- "SPEECH PATHOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PERMANENT M.D.", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DENTAL", var_0493_levels)]] <- "DENTAL RELATED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TRAIN", var_0493_levels)]] <- "TRAINING RELATED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SALES", var_0493_levels)]] <- "SALESMAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OPERATOR", var_0493_levels)]] <- "OPERATOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DOCTOR", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("BROKER", var_0493_levels)]] <- "BROKER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("RESPIRATORY", var_0493_levels)]] <- "RESPIRATORY CARE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ATTORNEY", var_0493_levels)]] <- "LAWYER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("INTERN", var_0493_levels)]] <- "INTERN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("JOURNEYMAN", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("APPRAISER", var_0493_levels)]] <- "APPRAISER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("WATER", var_0493_levels)]] <- "WATER RELATED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("RADIO", var_0493_levels)]] <- "RADIOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("BARBER", var_0493_levels)]] <- "COSMETOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("REHAB", var_0493_levels)]] <- "REHAB PROVIDER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PSYCHO", var_0493_levels)]] <- "PSYCHOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("STUDENT", var_0493_levels)]] <- "STUDENT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PRIMARY CARE", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHYSICAL", var_0493_levels)]] <- "THERAPIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PLUMBER", var_0493_levels)]] <- "PLUMBER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("HAIR", var_0493_levels)]] <- "COSMETOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("AESTHETICIAN", var_0493_levels)]] <- "COSMETOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MANAGER", var_0493_levels)]] <- "MANAGER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("APPRENTICE", var_0493_levels)]] <- "APPRENTICE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("BOX", var_0493_levels)]] <- "BOXER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DENTIST", var_0493_levels)]] <- "DENTAL RELATED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GUARD", var_0493_levels)]] <- "SECURITY GUARD"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHARMA", var_0493_levels)]] <- "PHARMACIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SURVEY", var_0493_levels)]] <- "LAND SURVEYOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SUPERVISOR", var_0493_levels)]] <- "SUPERVISOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CHIRO", var_0493_levels)]] <- "CHIROPRACTOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DETECTIVE", var_0493_levels)]] <- "PRIVATE DETECTIVE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ELECT", var_0493_levels)]] <- "ELECTRICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ASBESTOS", var_0493_levels)]] <- "ASBESTOS RELATED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("AGENT", var_0493_levels)]] <- "AGENT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("WHOLE", var_0493_levels)]] <- "WHOLESALER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DIRECTOR", var_0493_levels)]] <- "FUNERAL DIRECTOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("INSTRUCTOR", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DIET", var_0493_levels)]] <- "DIETICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GEO", var_0493_levels)]] <- "GEOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("RESPR", var_0493_levels)]] <- "RESPIRATORY CARE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ANESTHESIOLOGY", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PSYCH", var_0493_levels)]] <- "MENTAL HEALTH PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("AUDIO", var_0493_levels)]] <- "AUDIOLOGIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("INDIVIDUAL", var_0493_levels)]] <- "SELF EMPLOYED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MENTAL", var_0493_levels)]] <- "MENTAL HEALTH PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OPT", var_0493_levels)]] <- "OPTICIAN/OPTOMETRIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ANIMAL", var_0493_levels)]] <- "VET"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MED", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DISP", var_0493_levels)]] <- "MISC. DISPENSER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("VOLT", var_0493_levels)]] <- "ELECTRICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("HEAT", var_0493_levels)]] <- "ELECTRICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("^AIR", var_0493_levels)]] <- "ELECTRICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("BATON", var_0493_levels)]] <- "SECURITY GUARD"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PHYS ", var_0493_levels)]] <- "MEDICAL PA"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FAMILY PRACTICE", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PEDORTHIST", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GENERAL SURGERY", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GERIATRICS", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MARTIAL", var_0493_levels)]] <- "MARTIAL ARTS"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CONTRACT", var_0493_levels)]] <- "CONTRACTOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GRAND", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("^DO", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PERIODONTIST", var_0493_levels)]] <- "DENTIST"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CARE", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("EMT", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CARDIAC", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OWNER", var_0493_levels)]] <- "SELF EMPLOYED"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("INSPECTOR", var_0493_levels)]] <- "MISC. INSPECTOR"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ACTIVE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("UNSPECIFIED SPECIALTY", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TREATMENT - CLASS I", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("RI", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("STATE LICENSED", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TECH", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OTHER SPECIALTY", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("REGISTERED APPLICATORS", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PROFESSIONAL PLANNER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LPN", var_0493_levels)]] <- "NURSE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NURSING", var_0493_levels)]] <- "NURSE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NEONATOLOGY", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("NON-RESIDENT ADJUSTER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OPERATOR", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LOCKSMITH", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LICENSED", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ADVANCED PRACTICE", var_0493_levels)]] <- "NURSE"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TRANSITIONAL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TRAVEL AGENCY", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("UTILITY FOREMAN", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TANNING FACILITY", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("TRADESMEN", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("SECOND CORNER PERSON", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ORTHOTIC FITTER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MORTUARY SCIENCE LICENSEE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FULL SPECIALIST", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("EXPLOSIVES USER HANDLER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("EMBALMER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LCADC", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MORTUARY PRACTITIONER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FORESTER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FACULTY/EDUCATOR", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GENERAL CERTIFICATE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LEGAL SERVICES", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("COMMERCIAL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED DESIGNATED REPRESENTATIVE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ERTIFIED POD X-RAY ASSISTANT", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED TO PURCHASE FREON", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("COMMERCIAL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CONTROLLED SUBSTANCE - 2", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FIELD REPRESENTATIVE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FUNERAL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LAND", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("WORKERS", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LIMITED CERTIFICATE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("REGISTERED ACCESSABILITY SPECIALISTS", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FIREARM PERMIT", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ASSIST SP", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED FOOD SAFETY MGR", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED RESIDENTIAL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CONTROLLED SUBSTANCE - 3", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DC", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FULL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OCCUPATIONAL", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("DIPENSER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("WIREMAN", var_0493_levels)]] <- "ELECTRICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("PERMANENT EMPLOYEE REGISTRATION CARD", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("HOMEMAKER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED GENERAL", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("AR", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("GENERAL PRACTICE", var_0493_levels)]] <- "PHYSICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("BEAUT", var_0493_levels)]] <- "BEAUTICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ESTHETICIAN", var_0493_levels)]] <- "BEAUTICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("FACIAL", var_0493_levels)]] <- "BEAUTICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("^HEALTH", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFICATE OF IDENTIFICATION", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CEMETERY", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("COSMET", var_0493_levels)]] <- "BEAUTICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("HOME", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("EMT", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("COMMUNITY", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("AUCTIONEER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ANNOUNCER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED PENNSYLVANIA EVALUATOR", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("HYGIENIST", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CERTIFIED SHORTHAND REPORTER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ADMINISTRATOR/ASSISTANT", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("APPRAISER", var_0493_levels)]] <- "BEAUTICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("BRAKE", var_0493_levels)]] <- "BEAUTICIAN"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("ACCOUNTANCY", var_0493_levels)]] <- "ACCOUNTANT"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CLINICAL LABORATORY PERSONNEL", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CLINICAL ASSISTANT", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("INSURANCE EDUCATION PROVIDERS", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LABORATORY ASSISTANT", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("LOAN OFFICER", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("OPERATING/RECOVERY ROOM", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("CONTROLLED SUBSTANCE - 1", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("RESIDENT & NON-RESIDENT INSURANCE PRODUCERS", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("REAL ESTATE CANDIDATE", var_0493_levels)]] <- "OTHER"
train_test_char_df$VAR_0493_PROFESSION[train_test_char_df$VAR_0493_PROFESSION %in% var_0493_levels[grepl("MEDICAL PA", var_0493_levels)]] <- "HEALTHCARE PROFESSIONAL"

## The final factor levels are... reduced from 797 levels to 57 levels
var_0493_levels <- levels(factor(train_test_char_df$VAR_0493_PROFESSION))



## There are different types of MISSING values in the data set
## "", NA, [], 99999.99, -1, etc.
train_test_char_df[train_test_char_df == ""] <- "NA"
train_test_char_df[train_test_char_df == "[]"] <- "NA"
train_test_char_df[train_test_char_df == "-1"] <- "NA"
train_test_char_df[is.na(train_test_char_df)] <- "NA"

## Convert the CHARACTER columns into FACTORS
train_test_char_df <- as.data.frame(unclass(train_test_char_df))


## Now put these FACTOR and NUMERIC data frames side by side
train_test_xformed_df <- cbind(train_test_char_df, train_test_numeric_nzv_xformed_df)

## Now separate the train and test data sets
train_proc_df <- train_test_xformed_df[which(train_test_xformed_df$source == "train"), ]
test_proc_df <- train_test_xformed_df[which(train_test_xformed_df$source == "test"), ]


## Then DROP the SOURCE column
train_proc_df$source <- NULL
test_proc_df$source <- NULL
## Save these data frames
saveRDS(object=train_proc_df, file="./data/train_proc_df.rds")
saveRDS(object=test_proc_df, file="./data/test_proc_df.rds")

## Oct 19, 2015 - 1245 pm; System crashed - starting again!!
## LESSON LEARNT - SAVE all intermediate data files
train_proc_df <- readRDS(file="./data/train_proc_df.rds")
test_proc_df <-  readRDS(file="./data/test_proc_df.rds")

## Add the target column back in the train data set
target_vec <- as.numeric(train_df$target)

## At this point, I will just go ahead and converting the factors into numeric labels and do XGBOOST or Random trees
## Convert this into a sparse matrix
## NOTE >>>>> The sparse_matrix conversion failed because of NAs data set
## I had to convert all NAs into 0 and it worked great >>> so point to NOTE
## The sparse matrix was producing LESSER rows than the original data set
## This is due to NAs - seems like there are still NA in the data frame
## TODO: Have to find a way to IMPUTE these - earlier attemps failed owing to size I guess
train_sparse_matrix <- sparse.model.matrix(ID~.-1, data=train_proc_df)
head(train_sparse_matrix)


test_sparse_matrix <- sparse.model.matrix(ID~.-1, data=test_proc_df)
head(test_sparse_matrix)

## Write the files in RDS format - so loading CSV is not needed
saveRDS(train_sparse_matrix, file = "./data/train_sparse_matrix.rds")
saveRDS(test_sparse_matrix, file = "./data/test_sparse_matrix.rds")





# Build the model
# 
# The code below is very usual. For more information, you can look at the documentation of 
# xgboost function (or at the vignette Xgboost presentation).
## With nround = 100 >> brought me up 1055 positions - at least i am not the second last guy on the list
bst <- xgboost(data = train_sparse_matrix, 
               label = target_vec, 
               nround = 500, 
               objective = "binary:logistic", 
               eval_metric="auc")

## The CV variant is ~ returns a Data Table
cv_results_dt <- xgb.cv(data = train_sparse_matrix, 
                 label = target_vec, 
                 nfold = 5, 
                 nround = 10, 
                 objective = "binary:logistic", 
                 eval_metric="auc")


## Run a round of prediction
pred <- predict(bst, test_sparse_matrix)


submission_file <- cbind(test_proc_df$ID, pred)
colnames(submission_file) <- c("ID", "target")
options("scipen"=100, "digits"=8)
write.csv(submission_file, "springleaf_response_prediction_v3.csv", row.names = FALSE, quote = FALSE)






