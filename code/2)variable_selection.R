library(data.table)
library(ggplot2)
library(dplyr)

#### GOALS OF FILE ######
# 1) Remove all columns that are not valid predictors of PRICE
# 2) Conform data to format usable for a regression 
#########################

### BE SURE TO RUN 
# - 1)clean_data.R to load a cleaned dataset








### Remove unusable columns
# -------------------------------------------
ncol(bnb_data) 
## 76 columns

# Drop all URLs - we don't plan on using these....
bnb_data[,grep("^.*url$", colnames(bnb_data)):= NULL]

# Drop all the redundant max/min nights variables - keep only the average
bnb_data[,grep("^.*nights$", colnames(bnb_data)):= NULL]

# Drop columns with only NAs
bnb_data <- bnb_data[, .SD, .SDcols = \(x) !all(is.na(x))]

# Drop any invariant columns (ie. only 1 value)
bnb_data <- bnb_data[, !sapply(bnb_data, FUN = function(x) length(unique(x))==1), with = FALSE]

# Drop other misc columns that have no clear use
bnb_data[,c("host_verifications", "neighbourhood"):=NULL]


ncol(bnb_data)
## 58 columns
