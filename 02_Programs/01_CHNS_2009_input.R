# Script to process CHNS data as model input file
# Cleans data
# Imputes data using multiple imputation
# Subsets population of interest
##### LIBRARIES ####################################################################################
library(dplyr)
library(purrr)
library(foreign)
library(Hmisc) # multiple imputation
library(survey)

##### GENERAL SETTINGS #############################################################################
setwd("D:/Research-SJTUH/CVD/code/CVD_model/01_Input")
# File paths for saving intermediate and final data sets
# raw_filepath = "./CHNS_raw.csv"
# preImp_filepath = "./CHNS_preImp.csv"
Imp_filepath = "./CHNS_Imp_0305.csv"

cycles <- c("2009")

##### PREPARING FOR IMPUTATION: RENAMING, RECODING, AND CREATING VARIABLES #########################

# Define additional variables (these variables will not be imputed, but will be used for pre-imputation population stats)
# Note the way ifelse works: if some criteria are negative and some are NA, it will return NA.
# If at least one criterium is positive, it will return positive (even if others are NA)
# Change to: if all are NA, return NA. If some are negative and some are NA, return negative.
# If some are positive and some are NA, return positive.
# To implement this change, I will sum across the criteria, removing NA values.
##### MULTIPLE IMPUTATION ##########################################################################

if(FALSE) {
  set.seed(1899)
  orig.data <- read.csv("CHNS_V3.csv")
  orig.data$Subject_ID <- c(1:nrow(orig.data))
  n.participants <- nrow(orig.data)
  
  num.imp <-10 # Set number of imputations
  num.knots <- 3 # set 0 for linear predictions of continous variables
  imputed.data <- aregImpute(~AGE+GENDER+BMI+HEIGHT+WEIGHT+TC+HDL_C+SBP+DBP+SMOKING+GLUCOSE+DM+WBC+TG
                             , orig.data, n.impute=num.imp, nk=num.knots, x=T)
  imputed.dataset <- data.frame()
  for(j in 1:num.imp) {  
    print(j)
    completed <- orig.data
    imputed <- impute.transcan(imputed.data, imputation = j, data = orig.data, list.out=TRUE, pr=FALSE, check = FALSE)
    completed[names(imputed)] <- imputed # Replace variables from the original dataset with imputed values
    imputed.dataset <- rbind(imputed.dataset, as.data.frame(completed))
  }
  
  
  # Save data set
  write.csv(imputed.dataset, Imp_filepath, row.names = F)
}


###debug

set.seed(1899)
orig.data <- read.csv("CHNS_V3.csv")
orig.data$Subject_ID <- c(1:nrow(orig.data))
n.participants <- nrow(orig.data)

num.imp <- 10 # Set number of imputations
num.knots <- 3 # set 0 for linear predictions of continous variables

# Initialize a list to store the imputed datasets
imputed.datasets <- list()

# Perform multiple imputation
for (j in 1:num.imp) {  
  imputed.data <- aregImpute(~AGE+GENDER+BMI+HEIGHT+WEIGHT+TC+HDL_C+SBP+DBP+SMOKING+GLUCOSE+DM+WBC+TG,
                             orig.data, n.impute=1, nk=num.knots, x=TRUE)
  
  completed <- orig.data
  imputed <- impute.transcan(imputed.data, imputation = 1, data = orig.data, list.out=TRUE, pr=FALSE, check = FALSE)
  completed[names(imputed)] <- imputed # Replace variables from the original dataset with imputed values
  
  # Store completed dataset in the list
  imputed.datasets[[j]] <- completed
}  

# Calculate the average of imputed datasets
average_dataset <- imputed.datasets[[1]]  # Initialize with the first dataset
for (j in 2:num.imp) {
  average_dataset[, -which(names(average_dataset) %in% c("Subject_ID"))] <- 
    average_dataset[, -which(names(average_dataset) %in% c("Subject_ID"))] + 
    imputed.datasets[[j]][, -which(names(imputed.datasets[[j]]) %in% c("Subject_ID"))]
}
average_dataset[, -which(names(average_dataset) %in% c("Subject_ID"))] <- 
  average_dataset[, -which(names(average_dataset) %in% c("Subject_ID"))] / num.imp

# Save average dataset
write.csv(average_dataset, Imp_filepath, row.names = FALSE)





