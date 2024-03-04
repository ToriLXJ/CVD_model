##Predicting HCE_DM##
##############################################################
#2 Outpatient HCD
##############################################################
#setwd("C:/Users/ASUS/Desktop/data_v3")
HCE_DM_outpatient_parameters <- read.csv("01_Input/HCE_DM_Outpatient Estimation Parameters.csv", row.names = 1, encoding='UTF-8')

calc_HCE_DM_outpatient <- function(raw.input.data, params) {
  # Input: CHNS data for analysis and HCE parameters
  names <- c(rownames(HCE_DM_outpatient_parameters))
  
  #Gender
  raw.input.data$Male <- 0
  raw.input.data$Male[raw.input.data$GENDER==1] <- 1
  
  raw.input.data$Female <- 0
  raw.input.data$Female[raw.input.data$GENDER==2] <- 1
  
  #Age Categories
  raw.input.data$Age_under44 <- 0
  raw.input.data$Age_under44[raw.input.data$AGE<=44] <- 1
  
  raw.input.data$Age_45_54 <- 0
  raw.input.data$Age_45_54[raw.input.data$AGE>=45 & raw.input.data$AGE<=54] <- 1
  
  raw.input.data$Age_55_64 <- 0
  raw.input.data$Age_55_64[raw.input.data$AGE>=55 & raw.input.data$AGE<=64] <- 1
  
  raw.input.data$Age_65_74 <- 0
  raw.input.data$Age_65_74[raw.input.data$AGE>=65 & raw.input.data$AGE<=74] <- 1
  
  raw.input.data$Age_over75 <- 0
  raw.input.data$Age_over75[raw.input.data$AGE>=75] <- 1
  
  HCE_DM_outpatient_input<-raw.input.data[c("Subject_ID",names)]
  
  
  ############################################
  # If value is missing, place 0
  ############################################
  HCE_DM_outpatient_input[is.na(HCE_DM_outpatient_input)] <- 0
  
  HCE_DM_outpatient_baseline_mean <- 31.34 # for male,Age under44
  HCE_DM_outpatient_baseline_SE <- 6.268
  
  
  HCE_DM_outpatient_input$HCE_DM_outpatient_predict<- HCE_DM_outpatient_baseline_mean + (as.matrix(HCE_DM_outpatient_input[2:8]) %*% as.matrix(params))
  return(HCE_DM_outpatient_input$HCE_DM_outpatient_predict)
}
=======
##Predicting HCE_DM##
##############################################################
#2 Outpatient HCD
##############################################################
#setwd("C:/Users/ASUS/Desktop/data_v3")
HCE_DM_outpatient_parameters <- read.csv("01_Input/HCE_DM_Outpatient Estimation Parameters.csv", row.names = 1, encoding='UTF-8')

calc_HCE_DM_outpatient <- function(raw.input.data, params) {
  # Input: CHNS data for analysis and HCE parameters
  names <- c(rownames(HCE_DM_outpatient_parameters))
  
  #Gender
  raw.input.data$Male <- 0
  raw.input.data$Male[raw.input.data$GENDER==1] <- 1
  
  raw.input.data$Female <- 0
  raw.input.data$Female[raw.input.data$GENDER==2] <- 1
  
  #Age Categories
  raw.input.data$Age_under44 <- 0
  raw.input.data$Age_under44[raw.input.data$AGE<=44] <- 1
  
  raw.input.data$Age_45_54 <- 0
  raw.input.data$Age_45_54[raw.input.data$AGE>=45 & raw.input.data$AGE<=54] <- 1
  
  raw.input.data$Age_55_64 <- 0
  raw.input.data$Age_55_64[raw.input.data$AGE>=55 & raw.input.data$AGE<=64] <- 1
  
  raw.input.data$Age_65_74 <- 0
  raw.input.data$Age_65_74[raw.input.data$AGE>=65 & raw.input.data$AGE<=74] <- 1
  
  raw.input.data$Age_over75 <- 0
  raw.input.data$Age_over75[raw.input.data$AGE>=75] <- 1
  
  HCE_DM_outpatient_input<-raw.input.data[c("Subject_ID",names)]
  
  
  ############################################
  # If value is missing, place 0
  ############################################
  HCE_DM_outpatient_input[is.na(HCE_DM_outpatient_input)] <- 0
  
  HCE_DM_outpatient_baseline_mean <- 31.34 # for male,Age under44
  HCE_DM_outpatient_baseline_SE <- 6.268
  
  
  HCE_DM_outpatient_input$HCE_DM_outpatient_predict<- HCE_DM_outpatient_baseline_mean + (as.matrix(HCE_DM_outpatient_input[2:8]) %*% as.matrix(params))
  return(HCE_DM_outpatient_input$HCE_DM_outpatient_predict)
}
>>>>>>> cc3bf8b6e197d3f07033b3655634f88599a0fc32
