#Diabetes-Obesity, CVD Microsimulation (DOC-M) Model 
#The SSB tax project

#################
# 0.Preparation #
#################

#memory.size(max=T) # this is Windows-specific and will not work on the HPC running Linux

# 0.1 Install/Read R Packages

library(survey)
library(svMisc)
library(psych)
library(gdata)
library(dplyr)
library(data.table)   # needed for fread/fwrite
library(foreach)
library(doParallel)
library(abind)
#library(ggplot2)
library(dampack)
library(writexl)
library(triangle)
# remove all data from memory
rm(list = ls())


# Start the clock
ptm <- proc.time()

# 0.2 Create Functions 

# 0.2-1 To estimate gamma/beta/triangular parameters based on Mean and SD
estGammaParams <- function(mu, var) {
  beta <- var / mu
  alpha <- mu / beta
  return(params = list(alpha = alpha, beta = beta))
}

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


estTriangularParams <- function(mu, var) {
  mode <- mu - sqrt(6 * var) / 2
  c_value <- (mu - mode) * 2
  
  return(list(mode = mode, c_value = c_value))
}

rtriangle <- function(n, mode, c_value) {
  U <- runif(n)
  lower <- mode - c_value / 2
  upper <- mode + c_value / 2
  X <- ifelse(U <= (mode - lower) / c_value, 
              lower + sqrt(U * c_value * (mode - lower)), 
              upper - sqrt((1 - U) * c_value * (upper - mode)))
  return(X)
}

calc_nsims_rgamma <- function(n.sim, mu, se) {
  gamma_par <- estGammaParams(mu, se^2)
  return(rgamma(n.sim, gamma_par$alpha, 1/gamma_par$beta))
}

calc_nsims_rbeta <- function(n.sim, mu, se) {
  beta_par <- estBetaParams(mu, se^2)
  return(rbeta(n=n.sim, beta_par$alpha, beta_par$beta))
}

calc_nsims_rtriangle <- function(n.sim, mu, se) {
  triangular_par <- estTriangularParams(mu, se^2)
  return(rtriangle(n.sim, triangular_par$mode, triangular_par$c_value))
}

# 0.2-2 Converting multi-year risk (e.g., 10-year ASCVD risk) to annual probabilities
Multi_yr_Risk_to_annual_prob <- function(time, risk) {
  annual_rate <- -(1/time)*log(1-risk)
  annual_prob <- 1 - exp(-annual_rate)
}

# 0.3 Creating Working Directory 
current_directory <- getwd()
print(current_directory)
# setwd("C:/Users/ASUS/Desktop/data_v3")

# 0.4 Source other key scripts#########################################
source("02_Programs/CVD_risk_prediction.R")
source("02_Programs/DM_risk_prediction.R")
source("02_Programs/HCE_DM_inpatient estimator.R")
source("02_Programs/HCE_DM_outpatient estimator.R")
# source the script that defined the simulation function for running n.sim simulations #######################################################################TBD TBD TBD
source("02_Programs/sim_function_SSB.R")

# 0.5 Model settings (set manually or read from command line when submitted through cluster)
args <- commandArgs(trailingOnly = TRUE)  # get all arguments after script.R
# If no arguments are read from command line, set modeling choices manually. 
# Otherwise, read modeling choices from command line.
if (length(args) == 0) {
  seed <- 1234
  n.sim <- 2 #Number of probablistic samplings
  n.cycle <-100#Number of Cycle Length: How long does the model run (i.e., analytic time horizon)
  n.sample <- "ALL" #Number of individuals to be selected from the full sample; if full sample, enter "ALL"
  intervention <- "Policy"   ##$$specific for project 设置intervention
  n.loop=1  ##validation时设置为1
  #n.loop is the Number of replicates for each individual, each person is replicated for n.loop times and the outcomes are averaged across these replicates 
  #for the final results in each probablistic sampling. 
  bysub<-1 
} else {
  # expecting 5 arguments
  if (length(args) != 5) {
    stop("ERROR: Incorrect number of command line arguments", call. = FALSE)
  }
  seed <- as.numeric(args[1]) # extracting first arg as seed and attempt to cast to number
  n.sim <- as.numeric(args[2])
  n.cycle <- as.numeric(args[3])
  # Assume entire sample
  n.sample = "ALL"
  intervention=args[4]
  n.loop=as.numeric(args[5])
  # check that modeling choices were set
  if (is.na(seed)) {
    stop("ERROR: missing seed", call. = FALSE)
  }
  if (is.na(n.sim)) {
    stop("ERROR: missing n.sim", call. = FALSE)
  }
  if (is.na(n.cycle)) {
    stop("ERROR: missing n.cycle", call. = FALSE)
  }
}

output_pathRDS =  paste("03_Output/sim_out", n.sim, intervention, "SEED", seed,  ".rda", sep = "_")
output_pathCVS = "03_Output/Processed/final_output.csv"

set.seed(seed)

################################################################################################
# 1 Defining and Importing Necessary Impute parameters, creating n.sim random draws
################################################################################################

##1.0 Study specific settings: produce SSB project 

# Policy-effect size, costs, and discounting rate

#get n.sim random draws on policy effect size based mean and SE of the effect, and assumed distribution
#Policy
policy_effect_SSB_sim <- calc_nsims_rbeta(n.sim, mu = 0.30, se = 0.06) 
policy_effect_SSB <- policy_effect_SSB_sim

# Policy costs: scale to per year
c_policy = rnorm(n.sim, 39850686.57, 39850686.57*0.2)

# discounting rate
beta_cost <- beta_QALY <- 0.03 #Annual discounting rate of costs and QALYs

# 1.1 Read in master input file and cleaning

print('Importing data')

CHNS<- fread("01_Input/CHNS_Imp_final.csv", stringsAsFactors = TRUE, data.table = FALSE)

# 1.1.1 Select only necessary variables from master input file
variables <- c("AGE", "GENDER", "BMI", "SBP", "TC", 
               "SMOKING", "HDL_C", "DM", "WBC", "TG", "GLUCOSE","WEIGHT","HEIGHT","Subject_ID","WT_TOTAL","CVD_history")

CHNS <- CHNS[variables]

# 1.1.2 Select the target starting population to model (Age 35+ y) 
# 1.1.3 Define the analytic dataset to carry it forward

if (n.sample == "ALL"){
  data_for_analysis <- CHNS
} else {
  random_sample <- sample(1:nrow(CHNS),n.sample,replace=F)
  data_for_analysis <- CHNS[random_sample,]
}

data_for_analysis <- data_for_analysis[order(data_for_analysis$Subject_ID), ]

# 1.1.4 Additional DATA CLEANING
data_for_analysis$initial_H[data_for_analysis$CVD_history == 0 & data_for_analysis$DM == 0] <- "No CVD, No Diabetes"
data_for_analysis$initial_H[data_for_analysis$CVD_history == 0 & data_for_analysis$DM == 1] <- "No CVD, With Diabetes"


data_for_analysis$DEMO[data_for_analysis$GENDER == 1] <- "Male"
data_for_analysis$DEMO[data_for_analysis$GENDER == 2] <- "Female"

data_for_analysis$Age_cycle <- NA
data_for_analysis$BMI_cat[data_for_analysis$BMI < 18.5] <- "Underweight"
data_for_analysis$BMI_cat[data_for_analysis$BMI >= 18.5 & data_for_analysis$BMI < 25] <- "Normal"
data_for_analysis$BMI_cat[data_for_analysis$BMI >=25 & data_for_analysis$BMI < 30] <- "Overweight"
data_for_analysis$BMI_cat[data_for_analysis$BMI >= 30 & data_for_analysis$BMI < 35] <- "Obesity I"
data_for_analysis$BMI_cat[data_for_analysis$BMI >= 35 & data_for_analysis$BMI < 40] <- "Obesity II"
data_for_analysis$BMI_cat[data_for_analysis$BMI >= 40] <- "Obesity III"
data_for_analysis$Obesity[data_for_analysis$BMI < 30] <- 0
data_for_analysis$Obesity[data_for_analysis$BMI >= 30] <- 1

data_for_analysis$WT_TOTAL[data_for_analysis$AGE<=39 & data_for_analysis$GENDER==1] <- 633.48
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=40 & data_for_analysis$AGE<=44 & data_for_analysis$GENDER==1] <- 673.04
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=45 & data_for_analysis$AGE<=49 & data_for_analysis$GENDER==1] <- 549.96 
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=50 & data_for_analysis$AGE<=54 & data_for_analysis$GENDER==1] <- 511.34 
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=55 & data_for_analysis$AGE<=59 & data_for_analysis$GENDER==1] <- 477.01
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=60 & data_for_analysis$AGE<=64 & data_for_analysis$GENDER==1] <- 341.54
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=65 & data_for_analysis$AGE<=69 & data_for_analysis$GENDER==1] <- 244.30
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=70 & data_for_analysis$AGE<=74 & data_for_analysis$GENDER==1] <- 195.88
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=75 & data_for_analysis$AGE<=79 & data_for_analysis$GENDER==1] <- 132.49
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=80 & data_for_analysis$AGE<=84 & data_for_analysis$GENDER==1] <- 62.39
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=85 & data_for_analysis$AGE<=89 & data_for_analysis$GENDER==1] <- 23.75 
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=90 & data_for_analysis$AGE<=94 & data_for_analysis$GENDER==1] <- 4.35
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=95 & data_for_analysis$GENDER==1] <- 1


data_for_analysis$WT_TOTAL[data_for_analysis$AGE<=39 & data_for_analysis$GENDER==2] <- 644.11
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=40 & data_for_analysis$AGE<=44 & data_for_analysis$GENDER==2] <- 680.65
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=45 & data_for_analysis$AGE<=49 & data_for_analysis$GENDER==2] <- 562.88
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=50 & data_for_analysis$AGE<=54 & data_for_analysis$GENDER==2] <- 504.73
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=55 & data_for_analysis$AGE<=59 & data_for_analysis$GENDER==2] <- 476.17
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=60 & data_for_analysis$AGE<=64 & data_for_analysis$GENDER==2] <- 329.42
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=65 & data_for_analysis$AGE<=69 & data_for_analysis$GENDER==2] <- 239
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=70 & data_for_analysis$AGE<=74 & data_for_analysis$GENDER==2] <- 195.59
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=75 & data_for_analysis$AGE<=79 & data_for_analysis$GENDER==2] <- 138.93
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=80 & data_for_analysis$AGE<=84 & data_for_analysis$GENDER==2] <- 79.71
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=85 & data_for_analysis$AGE<=89 & data_for_analysis$GENDER==2] <- 33.95
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=90 & data_for_analysis$AGE<=94 & data_for_analysis$GENDER==2] <- 9.8
data_for_analysis$WT_TOTAL[data_for_analysis$AGE>=95 & data_for_analysis$GENDER==2] <- 2.71

# 1.1.5 DM risk adjustment for non-whites
data_for_analysis$risk_adjustment.DM <- ifelse(data_for_analysis$GENDER == 1, 0.14,0.11)


# 1.1.6 Other inputs

##Initial health states.
name.health.state <- c("No CVD, No Diabetes", "No CVD, With Diabetes", "First Stroke", "First CHD",
                       "CHD History, No Diabetes", "CHD History, With Diabetes","Stroke History, No Diabetes", "Stroke History, With Diabetes","Subsequent Stroke", "Subsequent CHD", "CHD+Stroke","Stroke+CHD","Stroke&CHD History, No Diabetes","Stroke&CHD History, With Diabetes","DM_Death","Stroke_Death", "CHD_Death", "Non_DM_Non_CVD_Death","Death") 
n.health.state <- length(name.health.state) # number of health state that individuals can transition over time
name.death.states <- c("DM_Death", "Stroke_Death", "CHD_Death", "Non_DM_Non_CVD_Death","Death")

# 1.2 SSB-disease etiologic effects data inputs : Age-specific relative risk estimates between SSB intake and disease outcomes
RR_SSB_disease <- fread("01_Input/SSB_RRs.csv", stringsAsFactors = TRUE, data.table = FALSE)
RR_SSB_disease <- subset(RR_SSB_disease, minage>=35)

# 1.2.1 Direct effect on disease
RR_SSB_chd <- subset(RR_SSB_disease, outcome == "CHD" & riskfactor == "SSB",select=c(minage, logRR.perchange, se.perchange))
RR_SSB_dm <- subset(RR_SSB_disease, outcome == "DM" & riskfactor == "SSB",select=c(minage, logRR.perchange, se.perchange))
RR_bmi_chd <- subset(RR_SSB_disease, outcome == "CHD" & riskfactor == "BMI",select=c(minage, logRR.perchange, se.perchange))
RR_bmi_stroke <- subset(RR_SSB_disease, outcome == "stroke" & riskfactor == "BMI",select=c(minage, logRR.perchange, se.perchange))
RR_bmi_dm <- subset(RR_SSB_disease, outcome == "DM" & riskfactor == "BMI",select=c(minage, logRR.perchange, se.perchange))

# Create n.sim random draws for logrr

random_logrr <- matrix(NA, nrow=nrow(RR_SSB_chd), ncol=n.sim)
rownames(random_logrr) <- c("35-44 y", "45-54 y", "55-64 y", "65-74 y","75+ y")
colnames(random_logrr) <- paste("simulation_", 1:n.sim, sep = " ")

random_logrr_SSB_chd <- random_logrr_SSB_dm <- random_logrr_bmi_chd <- random_logrr_bmi_stroke<- random_logrr_bmi_dm<-random_logrr

for (g in 1:nrow(random_logrr)) {
  random_logrr_SSB_chd[g,] <- rnorm(n.sim, RR_SSB_chd$logRR.perchange[g], RR_SSB_chd$se.perchange[g])
  random_logrr_SSB_dm[g,] <- rnorm(n.sim, RR_SSB_dm$logRR.perchange[g], RR_SSB_dm$se.perchange[g])
  random_logrr_bmi_chd[g,] <- rnorm(n.sim, RR_bmi_chd$logRR.perchange[g], RR_bmi_chd$se.perchange[g])
  random_logrr_bmi_stroke[g,] <- rnorm(n.sim, RR_bmi_stroke$logRR.perchange[g], RR_bmi_stroke$se.perchange[g])
  random_logrr_bmi_dm[g,] <- rnorm(n.sim, RR_bmi_dm$logRR.perchange[g], RR_bmi_dm$se.perchange[g])
}

# 1.2.2 Indirect effect on disease
# BMI change due to change in SSB intake  low=BMI<25
BMI_low_change_SSB = rnorm(n.sim, -0.1, 0.0255)
BMI_high_change_SSB = rnorm(n.sim, -0.23, 0.0459)

# 1.3 Health-state/Event specific mortality data  
Non_CVD_DM_mortality <- fread("01_Input/Non_CVD_Non_DM_Cause_Mortality(Annual probability).csv", stringsAsFactors = TRUE, data.table = FALSE)
Non_CVD_DM_mortality_Male <- t(mapply(calc_nsims_rbeta, n.sim, Non_CVD_DM_mortality$Male, Non_CVD_DM_mortality$Male_SE))
Non_CVD_DM_mortality_Female <- t(mapply(calc_nsims_rbeta, n.sim, Non_CVD_DM_mortality$Female, Non_CVD_DM_mortality$Female_SE))

stroke_mortality <- fread("01_Input/Stroke_Cause_Mortality(Annual probability).csv", stringsAsFactors = TRUE, data.table = FALSE)
stroke_mortality_Male <- t(mapply(calc_nsims_rbeta, n.sim, stroke_mortality$Male, stroke_mortality$Male_SE))
stroke_mortality_Female <- t(mapply(calc_nsims_rbeta, n.sim, stroke_mortality$Female, stroke_mortality$Female_SE))

CHD_mortality <- fread("01_Input/CHD_Cause_Mortality(Annual probability).csv", stringsAsFactors = TRUE, data.table = FALSE)
CHD_mortality_Male <- t(mapply(calc_nsims_rbeta, n.sim, CHD_mortality$Male, CHD_mortality$Male_SE))
CHD_mortality_Female <- t(mapply(calc_nsims_rbeta, n.sim, CHD_mortality$Female, CHD_mortality$Female_SE))

DM_mortality <- fread("01_Input/DM_Cause_Mortality(Annual probability).csv", stringsAsFactors = TRUE, data.table = FALSE)
DM_mortality_Male <- t(mapply(calc_nsims_rbeta, n.sim, DM_mortality$Male, DM_mortality$Male_SE))
DM_mortality_Female <- t(mapply(calc_nsims_rbeta, n.sim, DM_mortality$Female, DM_mortality$Female_SE))

# 1.4 Secular trends in major risk factors: estimated as average annual percent change
risk_factor_trend <- fread("01_Input/Risk_Factors_Trends_Summary.csv", stringsAsFactors = TRUE, data.table = FALSE)
risk_factor_trend_sim <- matrix(NA, nrow=nrow(risk_factor_trend), ncol=n.sim)
colnames(risk_factor_trend_sim) <- paste("simulation_", 1:n.sim, sep = " ")

risk_factor_trend_sim <- t(mapply(rnorm, n=n.sim, risk_factor_trend$APC, risk_factor_trend$APC_SE))

for (i in 1:nrow(risk_factor_trend)) {
  risk_factor_trend_sim[i,] <- as.matrix(rnorm(n.sim, risk_factor_trend$APC[i], risk_factor_trend$APC_SE[i]))
}

risk_factor_trend_sim <- cbind(risk_factor_trend, risk_factor_trend_sim)

#1.5 Healcare costs
#####CVD HCE#####

c_CVD_monitoring <- 16.58
c_CVD_monitoring_sim <- calc_nsims_rtriangle(n.sim,c_CVD_monitoring,0.5552)


c_CVD_annual_health_management <- 39.05
c_CVD_annual_health_management_sim <- calc_nsims_rtriangle(n.sim,c_CVD_annual_health_management,2.6393)

c_stroke_hospitalization <- 4207.29
c_stroke_hospitalization_sim <- calc_nsims_rgamma(n.sim,c_stroke_hospitalization,1505.05)

c_stroke_annual <-1289.75
c_stroke_annual_sim <- calc_nsims_rgamma(n.sim,c_stroke_annual,356.77)

c_CHD_hospitalization <- 7044.51
c_CHD_hospitalization_sim <- calc_nsims_rgamma(n.sim,c_CHD_hospitalization,1995.19 )

c_CHD_annual <- 1693.23
c_CHD_annual_sim <- calc_nsims_rgamma(n.sim,c_CHD_annual,112.83)

c_CHD_sim <- c_CVD_monitoring_sim + c_CVD_annual_health_management_sim + c_CHD_hospitalization_sim + c_CHD_annual_sim
c_stroke_sim <- c_CVD_monitoring_sim + c_CVD_annual_health_management_sim + c_stroke_hospitalization_sim + c_stroke_annual_sim

#####DM HCE#####
###########DM inpatient
##预测模型beta值的10次simulation(Model)
HCE_DM_inpatient_parameter_sim <- matrix(NA, nrow=nrow(HCE_DM_inpatient_parameters), ncol=n.sim)
rownames(HCE_DM_inpatient_parameter_sim) <- rownames(HCE_DM_inpatient_parameters)
colnames(HCE_DM_inpatient_parameter_sim) <- paste("simulation_inpatient_", 1:n.sim, sep = " ")

for (i in 1:nrow(HCE_DM_inpatient_parameters)) {
  HCE_DM_inpatient_parameter_sim[i,] <- as.matrix(rnorm(n.sim, HCE_DM_inpatient_parameters$Beta[i], HCE_DM_inpatient_parameters$SE[i]))
}


###########DM outpatient
##DM_Outpatient Model
HCE_DM_outpatient_parameter_sim <- matrix(NA, nrow=nrow(HCE_DM_outpatient_parameters), ncol=n.sim)
rownames(HCE_DM_outpatient_parameter_sim) <- rownames(HCE_DM_outpatient_parameters)
colnames(HCE_DM_outpatient_parameter_sim) <- paste("simulation_outpatient_", 1:n.sim, sep = " ")

for (i in 1:nrow(HCE_DM_outpatient_parameters)) {
  HCE_DM_outpatient_parameter_sim[i,] <- as.matrix(rnorm(n.sim, HCE_DM_outpatient_parameters$Beta[i], HCE_DM_outpatient_parameters$SE[i]))
}



#1.6 HRQOL
u_stroke <- 0.63
u_stroke_sim <- calc_nsims_rbeta(n.sim, u_stroke, 0.19)

u_post_stroke <- 0.65
u_post_stroke_sim <- calc_nsims_rbeta(n.sim, u_post_stroke, 0.09)

u_CHD <- 0.76
u_CHD_sim <- calc_nsims_rbeta(n.sim, u_CHD, 0.02)

u_post_CHD <- 0.88
u_post_CHD_sim <- calc_nsims_rbeta(n.sim, u_post_CHD, 0.04)

u_DM <- 0.92
u_DM_sim <- calc_nsims_rbeta(n.sim,u_DM,0.0041)

# 1.9 Productivity costs associated with CHD, stroke, DM
c_prod_stroke <- 21190.37
c_prod_stroke_sim <- calc_nsims_rgamma(n.sim,c_prod_stroke,c_prod_stroke*0.2)


c_prod_CHD <- 8463.185
c_prod_CHD_sim <- calc_nsims_rgamma(n.sim,c_prod_CHD,c_prod_CHD*0.2 )


c_prod_DM <- 2577.754
c_prod_DM_sim <- calc_nsims_rgamma(n.sim,c_prod_DM,c_prod_DM*0.2)

#################################################################################################################################
# 2 Estimating disease-specific risk, health-related quality of life (HrQOL), and healthcare expenditures (HCE) at the baseline #
#################################################################################################################################

# 2.1 Diabetes Risk Prediction (With BMI)
variable_for_raw.input <- names(data_for_analysis)
raw.input.data <- data_for_analysis
data_for_analysis$DM_prob <- calc_DM_risk(raw.input.data)

# 2.2 CVD Risk Prediction
raw.input.data <- data_for_analysis
data_for_analysis$CHD_prob <- calc_CHD_risk(raw.input.data)

raw.input.data <- data_for_analysis
data_for_analysis$Stroke_prob <- calc_Stroke_risk(raw.input.data)


#2.3 recurrent Stroke,CHD Risk Prediction
raw.input.data <- data_for_analysis
data_for_analysis$CHD_after_stroke_prob <- calc_nsims_rbeta(7327, 0.065, 0.005)
data_for_analysis$CHD_after_CHD_prob <- calc_nsims_rbeta(7327, 0.032, 0.003)
data_for_analysis$Stroke_after_stroke_prob <- calc_nsims_rbeta(7327, 0.177, 0.004)
data_for_analysis$Stroke_after_CHD_prob <- calc_nsims_rbeta(7327, 0.012, 0.002)

# 2.4 Individual HrQOL prediction
raw.input.data <- data_for_analysis
data_for_analysis$HRQOL_scores <- ifelse(data_for_analysis[,"DM"]==1 , u_DM_sim, 1)


# 2.5 Individual HCE prediction
##计算DM_inpatient的HCE
raw.input.data <- data_for_analysis
data_for_analysis$HCE_DM_inpatient_predict <- calc_HCE_DM_inpatient(raw.input.data, HCE_DM_inpatient_parameters[,1])

##计算DM_inpatient的HCE
raw.input.data <- data_for_analysis
data_for_analysis$HCE_DM_outpatient_predict <- calc_HCE_DM_outpatient(raw.input.data, HCE_DM_outpatient_parameters[,1])
#####总DM的费用
data_for_analysis$HCE_DM_predict <- data_for_analysis$HCE_DM_inpatient_predict + data_for_analysis$HCE_DM_outpatient_predict

data_for_analysis$HCE_predict <- ifelse(data_for_analysis[,"DM"]==1 , data_for_analysis$HCE_DM_predict, 0)


n.individual <- nrow(data_for_analysis)*n.loop # number of individuals in the model

#########################################################################
# 3 Run the simulation model - PLEASE DO NOT MODIFY UNLESS YOU KNOW WHAT YOU'RE DOING #
#########################################################################

print('running model')
model_start = proc.time()

# Detect system type
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
    if (os == "windows")
      os <- "windows"
  } else { ## if we still don't know
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Check if we are on Windows or Mac using our function.
cluster_type <- if (get_os() == "windows") {"PSOCK"} else {"FORK"}

no_cores <- detectCores() - 1
s = 1 # initialize iteration variable before forking
cl<-makeCluster(no_cores, type=cluster_type) # Make the cluster
# clusterEvalQ(cl)
registerDoParallel(cl)

acomb <- function(...) abind(..., along = 3)

# 3.1 run n.sim times of the simulation function in parallel processes, and then combine the results in sim_out, Run the function for each arm separately 
## The output is an 3-dimensional array (n.sample, variables, n.sim )
data_for_analysis $ initial_H[is.na(data_for_analysis$initial_H)] <- 1
data_for_analysis $Age_cycle <- data_for_analysis$AGE
sim_out_Policy <- foreach(s=1:n.sim, .combine = 'acomb', .verbose = T) %do% {
  set.seed(seed + n.cycle*s)
  run_sim(s, "Policy")
}


##save the output for policy arm
saveRDS(sim_out_Policy, file = paste("03_Output/sim_out_policy",  "SEED", seed, n.sim, n.cycle, n.loop, Sys.Date(), ".rda", sep = "_"))

sim_out_No_Policy <- foreach(s=1:n.sim, .combine = 'acomb', .verbose = T) %do% {
  set.seed(seed + n.cycle*s)
  run_sim(s, "No Policy")
}
##save the output for nonpolicy arm
saveRDS(sim_out_No_Policy, file = paste("03_Output/sim_out_nopolicy",  "SEED", seed, n.sim, n.cycle, n.loop, Sys.Date(), ".rda", sep = "_"))

stopCluster(cl)

print("Time to run model:")
proc.time() - model_start

#########################################################################
# 4 Summarize and save output                                           #
#########################################################################
print("Summarizing and saving output")
summ_start = proc.time()

#load the processing functions 
source("02_Programs/processing_function_SSB.R")

#process the data for subgroup analysis later 
data_for_analysis$age_cat[data_for_analysis$AGE<65] <-1
data_for_analysis$age_cat[data_for_analysis$AGE>64]<-2

data_for_analysis$gender_cat_100[data_for_analysis$AGE<100 & data_for_analysis$GENDER==1] <- 1
data_for_analysis$gender_cat_100[data_for_analysis$AGE<100 & data_for_analysis$GENDER==2] <- 2

# Create survey design object for full population

options(survey.lonely.psu = "adjust")  #表示处理调查的标准误差时如何处理单元PSU仅包含一个观察值的情况，adjust表示将采取适当的调整以处理这种情况。
design_sample<- svydesign(id=~Subject_ID, strata=~AGE + GENDER, weights=~WT_TOTAL, nest=TRUE, data=data_for_analysis)
# Pull out sample size and number of simulations from array subset
pop_count = as.numeric(dplyr::count(data_for_analysis, wt=WT_TOTAL))


################################################################################################
########### 4.1: summary and output for overall population #######################################

# Calculate difference between Policy and No Policy scenarios
diff_timehoriz = sim_out_Policy - sim_out_No_Policy
vars <- dimnames(diff_timehoriz)[[2]]
n.sample <- dim(diff_timehoriz)[1]
n.sim <- dim(diff_timehoriz)[3]

No_Policy_summary= calc_summary(sim_out_No_Policy, vars, n.sim, n.sample, design_sample)
Policy_summary = calc_summary(sim_out_Policy , vars, n.sim, n.sample, design_sample)
diff_summary1 = calc_summary(diff_timehoriz, vars, n.sim, n.sample, design_sample)

summary_table = full_join(No_Policy_summary, Policy_summary, by = "Outcome", suffix = c(".No_Policy", "")) %>%
  full_join(diff_summary1, by = "Outcome", suffix = c(".Policy", ".Diff1"))

write.csv(summary_table, paste("03_Output/summary_table_", seed, intervention, n.cycle,  "yrs_",  Sys.Date(),  ".csv", sep = ""))

pop_summary_table = summary_table
pop_summary_table[, -c(1)]=summary_table[, -c(1)]*pop_count
write.csv(pop_summary_table, paste("03_Output/pop_summary_table_", seed, intervention, n.cycle, "yrs_",  Sys.Date(), ".csv", sep = ""))


varkeep <- c("Total_cost_health", "Total_cost_societ", "effect_disc")
diff_allsim_summary = calc_allsim_summary(diff_timehoriz ,  varkeep, n.sim, design_sample )*pop_count
write.csv(diff_allsim_summary, paste("03_Output/pop_summary_allsim_", seed, intervention, n.cycle, "yrs_",Sys.Date(), ".csv", sep = ""))

# Calculate cost-effectiveness
cea_table1 = calc_ce(summary_table,"Total_cost_health") 
cea_table1$perspective<-"healthcare" 
cea_table2 = calc_ce(summary_table,"Total_cost_societ")
cea_table2$perspective<-"societ" 
cea_table=rbind(cea_table1, cea_table2)

write.csv(cea_table, paste("03_Output/cea_table_", seed, intervention, n.cycle, "yrs_", Sys.Date(), ".csv", sep = ""))

################################################################################################
########### 4.2: summary and output by population subgroups: age, sex#######################################

###summary by variables

byvars<-c("gender_cat_100")

summary_by_Policy<-calc_summary_by_Policy(byvars, sim_out_Policy_sub)
summary_by_No_Policy<-calc_summary_by_No_Policy(byvars, sim_out_No_Policy_sub)

write.csv(summary_by_Policy[1], paste("03_Output/summary_by_Policy_", seed, intervention, n.cycle,  "yrs_",  Sys.Date(),  ".csv", sep = ""))
write.csv(summary_by_No_Policy[1], paste("03_Output/summary_by_No_Policy_", seed, intervention, n.cycle,  "yrs_",  Sys.Date(),  ".csv", sep = ""))


varkeep <- c("Total_cost_health", "Total_cost_societ", "effect_disc")
allsimsby<-calc_allsim_by( byvars, varkeep, diff_timehoriz_sub)  
write.csv(allsimsby, paste("03_Output/pop_summary_allsim_bysub", seed, intervention, n.cycle, "yrs_",  Sys.Date(), ".csv", sep = ""))

print("Time to summarize and save output:")
proc.time() - summ_start

print("Total time:")
proc.time() - ptm

install.packages("xlsx")
library("xlsx")
write.xlsx(No_Policy_summary, "03_Output/No_Policy_summary.xlsx")

