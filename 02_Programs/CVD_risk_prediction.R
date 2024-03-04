#First CVD Risk Calculator

######Annual CHD Risk Calculator

#Risk equation parameters
#setwd("C:/Users/ASUS/Desktop/data_v3")
#raw.input.data <- read.csv("01_Input/CHNS_Imp.csv")
CHD_risk_params <- read.csv("01_Input/CHD Risk Equation Parameters.csv", row.names = 1,encoding="UTF-8")

calc_CHD_risk <- function(raw.input.data){
  ##Input Data 新建一个行数=原始数据集，列为CHD参数变量的矩阵
  risk.input.data <- matrix(nrow=nrow(raw.input.data), ncol=nrow(CHD_risk_params))
  colnames(risk.input.data) <- rownames(CHD_risk_params)
  
  risk_score <- matrix(nrow=nrow(raw.input.data), ncol=2)
  colnames(risk_score) <- c("individual Sum","CHD_Risk")
  
  #数据赋值及单位转化
  risk.input.data[,1] <- raw.input.data[,"AGE"]/5
  risk.input.data[,2] <- raw.input.data[,"BMI"]/3
  risk.input.data[,3] <- raw.input.data[,"SBP"]/20
  risk.input.data[,4] <- raw.input.data[,"TC"]
  risk.input.data[,5] <- raw.input.data[,"SMOKING"]
  risk.input.data[,6] <- raw.input.data[,"HDL_C"]
  risk.input.data[,7] <- raw.input.data[,"DM"]
  
  #Risk_calculation
  
  #Part 1: Individual Sum (=coefficients*value=beta*X)  根据性别选择不同的beta值
  individual_sum <- ifelse(raw.input.data[,"GENDER"]==1, risk.input.data[,1:7] %*% CHD_risk_params[1:7,"Male"],
                           risk.input.data[,1:7] %*% CHD_risk_params[1:7,"Female"])
 
  
  #alpha值 for calibration
  alpha_Female <- 0
  alpha_Male <- 0
  raw.input.data$CHD_prob <- ifelse(raw.input.data[,"GENDER"]==1, exp(alpha_Male+individual_sum)/(1+exp(alpha_Male+individual_sum)),
                                    exp(alpha_Female+individual_sum)/(1+exp(alpha_Female+individual_sum)))
  return(raw.input.data$CHD_prob)
}


######Annual Stroke Risk Calculator

#Risk equation parameters
Stroke_risk_params <- read.csv("01_Input/Stroke Risk Equation Parameters.csv", row.names = 1,encoding="UTF-8")

calc_Stroke_risk <- function(raw.input.data){
  ##Input Data
  risk.input.data <- matrix(nrow=nrow(raw.input.data), ncol=nrow(Stroke_risk_params))
  colnames(risk.input.data) <- rownames(Stroke_risk_params)
  
  risk_score <- matrix(nrow=nrow(raw.input.data), ncol=2)
  colnames(risk_score) <- c("individual Sum","Stroke_Risk")
  
  #单位转化
  risk.input.data[,1] <- raw.input.data[,"AGE"]/5
  risk.input.data[,2] <- raw.input.data[,"BMI"]/3
  risk.input.data[,3] <- raw.input.data[,"SBP"]/20
  risk.input.data[,4] <- raw.input.data[,"TC"]
  risk.input.data[,5] <- raw.input.data[,"SMOKING"]
  risk.input.data[,6] <- raw.input.data[,"HDL_C"]
  risk.input.data[,7] <- raw.input.data[,"DM"]
  
  #Risk_calculation
  
  #Part 1: Individual Sum (=coefficients*value)  分sex
  individual_sum <- ifelse(raw.input.data[,"GENDER"]==1, risk.input.data[,1:7] %*% Stroke_risk_params[1:7,"Male"],
                           risk.input.data[,1:7] %*% Stroke_risk_params[1:7,"Female"])
  
  
  #alpha值
  alpha_Female <- 0
  alpha_Male <- 0
  raw.input.data$Stroke_prob <- ifelse(raw.input.data[,"GENDER"]==1, exp(alpha_Male+individual_sum)/(1+exp(alpha_Male+individual_sum)),
                                    exp(alpha_Female+individual_sum)/(1+exp(alpha_Female+individual_sum)))

  return(raw.input.data$Stroke_prob)
}

=======
#First CVD Risk Calculator

######Annual CHD Risk Calculator

#Risk equation parameters
#setwd("C:/Users/ASUS/Desktop/data_v3")
#raw.input.data <- read.csv("01_Input/CHNS_Imp.csv")
CHD_risk_params <- read.csv("01_Input/CHD Risk Equation Parameters.csv", row.names = 1,encoding="UTF-8")

calc_CHD_risk <- function(raw.input.data){
  ##Input Data 新建一个行数=原始数据集，列为CHD参数变量的矩阵
  risk.input.data <- matrix(nrow=nrow(raw.input.data), ncol=nrow(CHD_risk_params))
  colnames(risk.input.data) <- rownames(CHD_risk_params)
  
  risk_score <- matrix(nrow=nrow(raw.input.data), ncol=2)
  colnames(risk_score) <- c("individual Sum","CHD_Risk")
  
  #数据赋值及单位转化
  risk.input.data[,1] <- raw.input.data[,"AGE"]/5
  risk.input.data[,2] <- raw.input.data[,"BMI"]/3
  risk.input.data[,3] <- raw.input.data[,"SBP"]/20
  risk.input.data[,4] <- raw.input.data[,"TC"]
  risk.input.data[,5] <- raw.input.data[,"SMOKING"]
  risk.input.data[,6] <- raw.input.data[,"HDL_C"]
  risk.input.data[,7] <- raw.input.data[,"DM"]
  
  #Risk_calculation
  
  #Part 1: Individual Sum (=coefficients*value=beta*X)  根据性别选择不同的beta值
  individual_sum <- ifelse(raw.input.data[,"GENDER"]==1, risk.input.data[,1:7] %*% CHD_risk_params[1:7,"Male"],
                           risk.input.data[,1:7] %*% CHD_risk_params[1:7,"Female"])
 
  
  #alpha值 for calibration
  alpha_Female <- 0
  alpha_Male <- 0
  raw.input.data$CHD_prob <- ifelse(raw.input.data[,"GENDER"]==1, exp(alpha_Male+individual_sum)/(1+exp(alpha_Male+individual_sum)),
                                    exp(alpha_Female+individual_sum)/(1+exp(alpha_Female+individual_sum)))
  return(raw.input.data$CHD_prob)
}


######Annual Stroke Risk Calculator

#Risk equation parameters
Stroke_risk_params <- read.csv("01_Input/Stroke Risk Equation Parameters.csv", row.names = 1,encoding="UTF-8")

calc_Stroke_risk <- function(raw.input.data){
  ##Input Data
  risk.input.data <- matrix(nrow=nrow(raw.input.data), ncol=nrow(Stroke_risk_params))
  colnames(risk.input.data) <- rownames(Stroke_risk_params)
  
  risk_score <- matrix(nrow=nrow(raw.input.data), ncol=2)
  colnames(risk_score) <- c("individual Sum","Stroke_Risk")
  
  #单位转化
  risk.input.data[,1] <- raw.input.data[,"AGE"]/5
  risk.input.data[,2] <- raw.input.data[,"BMI"]/3
  risk.input.data[,3] <- raw.input.data[,"SBP"]/20
  risk.input.data[,4] <- raw.input.data[,"TC"]
  risk.input.data[,5] <- raw.input.data[,"SMOKING"]
  risk.input.data[,6] <- raw.input.data[,"HDL_C"]
  risk.input.data[,7] <- raw.input.data[,"DM"]
  
  #Risk_calculation
  
  #Part 1: Individual Sum (=coefficients*value)  分sex
  individual_sum <- ifelse(raw.input.data[,"GENDER"]==1, risk.input.data[,1:7] %*% Stroke_risk_params[1:7,"Male"],
                           risk.input.data[,1:7] %*% Stroke_risk_params[1:7,"Female"])
  
  
  #alpha值
  alpha_Female <- 0
  alpha_Male <- 0
  raw.input.data$Stroke_prob <- ifelse(raw.input.data[,"GENDER"]==1, exp(alpha_Male+individual_sum)/(1+exp(alpha_Male+individual_sum)),
                                    exp(alpha_Female+individual_sum)/(1+exp(alpha_Female+individual_sum)))

  return(raw.input.data$Stroke_prob)
}

>>>>>>> cc3bf8b6e197d3f07033b3655634f88599a0fc32
