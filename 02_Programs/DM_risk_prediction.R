##Annual Diabetes Incidence Score##
dm_risk_params <- read.csv("01_Input/DM Risk Model Parameters.csv", row.names = 1,encoding = 'UTF-8')
calc_DM_risk <- function(raw.input.data) {
  names <- c(rownames(dm_risk_params))
  
  ###Diabetes
  DM_risk_score <- matrix(nrow=nrow(raw.input.data), ncol=1)
  colnames(DM_risk_score) <- c("Diabete risk score")
  
  #Predictor 1: AGE
  raw.input.data$Age_cat1<-0
  raw.input.data$Age_cat1[raw.input.data$AGE>=35 & raw.input.data$AGE<=44]<-1
  
  raw.input.data$Age_cat2<-0
  raw.input.data$Age_cat2[raw.input.data$AGE>=45 & raw.input.data$AGE<=54]<-1
  
  raw.input.data$Age_cat3<-0
  raw.input.data$Age_cat3[raw.input.data$AGE>=55 & raw.input.data$AGE<=64]<-1
  
  raw.input.data$Age_cat4<-0
  raw.input.data$Age_cat4[raw.input.data$AGE>=65 & raw.input.data$AGE<=74]<-1
  
  raw.input.data$Age_cat5<-0
  raw.input.data$Age_cat5[raw.input.data$AGE>=75]<-1
  
  #Predictor 2: BMI
  raw.input.data$BMI_cat1<-0
  raw.input.data$BMI_cat1[raw.input.data$BMI<21]<-1
  
  raw.input.data$BMI_cat2<-0
  raw.input.data$BMI_cat2[raw.input.data$BMI>=21 & raw.input.data$BMI<22]<-1
  
  raw.input.data$BMI_cat3<-0
  raw.input.data$BMI_cat3[raw.input.data$BMI>=22 & raw.input.data$BMI<24]<-1
  
  raw.input.data$BMI_cat4<-0
  raw.input.data$BMI_cat4[raw.input.data$BMI>=24 & raw.input.data$BMI<26]<-1
  
  raw.input.data$BMI_cat5<-0
  raw.input.data$BMI_cat5[raw.input.data$BMI>=26]<-1
  
  #Predictor 3: WBC
  raw.input.data$WBC_cat1<-0
  raw.input.data$WBC_cat1[raw.input.data$WBC<4.9]<-1
  
  raw.input.data$WBC_cat2<-0
  raw.input.data$WBC_cat2[raw.input.data$WBC>=4.9 & raw.input.data$WBC<5.7]<-1
  
  raw.input.data$WBC_cat3<-0
  raw.input.data$WBC_cat3[raw.input.data$WBC>=5.7 & raw.input.data$WBC<6.4]<-1
  
  raw.input.data$WBC_cat4<-0
  raw.input.data$WBC_cat4[raw.input.data$WBC>=6.4 & raw.input.data$WBC<7.5]<-1
  
  raw.input.data$WBC_cat5<-0
  raw.input.data$WBC_cat5[raw.input.data$WBC>=7.5]<-1
  
  #Predictor 4: TG
  raw.input.data$TG_cat1<-0
  raw.input.data$TG_cat1[raw.input.data$TG<0.7]<-1
  
  raw.input.data$TG_cat2<-0
  raw.input.data$TG_cat2[raw.input.data$TG>=0.7 & raw.input.data$TG<0.92]<-1
  
  raw.input.data$TG_cat3<-0
  raw.input.data$TG_cat3[raw.input.data$TG>=0.92 & raw.input.data$TG<1.19]<-1
  
  raw.input.data$TG_cat4<-0
  raw.input.data$TG_cat4[raw.input.data$TG>=1.19 & raw.input.data$TG<1.71]<-1
  
  raw.input.data$TG_cat5<-0
  raw.input.data$TG_cat5[raw.input.data$TG>=1.71]<-1
  
  #Predictor 5: HDL_C
  raw.input.data$HDL_C_cat1<-0
  raw.input.data$HDL_C_cat1[raw.input.data$HDL_C<0.98]<-1
  
  raw.input.data$HDL_C_cat2<-0
  raw.input.data$HDL_C_cat2[raw.input.data$HDL_C>=0.98 & raw.input.data$HDL_C<1.13]<-1
  
  raw.input.data$HDL_C_cat3<-0
  raw.input.data$HDL_C_cat3[raw.input.data$HDL_C>=1.13 & raw.input.data$HDL_C<1.29]<-1
  
  raw.input.data$HDL_C_cat4<-0
  raw.input.data$HDL_C_cat4[raw.input.data$HDL_C>=1.29 & raw.input.data$HDL_C<1.49]<-1
  
  raw.input.data$HDL_C_cat5<-0
  raw.input.data$HDL_C_cat5[raw.input.data$HDL_C>=1.49]<-1
  
  #Predictor 6: GLUCOSE
  raw.input.data$GLUCOSE_cat1<-0
  raw.input.data$GLUCOSE_cat1[raw.input.data$GLUCOSE<5.16]<-1
  
  raw.input.data$GLUCOSE_cat2<-0
  raw.input.data$GLUCOSE_cat2[raw.input.data$GLUCOSE>=5.16 & raw.input.data$GLUCOSE<5.43]<-1
  
  raw.input.data$GLUCOSE_cat3<-0
  raw.input.data$GLUCOSE_cat3[raw.input.data$GLUCOSE>=5.43 & raw.input.data$GLUCOSE<5.71]<-1
  
  raw.input.data$GLUCOSE_cat4<-0
  raw.input.data$GLUCOSE_cat4[raw.input.data$GLUCOSE>=5.71 & raw.input.data$GLUCOSE<6.10]<-1
  
  raw.input.data$GLUCOSE_cat5<-0
  raw.input.data$GLUCOSE_cat5[raw.input.data$GLUCOSE>=6.10]<-1
  
  DM_risk_input<-raw.input.data[c("Subject_ID",names)]
  
  ############################################
  # If value is missing, place 0
  ############################################
  DM_risk_input[is.na(DM_risk_input)] <- 0
  
  ############################################
  # Diabetes Risk Calculation
  ############################################
  DM_risk_input$DM_scores<- as.matrix(DM_risk_input[2:31]) %*% as.matrix(dm_risk_params[1:30,1])
  
  DM_risk_input$DM_risk <- 0
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-15| DM_risk_input$DM_scores== -14] <- 0.05
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-13| DM_risk_input$DM_scores== -12] <- 0.06
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-11| DM_risk_input$DM_scores== -10] <- 0.07
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-9] <- 0.08
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-8| DM_risk_input$DM_scores== -7] <- 0.09
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-6] <- 0.10
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-5| DM_risk_input$DM_scores== -4] <- 0.11
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-3] <- 0.12
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-2] <- 0.13
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-1] <- 0.14
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==-0] <- 0.15
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==1] <- 0.16
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==2] <- 0.18
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==3] <- 0.19
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==4] <- 0.20
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==5] <- 0.22
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==6] <- 0.23
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==7] <- 0.25
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==8] <- 0.26
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==9] <- 0.28
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==10] <- 0.30
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==11] <- 0.32
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==12] <- 0.34
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==13] <- 0.36
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==14] <- 0.39
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==15] <- 0.41
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==16] <- 0.43
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==17] <- 0.46
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==18] <- 0.48
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==19] <- 0.51
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==20] <- 0.54
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==21] <- 0.57
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==22] <- 0.59
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==23] <- 0.62
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==24] <- 0.65
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==25] <- 0.68
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==26] <- 0.71
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==27] <- 0.73
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==28] <- 0.76
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==29] <- 0.79
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==30] <- 0.81
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==31] <- 0.84
  DM_risk_input$DM_risk[DM_risk_input$DM_scores==32] <- 0.86
  
  return (DM_risk_input$DM_risk)
}

