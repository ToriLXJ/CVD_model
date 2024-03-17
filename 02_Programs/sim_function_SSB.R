#Updating Disease history status
update_risk_factor <- function(sim_out_t, risk_factor, predictor, s) {
  # Updates risk factor values from time t to time t+1
  risk_factor_vals <- case_when(
    as.numeric(sim_out_t[,"Age_cycle"]) >= 35 & as.numeric(sim_out_t[,"Age_cycle"]) <= 39 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 1 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 40 & as.numeric(sim_out_t[,"Age_cycle"]) <= 44 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 2 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 45 & as.numeric(sim_out_t[,"Age_cycle"]) <= 49 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 3 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 50 & as.numeric(sim_out_t[,"Age_cycle"]) <= 54 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 4 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 55 & as.numeric(sim_out_t[,"Age_cycle"]) <= 59 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 5 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 60 & as.numeric(sim_out_t[,"Age_cycle"]) <= 64 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 6 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 65 & as.numeric(sim_out_t[,"Age_cycle"]) <= 69 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 7 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 70 & as.numeric(sim_out_t[,"Age_cycle"]) <= 74 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 8 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 75 & as.numeric(sim_out_t[,"Age_cycle"]) <= 79 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 9 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 80 & sim_out_t[,"DEMO"] == "Male" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 10 & risk_factor_trend_sim$DEMO == "Male"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 35 & as.numeric(sim_out_t[,"Age_cycle"]) <= 39 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 1 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 40 & as.numeric(sim_out_t[,"Age_cycle"]) <= 44 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 2 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 45 & as.numeric(sim_out_t[,"Age_cycle"]) <= 49 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 3 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 50 & as.numeric(sim_out_t[,"Age_cycle"]) <= 54 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 4 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 55 & as.numeric(sim_out_t[,"Age_cycle"]) <= 59 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 5 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 60 & as.numeric(sim_out_t[,"Age_cycle"]) <= 64 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 6 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 65 & as.numeric(sim_out_t[,"Age_cycle"]) <= 69 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 7 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 70 & as.numeric(sim_out_t[,"Age_cycle"]) <= 74 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 8 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 75 & as.numeric(sim_out_t[,"Age_cycle"]) <= 79 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 9 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    as.numeric(sim_out_t[,"Age_cycle"]) >= 80 & sim_out_t[,"DEMO"] == "Female" ~ as.numeric(sim_out_t[,risk_factor])*(1+(risk_factor_trend_sim[which(risk_factor_trend_sim$Predictors==predictor & risk_factor_trend_sim$Age_group == 10 & risk_factor_trend_sim$DEMO == "Female"),s+5]/100)),
    
    
    TRUE ~ as.numeric(NA))
  
  return(risk_factor_vals)
}



run_sim <- function(s, intervention) {
  
  if (intervention == "Policy") {
    policy_effect_SSB <- policy_effect_SSB_sim
  } else {
    policy_effect_SSB <- rep(0, n.sim)
  }
  
  # Runs single simulation
  # Returns array with variables for each individual after n.cycle
  
  # Define inpute variables
  Input_variables <- c("Subject_ID","DEMO","GENDER", "AGE", "BMI","SBP","TC","SMOKING" ,"HDL_C", "DM","WBC","TG", "GLUCOSE","Obesity", "WEIGHT", "HEIGHT","CVD_history",
                       "DM_prob", "CHD_prob", "Stroke_prob","CHD_after_stroke_prob","Stroke_after_stroke_prob","Stroke_after_CHD_prob","CHD_after_CHD_prob","HRQOL_scores", "HCE_predict")
  # Define output variables
  Out_variables<-c("Subject_ID","Incident DM","Incident CHD","Incident Stroke","Incident First CHD","Incident First Stroke","Incident Recurrent CHD","Incident Recurrent Stroke","Incident CHD+Stroke","Incident Stroke+CHD","Death","CHD_Death","Stroke_Death","DM_Death","Non_DM_Non_CVD_Death","Life Years","effect_disc", "HCE_disc", "Prod_cost", "Prod_cost_disc","Policy_cost", "Total_cost_health","Total_cost_societ" )
  #Set up initial values
  #replicate each individual in the data for n.loop times, to minimize stochastic error
  
  sim_out_t<-do.call("rbind", replicate(n.loop, data_for_analysis[,Input_variables], simplify = FALSE))
  sim_out_t$state=rep(data_for_analysis$initial_H, n.loop)
  sim_out_t[,"Age_cycle"]=sim_out_t[,"AGE"]
  sim_out_t[,"BaseBMI"]=sim_out_t[,"BMI"]
  sim_out_t[,"HCE_disc"] <- sim_out_t$HCE_predict
  sim_out_t[,"effect_disc"] <- sim_out_t$HRQOL_scores
  
  #Assume productivity costs to be 0 at baseline for both intervention and control group, since no difference has occur 
  sim_out_t[,"Prod_cost"] <-0
  sim_out_t[,"Prod_cost_disc"] <-0
  #Assume productivity costs to be 0 at baseline for both intervention and control group, since no difference has occur 
  sim_out_t[,"Policy_cost"] <-0
  sim_out_t[,"Policy_cost_disc"] <- 0
  sim_out_t[,"Life Years"]<- 0
  sim_out_t[,"Incident DM"]<- 0
  sim_out_t[,"Incident First CHD"]<-0
  sim_out_t[,"Incident First Stroke"]<-0
  sim_out_t[,"Incident Recurrent CHD"]<-0
  sim_out_t[,"Incident Recurrent Stroke"]<-0
  sim_out_t[,"Incident CHD+Stroke"] <- 0
  sim_out_t[,"Incident Stroke+CHD"] <- 0
  
  sim_out_t[,"Incident CHD"]<-0
  sim_out_t[,"Incident Stroke"]<-0
  sim_out_t[,"CHD_Death"]<- 0
  sim_out_t[,"Stroke_Death"]<- 0
  sim_out_t[,"DM_Death"]<- 0
  sim_out_t[,"Non_DM_Non_CVD_Death"]<- 0
  sim_out_t[,"Death"] <- 0
  
  
  ########################################
  ######2. run model for n.cycle###############
  ########################################  
  
  
  for (t in 1:n.cycle) {
    print(t)
    ###################################################################################  
    # 2.1 Updating Risk Factors over time: Applying secular trends based on age, gender, R/E
    ###################################################################################  
    
    #Time-varying data inputs
    sim_out_t[,"Age_cycle"] <- sim_out_t[,"AGE"] + t
    # Updating Risk Factors over time: Applying secular trends based on age, gender
    
    ##$$Policy specific settings: produce project only
    # Adjust BMI based on change in SSB intake 
    # Add sensivity scenario: change the BMI pathway. Change only apply once
    #提取BMI列
    BMI <- sim_out_t[,"BMI"]
    # 对BMI列进行条件判断和更新
    sim_out_t[,"BMI"] <- ifelse(intervention == "Policy" & BMI < 25, 
                                BMI <- BMI + t * BMI_low_change_SSB[s] * policy_effect_SSB_sim[s],
                                ifelse(intervention == "Policy" & BMI >= 25,
                                       BMI <- BMI + t * BMI_high_change_SSB[s] * policy_effect_SSB_sim[s],
                                       BMI))
    
    # 更新风险因子
    sim_out_t[,"BMI"] <- update_risk_factor(sim_out_t, "BMI", "BMI", s)
    ##把BMI控制在12和70之间
    sim_out_t[,"BMI"] <- ifelse(sim_out_t[,"BMI"] < 12, 12,
                                ifelse(sim_out_t[,"BMI"]>70, 70, sim_out_t[,"BMI"]))
    sim_out_t[,"SBP"] <- update_risk_factor(sim_out_t, "SBP", "SBP", s)
    sim_out_t[,"TC"] <- update_risk_factor(sim_out_t, "TC", "TC", s)
    sim_out_t[,"SMOKING"] <- update_risk_factor(sim_out_t, "SMOKING", "SMOKING", s)
    sim_out_t[,"HDL_C"] <- update_risk_factor(sim_out_t, "HDL_C", "HDL_C", s)
    sim_out_t[,"DM"] <- update_risk_factor(sim_out_t, "DM", "DM", s)
    sim_out_t[,"WBC"] <- update_risk_factor(sim_out_t, "WBC", "WBC", s)
    sim_out_t[,"TG"] <- update_risk_factor(sim_out_t, "TG", "TG", s)
    sim_out_t[,"GLUCOSE"] <- update_risk_factor(sim_out_t, "GLUCOSE", "GLUCOSE", s)
    ###################################################################################  
    # 2.2 Updating RRs of changing risk factors
    ###################################################################################  
    # SSB-CHD/DM
    RR_diff_SSB_chd <- case_when(
      sim_out_t[,"Age_cycle"] < 45 ~ exp(random_logrr_SSB_chd[1,s]*policy_effect_SSB[s]),
      sim_out_t[,"Age_cycle"] < 55 ~ exp(random_logrr_SSB_chd[2,s]*policy_effect_SSB[s]),
      sim_out_t[,"Age_cycle"] < 65 ~ exp(random_logrr_SSB_chd[3,s]*policy_effect_SSB[s]),
      sim_out_t[,"Age_cycle"] < 75 ~ exp(random_logrr_SSB_chd[4,s]*policy_effect_SSB[s]),
      TRUE ~ exp(random_logrr_SSB_chd[5,s]*policy_effect_SSB[s])
    )
    
    RR_diff_SSB_dm <- case_when(
      sim_out_t[,"Age_cycle"] < 45 ~ exp(random_logrr_SSB_dm[1,s]*policy_effect_SSB[s]),
      sim_out_t[,"Age_cycle"] < 55 ~ exp(random_logrr_SSB_dm[2,s]*policy_effect_SSB[s]),
      sim_out_t[,"Age_cycle"] < 65 ~ exp(random_logrr_SSB_dm[3,s]*policy_effect_SSB[s]),
      sim_out_t[,"Age_cycle"] < 75 ~ exp(random_logrr_SSB_dm[4,s]*policy_effect_SSB[s]),
      TRUE ~ exp(random_logrr_SSB_dm[5,s]*policy_effect_SSB[s])
    )
    
    # BMI-CHD/stroke/DM
    RR_diff_bmi_chd <- case_when(
      sim_out_t[,"Age_cycle"] < 45 ~ exp(random_logrr_bmi_chd[1,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5), 
      sim_out_t[,"Age_cycle"] < 55 ~ exp(random_logrr_bmi_chd[2,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),
      sim_out_t[,"Age_cycle"] < 65 ~ exp(random_logrr_bmi_chd[3,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),
      sim_out_t[,"Age_cycle"] < 75 ~ exp(random_logrr_bmi_chd[4,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),  
      TRUE ~ exp(random_logrr_bmi_chd[5,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5)
    )
    
    RR_diff_bmi_stroke <- case_when(
      sim_out_t[,"Age_cycle"] < 45 ~ exp(random_logrr_bmi_stroke[1,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5), 
      sim_out_t[,"Age_cycle"] < 55 ~ exp(random_logrr_bmi_stroke[2,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),
      sim_out_t[,"Age_cycle"] < 65 ~ exp(random_logrr_bmi_stroke[3,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),
      sim_out_t[,"Age_cycle"] < 75 ~ exp(random_logrr_bmi_stroke[4,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),  
      TRUE ~ exp(random_logrr_bmi_stroke[5,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5)
      
    )
    
    RR_diff_bmi_dm <- case_when(
      sim_out_t[,"Age_cycle"] < 45 ~ exp(random_logrr_bmi_dm[1,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5), 
      sim_out_t[,"Age_cycle"] < 55 ~ exp(random_logrr_bmi_dm[2,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),
      sim_out_t[,"Age_cycle"] < 65 ~ exp(random_logrr_bmi_dm[3,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),
      sim_out_t[,"Age_cycle"] < 75 ~ exp(random_logrr_bmi_dm[4,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5),  
      TRUE ~ exp(random_logrr_bmi_dm[5,s]*(sim_out_t[,"BMI"] - sim_out_t[,"BaseBMI"])/5)
    )
    
    # Combine effects by multiplying RRs
    RR_diff_total_CHD <- RR_diff_bmi_chd*RR_diff_SSB_chd
    RR_diff_total_Stroke <- RR_diff_bmi_stroke
    RR_diff_total_DM <- RR_diff_bmi_dm*RR_diff_SSB_dm
    
    ###################################################################################  
    # 2.3 Updating disease risks###########
    ###################################################################################  
    ##加上GENDER
    raw.input.data <- sim_out_t
    
    raw.input.data$AGE<-raw.input.data$Age_cycle
    
    sim_out_t[,"CHD_prob"] <- calc_CHD_risk(raw.input.data)
    
    sim_out_t[,"Stroke_prob"] <- calc_Stroke_risk(raw.input.data)
    
    sim_out_t[,"DM_prob"] <- calc_DM_risk(raw.input.data)
    
    sim_out_t[,"CHD_after_stroke_prob"] <- calc_nsims_rbeta(n.individual,0.065, 0.005)
    
    sim_out_t[,"Stroke_after_stroke_prob"] <- calc_nsims_rbeta(n.individual, 0.177, 0.004)
    
    sim_out_t[,"Stroke_after_CHD_prob"] <- calc_nsims_rbeta(n.individual, 0.012, 0.002)
    
    sim_out_t[,"CHD_after_CHD_prob"] <- calc_nsims_rbeta(n.individual, 0.032, 0.003)
    
    
    #Defining transition probabilities
    # Subtract from age because indexing starts at age 34
    ###################################################################################  
    # 2.4 Updating transition probabilities###########
    ###################################################################################   
    p.death.DM <- case_when(
      sim_out_t[,"DEMO"] == "Male" ~ DM_mortality_Male[sim_out_t[,"Age_cycle"]-34,s],
      sim_out_t[,"DEMO"] == "Female" ~ DM_mortality_Female[sim_out_t[,"Age_cycle"]-34,s],
    )
    
    p.death.CHD <- case_when(
      sim_out_t[,"DEMO"] == "Male" ~ CHD_mortality_Male[sim_out_t[,"Age_cycle"]-34,s],
      sim_out_t[,"DEMO"] == "Female" ~ CHD_mortality_Female[sim_out_t[,"Age_cycle"]-34,s],
    )
    
    p.death.Stroke <- case_when(
      sim_out_t[,"DEMO"] == "Male" ~ stroke_mortality_Male[sim_out_t[,"Age_cycle"]-34,s],
      sim_out_t[,"DEMO"] == "Female" ~ stroke_mortality_Female[sim_out_t[,"Age_cycle"]-34,s],
    )
    
    p.death <- case_when(
      sim_out_t[,"DEMO"] == "Male" ~ Non_CVD_DM_mortality_Male[sim_out_t[,"Age_cycle"]-34,s],
      sim_out_t[,"DEMO"] == "Female" ~ Non_CVD_DM_mortality_Female[sim_out_t[,"Age_cycle"]-34,s],
    )
    
    
    #Markov State #1: "No CVD, No Diabetes"
    p.H.2.death <- p.death
    p.H.2.DM <- RR_diff_total_DM*as.numeric(sim_out_t[,"DM_prob"])*rep(data_for_analysis$risk_adjustment.DM, n.loop)  ##1个  -RR；看原模型的概率值对比
    p.H.2.initial_CHD <- RR_diff_total_CHD*as.numeric(sim_out_t[,"CHD_prob"])
    p.H.2.initial_Stroke <- RR_diff_total_Stroke*as.numeric(sim_out_t[,"Stroke_prob"])
    
    # 如果和大于1
    if (any((p.H.2.death+p.H.2.DM + p.H.2.initial_CHD + p.H.2.initial_Stroke) > 1)) {
      # 概率更新
      p.H.2.death <- p.H.2.death / (p.H.2.death+p.H.2.DM + p.H.2.initial_CHD + p.H.2.initial_Stroke)
      p.H.2.DM <- p.H.2.DM / (p.H.2.death+p.H.2.DM + p.H.2.initial_CHD + p.H.2.initial_Stroke)
      p.H.2.initial_CHD <- p.H.2.initial_CHD / (p.H.2.death+p.H.2.DM + p.H.2.initial_CHD + p.H.2.initial_Stroke)
      p.H.2.initial_Stroke <- p.H.2.initial_Stroke / (p.H.2.death+p.H.2.DM + p.H.2.initial_CHD + p.H.2.initial_Stroke)
    } else {
      # 否则与原始值相同
      p.H.2.death <- p.H.2.death
      p.H.2.DM <- p.H.2.DM
      p.H.2.initial_CHD <- p.H.2.initial_CHD
      p.H.2.initial_Stroke <- p.H.2.initial_Stroke
    }
    
    p.H.2.H <- 1 - (p.H.2.death+p.H.2.DM + p.H.2.initial_CHD + p.H.2.initial_Stroke)
    
    #Markov State #2: "No CVD, With Diabetes"
    p.DM.2.death <- (1-exp(-(p.death+p.death.DM)))
    p.DM.2.initial_CHD <- RR_diff_total_CHD*as.numeric(sim_out_t[,"CHD_prob"])
    p.DM.2.initial_Stroke <- RR_diff_total_Stroke*as.numeric(sim_out_t[,"Stroke_prob"])
    
    # 如果和大于1
    if (any((p.DM.2.initial_CHD + p.DM.2.initial_Stroke + p.DM.2.death) > 1)) {
      # 概率更新
      p.DM.2.death <- p.DM.2.death / (p.DM.2.initial_CHD + p.DM.2.initial_Stroke + p.DM.2.death)
      p.DM.2.initial_CHD <- p.DM.2.initial_CHD / (p.DM.2.initial_CHD + p.DM.2.initial_Stroke + p.DM.2.death)
      p.DM.2.initial_Stroke <- p.DM.2.initial_Stroke / (p.DM.2.initial_CHD + p.DM.2.initial_Stroke + p.DM.2.death)
    } else {
      # 否则与原始值相同
      p.DM.2.death <- p.DM.2.death
      p.DM.2.initial_CHD <- p.DM.2.initial_CHD
      p.DM.2.initial_Stroke <- p.DM.2.initial_Stroke
    }
    p.DM.2.DM <- 1 - (p.DM.2.initial_CHD + p.DM.2.initial_Stroke + p.DM.2.death)
    
    #Markov State #3: "First Stroke"
    p.initial_Stroke.2.death <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, (1-exp(-(p.death+p.death.Stroke+p.death.DM))), (1-exp(-(p.death+p.death.Stroke)))))
    p.initial_Stroke.2.Stroke_No_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 0, 1 - p.initial_Stroke.2.death))
    p.initial_Stroke.2.Stroke_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 1 - p.initial_Stroke.2.death, 0))
    
    #Markov State #4: "First CHD" 
    p.initial_CHD.2.death <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, (1-exp(-(p.death+p.death.CHD+p.death.DM))), (1-exp(-(p.death+p.death.CHD)))))
    p.initial_CHD.2.CHD_No_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 0, 1 - p.initial_CHD.2.death))
    p.initial_CHD.2.CHD_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 1 - p.initial_CHD.2.death, 0))
    
    #Markov State #5: "Stroke History, No Diabetes"
    p.Stroke.2.DM <- RR_diff_total_DM*as.numeric(sim_out_t[,"DM_prob"])*rep(data_for_analysis$risk_adjustment.DM, n.loop)
    p.Stroke.2.Stroke_CHD <- RR_diff_total_CHD*as.numeric(sim_out_t[,"CHD_after_stroke_prob"])
    p.Stroke.2.sub_Stroke <- RR_diff_total_Stroke*as.numeric(sim_out_t[,"Stroke_after_stroke_prob"])
    p.Stroke.2.death <- (1-exp(-(p.death+p.death.Stroke)))
    
    # 如果和大于1
    if (any((p.Stroke.2.DM + p.Stroke.2.Stroke_CHD + p.Stroke.2.sub_Stroke + p.Stroke.2.death) > 1)) {
      # 概率更新
      p.Stroke.2.DM <- p.Stroke.2.DM / (p.Stroke.2.DM + p.Stroke.2.Stroke_CHD + p.Stroke.2.sub_Stroke + p.Stroke.2.death)
      p.Stroke.2.Stroke_CHD <- p.Stroke.2.Stroke_CHD / (p.Stroke.2.DM + p.Stroke.2.Stroke_CHD + p.Stroke.2.sub_Stroke + p.Stroke.2.death)
      p.Stroke.2.sub_Stroke <- p.Stroke.2.sub_Stroke / (p.Stroke.2.DM + p.Stroke.2.Stroke_CHD + p.Stroke.2.sub_Stroke + p.Stroke.2.death)
      p.Stroke.2.death <- p.Stroke.2.death / (p.Stroke.2.DM + p.Stroke.2.Stroke_CHD + p.Stroke.2.sub_Stroke + p.Stroke.2.death)
    } else {
      # 否则与原始值相同
      p.Stroke.2.DM <- p.Stroke.2.DM
      p.Stroke.2.Stroke_CHD <- p.Stroke.2.Stroke_CHD
      p.Stroke.2.sub_Stroke <- p.Stroke.2.sub_Stroke
      p.Stroke.2.death <- p.Stroke.2.death
    }
    
    p.Stroke.2.Stroke <- 1 - (p.Stroke.2.DM + p.Stroke.2.Stroke_CHD + p.Stroke.2.sub_Stroke + p.Stroke.2.death)
    
    
    #Markov State #6: "CHD History, No Diabetes"
    p.CHD.2.DM <- RR_diff_total_DM*as.numeric(sim_out_t[,"DM_prob"])*rep(data_for_analysis$risk_adjustment.DM, n.loop)
    p.CHD.2.sub_CHD <- RR_diff_total_CHD*as.numeric(sim_out_t[,"CHD_after_CHD_prob"])
    p.CHD.2.CHD_Stroke <- RR_diff_total_Stroke*as.numeric(sim_out_t[,"Stroke_after_CHD_prob"])
    p.CHD.2.death <- (1-exp(-(p.death+p.death.CHD)))
    
    # 如果和大于1
    if (any((p.CHD.2.DM + p.CHD.2.sub_CHD + p.CHD.2.CHD_Stroke + p.CHD.2.death) > 1)) {
      # 概率更新
      p.CHD.2.DM <- p.CHD.2.DM / (p.CHD.2.DM + p.CHD.2.sub_CHD + p.CHD.2.CHD_Stroke + p.CHD.2.death)
      p.CHD.2.sub_CHD <- p.CHD.2.sub_CHD / (p.CHD.2.DM + p.CHD.2.sub_CHD + p.CHD.2.CHD_Stroke + p.CHD.2.death)
      p.CHD.2.CHD_Stroke <- p.CHD.2.CHD_Stroke / (p.CHD.2.DM + p.CHD.2.sub_CHD + p.CHD.2.CHD_Stroke + p.CHD.2.death)
      p.CHD.2.death <- p.CHD.2.death / (p.CHD.2.DM + p.CHD.2.sub_CHD + p.CHD.2.CHD_Stroke + p.CHD.2.death)
    } else {
      # 否则与原始值相同
      p.CHD.2.DM <- p.CHD.2.DM
      p.CHD.2.sub_CHD <- p.CHD.2.sub_CHD
      p.CHD.2.CHD_Stroke <- p.CHD.2.CHD_Stroke
      p.CHD.2.death <- p.CHD.2.death
    }
    
    p.CHD.2.CHD <- 1 - (p.CHD.2.DM + p.CHD.2.sub_CHD + p.CHD.2.CHD_Stroke + p.CHD.2.death)
    
    #Markov State #7: "Stroke History, With Diabetes"
    p.Stroke_DM.2.Stroke_CHD <- RR_diff_total_CHD*as.numeric(sim_out_t[,"CHD_after_stroke_prob"])
    p.Stroke_DM.2.sub_Stroke <- RR_diff_total_Stroke*as.numeric(sim_out_t[,"Stroke_after_stroke_prob"])
    p.Stroke_DM.2.death <- (1-exp(-(p.death+p.death.Stroke+p.death.DM)))
    
    # 如果和大于1
    if (any((p.Stroke_DM.2.death + p.Stroke_DM.2.Stroke_CHD + p.Stroke_DM.2.sub_Stroke) > 1)) {
      # 概率更新
      p.Stroke_DM.2.Stroke_CHD <- p.Stroke_DM.2.Stroke_CHD / (p.Stroke_DM.2.death + p.Stroke_DM.2.Stroke_CHD + p.Stroke_DM.2.sub_Stroke)
      p.Stroke_DM.2.sub_Stroke <- p.Stroke_DM.2.sub_Stroke / (p.Stroke_DM.2.death + p.Stroke_DM.2.Stroke_CHD + p.Stroke_DM.2.sub_Stroke)
      p.Stroke_DM.2.death <- p.Stroke_DM.2.death / (p.Stroke_DM.2.death + p.Stroke_DM.2.Stroke_CHD + p.Stroke_DM.2.sub_Stroke)
    } else {
      # 否则与原始值相同
      p.Stroke_DM.2.Stroke_CHD <- p.Stroke_DM.2.Stroke_CHD
      p.Stroke_DM.2.sub_Stroke <- p.Stroke_DM.2.sub_Stroke
      p.Stroke_DM.2.death <- p.Stroke_DM.2.death
    }
    p.Stroke_DM.2.Stroke_DM <- 1 - (p.Stroke_DM.2.death + p.Stroke_DM.2.Stroke_CHD + p.Stroke_DM.2.sub_Stroke)
    
    #Markov State #8: "CHD History, With Diabetes"
    p.CHD_DM.2.sub_CHD <- RR_diff_total_CHD*as.numeric(sim_out_t[,"CHD_after_CHD_prob"])
    p.CHD_DM.2.CHD_Stroke <- RR_diff_total_Stroke*as.numeric(sim_out_t[,"Stroke_after_CHD_prob"])
    p.CHD_DM.2.death <- (1-exp(-(p.death+p.death.CHD+p.death.DM)))
    
    # 如果和大于1
    if (any((p.CHD_DM.2.death + p.CHD_DM.2.sub_CHD + p.CHD_DM.2.CHD_Stroke) > 1)) {
      # 概率更新
      p.CHD_DM.2.sub_CHD <- p.CHD_DM.2.sub_CHD / (p.CHD_DM.2.death + p.CHD_DM.2.sub_CHD + p.CHD_DM.2.CHD_Stroke)
      p.CHD_DM.2.CHD_Stroke <- p.CHD_DM.2.CHD_Stroke / (p.CHD_DM.2.death + p.CHD_DM.2.sub_CHD + p.CHD_DM.2.CHD_Stroke)
      p.CHD_DM.2.death <- p.CHD_DM.2.death / (p.CHD_DM.2.death + p.CHD_DM.2.sub_CHD + p.CHD_DM.2.CHD_Stroke)
    } else {
      # 否则与原始值相同
      p.CHD_DM.2.sub_CHD <- p.CHD_DM.2.sub_CHD
      p.Stroke_DM.2.sub_Stroke <- p.Stroke_DM.2.sub_Stroke
      p.CHD_DM.2.death <- p.CHD_DM.2.death
    }
    p.CHD_DM.2.CHD_DM <- 1 - (p.CHD_DM.2.death + p.CHD_DM.2.sub_CHD + p.CHD_DM.2.CHD_Stroke)
    
    #Markov State #9:"Recurrent Stroke" 
    p.sub_Stroke.2.death <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, (1-exp(-(p.death+p.death.Stroke+p.death.DM))), (1-exp(-(p.death+p.death.Stroke)))))
    p.sub_Stroke.2.Stroke_No_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 0, 1-p.sub_Stroke.2.death))#两次stroke
    p.sub_Stroke.2.Stroke_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 1 - p.sub_Stroke.2.death, 0))
    
    #Markov State #10:"Subsequent CHD" 
    p.sub_CHD.2.death <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, (1-exp(-(p.death+p.death.CHD+p.death.DM))), (1-exp(-(p.death+p.death.CHD)))))
    p.sub_CHD.2.CHD_No_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 0, 1 - p.sub_CHD.2.death))
    p.sub_CHD.2.CHD_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 1 - p.sub_CHD.2.death, 0))
    
    #Markov State #11: chd after stroke(stroke+chd)
    p.Stroke_CHD.2.death <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, (1-exp(-(p.death+p.death.CHD+p.death.DM))), (1-exp(-(p.death+p.death.CHD)))))
    p.Stroke_CHD.2.Stroke_CHD_No_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 0, 1 - p.Stroke_CHD.2.death))
    p.Stroke_CHD.2.Stroke_CHD_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 1 - p.Stroke_CHD.2.death, 0))
    
    #Markov State #12 stroke after chd(chd+stroke)
    p.CHD_Stroke.2.death <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, (1-exp(-(p.death+p.death.DM+p.death.Stroke))), (1-exp(-(p.death+p.death.Stroke)))))
    p.CHD_Stroke.2.CHD_Stroke_No_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 0, 1 - p.CHD_Stroke.2.death))
    p.CHD_Stroke.2.CHD_Stroke_DM <- as.numeric(ifelse(as.numeric(sim_out_t[,"DM"]) ==1, 1 - p.CHD_Stroke.2.death, 0))
    
    #Markov state #13:"Stroke&CHD history, No diabetes"
    p.CVD_No_DM.2.DM <- RR_diff_total_DM*as.numeric(sim_out_t[,"DM_prob"])*rep(data_for_analysis$risk_adjustment.DM, n.loop)
    p.CVD_No_DM.2.death <- (1-exp(p.death+p.death.CHD+p.death.Stroke))
    
    # 如果和大于1
    if (any((p.CVD_No_DM.2.DM + p.CVD_No_DM.2.death) > 1)) {
      # 概率更新
      p.CVD_No_DM.2.DM <- p.CVD_No_DM.2.DM / (p.CVD_No_DM.2.DM + p.CVD_No_DM.2.death)
      p.CVD_No_DM.2.death <- p.CVD_No_DM.2.death / (p.CVD_No_DM.2.DM + p.CVD_No_DM.2.death)
    } else {
      # 否则与原始值相同
      p.CVD_No_DM.2.DM <- p.CVD_No_DM.2.DM
      p.CVD_No_DM.2.death <- p.CVD_No_DM.2.death
    }
    
    p.CVD_No_DM.2.CVD_No_DM <- 1 - (p.CVD_No_DM.2.DM + p.CVD_No_DM.2.death)
    
    #Markov state #14:"Stroke&CHD history, With diabetes"
    p.CVD_DM.2.death <- (1-exp(p.death+p.death.CHD+p.death.Stroke+p.death.DM))
    # 如果和大于1
    if (any((p.CVD_DM.2.death) > 1)) {
      # 概率更新
      p.CVD_DM.2.death <- 1
    } else {
      # 否则与原始值相同
      p.CVD_DM.2.death <- p.CVD_DM.2.death
    }
    
    p.CVD_DM.2.CVD_DM <- 1 - p.CVD_DM.2.death
    
    #关于to death的问题：recurrent chd/stroke; chd&stroke都是GBD里面的数据
    #比如原来的模型中，recurrent chd to death和chd to death的概率是一样的，但在现实情况中感觉不是很合理
    
    
    ###################################################################################  
    ####2.5 Assign transition probablities to the transition matrix ##########
    ###################################################################################
    
    p.transition <- array(NA, dim=c(n.individual, n.health.state),
                          dimnames =  list(c(1:n.individual),name.health.state))
    p.transition[,"No CVD, No Diabetes"] <- ifelse(sim_out_t[,"state"] == "No CVD, No Diabetes", p.H.2.H, rep(0, n.individual))
    
    p.transition[,"No CVD, With Diabetes"] <- ifelse(sim_out_t[,"state"] == "No CVD, No Diabetes", p.H.2.DM, 
                                                     ifelse(sim_out_t[,"state"] == "No CVD, With Diabetes", p.DM.2.DM, rep(0, n.individual)))
    p.transition[,"First Stroke"] <- ifelse(sim_out_t[,"state"] == "No CVD, No Diabetes", p.H.2.initial_Stroke, 
                                            ifelse(sim_out_t[,"state"] == "No CVD, With Diabetes", p.DM.2.initial_Stroke, rep(0, n.individual)))
    p.transition[,"First CHD"] <- ifelse(sim_out_t[,"state"] == "No CVD, No Diabetes", p.H.2.initial_CHD, 
                                         ifelse(sim_out_t[,"state"] == "No CVD, With Diabetes", p.DM.2.initial_CHD, rep(0, n.individual)))
    p.transition[,"Stroke History, No Diabetes"] <- case_when(
      sim_out_t[,"state"] == "First Stroke" ~ p.initial_Stroke.2.Stroke_No_DM, 
      sim_out_t[,"state"] == "Stroke History, No Diabetes" ~ p.Stroke.2.Stroke, 
      sim_out_t[,"state"] == "Subsequent Stroke" ~ p.sub_Stroke.2.Stroke_No_DM, 
      TRUE ~ rep(0, n.individual)
    )
    p.transition[,"Stroke History, With Diabetes"] <- case_when(
      sim_out_t[,"state"] == "First Stroke" ~ p.initial_Stroke.2.Stroke_DM, 
      sim_out_t[,"state"] == "Stroke History, No Diabetes" ~ p.Stroke.2.DM, 
      sim_out_t[,"state"] == "Stroke History, With Diabetes" ~ p.Stroke_DM.2.Stroke_DM, 
      sim_out_t[,"state"] == "Subsequent Stroke" ~ p.sub_Stroke.2.Stroke_DM,
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"CHD History, No Diabetes"] <- case_when(
      sim_out_t[,"state"] == "First CHD" ~ p.initial_CHD.2.CHD_No_DM, 
      sim_out_t[,"state"] == "CHD History, No Diabetes" ~ p.CHD.2.CHD, 
      sim_out_t[,"state"] == "Subsequent CHD" ~ p.sub_CHD.2.CHD_No_DM, 
      TRUE ~ rep(0, n.individual)
    )
    p.transition[,"CHD History, With Diabetes"] <- case_when(
      sim_out_t[,"state"] == "First CHD" ~ p.initial_CHD.2.CHD_DM, 
      sim_out_t[,"state"] == "CHD History, No Diabetes" ~ p.CHD.2.DM, 
      sim_out_t[,"state"] == "CHD History, With Diabetes" ~ p.CHD_DM.2.CHD_DM, 
      sim_out_t[,"state"] == "Subsequent CHD" ~ p.sub_CHD.2.CHD_DM,
      TRUE ~ rep(0, n.individual)
    )
    
    
    p.transition[,"Subsequent Stroke"] <- case_when(
      sim_out_t[,"state"] == "Stroke History, No Diabetes" ~ p.Stroke.2.sub_Stroke, 
      sim_out_t[,"state"] == "Stroke History, With Diabetes" ~ p.Stroke_DM.2.sub_Stroke, 
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"Subsequent CHD"] <- case_when(
      sim_out_t[,"state"] == "CHD History, No Diabetes" ~ p.CHD.2.sub_CHD, 
      sim_out_t[,"state"] == "CHD History, With Diabetes" ~ p.CHD_DM.2.sub_CHD, 
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"Stroke+CHD"] <- case_when(
      sim_out_t[,"state"] == "Stroke History, No Diabetes" ~ p.Stroke.2.Stroke_CHD,
      sim_out_t[,"state"] == "Stroke History, With Diabetes" ~ p.Stroke_DM.2.Stroke_CHD,
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"CHD+Stroke"]<- case_when(
      sim_out_t[,"state"] == "CHD History, No Diabetes" ~ p.CHD.2.CHD_Stroke,
      sim_out_t[,"state"] == "CHD History, With Diabetes" ~ p.CHD_DM.2.CHD_Stroke,
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"Stroke&CHD History, No Diabetes"] <- case_when(
      sim_out_t[,"state"] == "Stroke+CHD" ~ p.Stroke_CHD.2.Stroke_CHD_No_DM,
      sim_out_t[,"state"] == "CHD+Stroke" ~ p.CHD_Stroke.2.CHD_Stroke_No_DM,
      sim_out_t[,"state"] == "Stroke&CHD History, No Diabetes" ~ p.CVD_No_DM.2.CVD_No_DM,
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"Stroke&CHD History, With Diabetes"]<- case_when(
      sim_out_t[,"state"] == "Stroke+CHD" ~ p.Stroke_CHD.2.Stroke_CHD_DM,
      sim_out_t[,"state"] == "CHD+Stroke" ~ p.CHD_Stroke.2.CHD_Stroke_DM,
      sim_out_t[,"state"] == "Stroke&CHD History, No Diabetes" ~ p.CVD_No_DM.2.DM,
      sim_out_t[,"state"] == "Stroke&CHD History, With Diabetes" ~ p.CVD_DM.2.CVD_DM,
      TRUE ~ rep(0, n.individual)
    )
    
    
    p.transition[,"DM_Death"] <- case_when(
      sim_out_t[,"state"] %in% 
        c("No CVD, With Diabetes", "Stroke History, With Diabetes", "CHD History, With Diabetes","Stroke&CHD History, With Diabetes") ~ p.death.DM, 
      sim_out_t[,"state"] == "DM_Death" ~ rep(1, n.individual),
      TRUE ~ rep(0, n.individual)
    )
    
    
    p.transition[,"Stroke_Death"] <- case_when(
      sim_out_t[,"state"] %in% 
        c("First Stroke", "Subsequent Stroke", "Stroke History, No Diabetes", "Stroke History, With Diabetes","CHD+Stroke","Stroke&CHD History, With Diabetes","Stroke&CHD History, No Diabetes") ~ p.death.Stroke, 
      sim_out_t[,"state"] == "Stroke_Death" ~ rep(1, n.individual),
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"CHD_Death"] <- case_when(
      sim_out_t[,"state"] %in%
        c("First CHD", "Subsequent CHD", "CHD History, No Diabetes", "CHD History, With Diabetes","Stroke+CHD","Stroke&CHD History, With Diabetes","Stroke&CHD History, No Diabetes") ~ p.death.CHD,
      sim_out_t[,"state"] == "CHD_Death" ~ rep(1, n.individual),
      TRUE ~ rep(0, n.individual)
    )
    
    p.transition[,"Non_DM_Non_CVD_Death"] <- case_when(
      sim_out_t[,"state"] %in% c("DM_Death", "Stroke_Death", "CHD_Death") ~ rep(0, n.individual),
      sim_out_t[,"state"] == "Non_DM_Non_CVD_Death" ~ rep(1, n.individual),
      TRUE ~ p.death
    )
    p.transition[,"Death"] <- case_when(
      sim_out_t[,"state"] == "No CVD, No Diabetes" ~ p.death, 
      sim_out_t[,"state"] == "No CVD, With Diabetes" ~ p.DM.2.death, 
      sim_out_t[,"state"] == "First Stroke" ~ p.initial_Stroke.2.death, 
      sim_out_t[,"state"] == "First CHD" ~ p.initial_CHD.2.death, 
      sim_out_t[,"state"] == "Stroke History, No Diabetes" ~ p.Stroke.2.death, 
      sim_out_t[,"state"] == "Stroke History, With Diabetes" ~ p.Stroke_DM.2.death, 
      sim_out_t[,"state"] == "CHD History, No Diabetes" ~ p.CHD.2.death, 
      sim_out_t[,"state"] == "CHD History, With Diabetes" ~ p.CHD_DM.2.death, 
      sim_out_t[,"state"] == "Subsequent Stroke" ~ p.sub_Stroke.2.death, 
      sim_out_t[,"state"] == "Subsequent CHD" ~ p.sub_CHD.2.death, 
      sim_out_t[,"state"] == "CHD+Stroke" ~ p.CHD_Stroke.2.death,
      sim_out_t[,"state"] == "Stroke+CHD" ~ p.Stroke_CHD.2.death,
      sim_out_t[,"state"] == "Stroke&CHD History, No Diabetes" ~ p.CVD_No_DM.2.death,
      sim_out_t[,"state"] == "Stroke&CHD History, With Diabetes" ~ p.CVD_DM.2.death,
      TRUE ~rep(1, n.individual) 
    )
    
    
    # Check that all transition probabilities add to 1 (rounded to 3 digits)
    #if (sum(round(rowSums(p.transition),3)!=1) != 0) {
    #p_sums = round(rowSums(p.transition),3)
    #error_out = sim_out_t[p_sums!=1,"state",t] # Output state of person(s) with error
    #stop("Transition probabilities do not add to 1. ", paste("Simulation", s, ", Time", t, ". "), "State(s) with error: ", error_out)
    #}
    #Transition to the next health state 
    
    sim_out_t[,"state"]<- apply(p.transition, 1, function(x) sample(name.health.state, 1, prob = x))
    
    sim_out_t[,"DM"] <- ifelse(sim_out_t[,"state"]%in% c("No CVD, With Diabetes", "Stroke History, With Diabetes", "CHD History, With Diabetes","Stroke&CHD History, With Diabetes"), 1, 
                               ifelse(sim_out_t[,"state"]== "DM_Death", 1, 
                                      ifelse(rep(data_for_analysis$GLUCOSE, n.loop) > 126, 1,0)))
    
    sim_out_t[,"CVD_history"] <- ifelse(sim_out_t[,"CVD_history"]==1, 1,
                                        ifelse( sim_out_t[,"state"] %in% c("First Stroke", "First CHD", "Stroke History, No Diabetes", "Stroke History, With Diabetes","CHD History, No Diabetes", "CHD History, With Diabetes",
                                                                           "Subsequent Stroke", "Subsequent CHD","CHD+Stroke","Stroke+CHD","Stroke&CHD History, No Diabetes","Stroke&CHD History, With Diabetes"),1, 0))
    
    sim_out_t[,"BMI"] <- ifelse(sim_out_t[,"state"]== "Death",NA,sim_out_t[,"BMI"])
    sim_out_t[,"Obesity"] <- ifelse(sim_out_t[,"BMI"] < 30, 0, 
                                    ifelse(sim_out_t[,"BMI"] >= 30, 1, sim_out_t[,"Obesity"]))
    raw.input.data<-sim_out_t
    
    # Update QALYs and costs
    #将处于健康状态QALY的设置为1，对于有DM和CVD的QALY模拟效用分布
    HRQOL_scores_t1<- ifelse(sim_out_t[,"state"]%in% c("First Stroke"), u_stroke_sim[s], 
                             ifelse(sim_out_t[,"state"]%in% c("First CHD"), u_CHD_sim[s],
                                    ifelse(sim_out_t[,"state"]%in% c("No CVD, With Diabetes", "Stroke History, With Diabetes", "CHD History, With Diabetes","Stroke&CHD History, With Diabetes"),u_DM_sim[s],
                                           ifelse(sim_out_t[,"state"]%in% c("Subsequent Stroke","CHD+Stroke"), u_post_stroke_sim[s],
                                                  ifelse(sim_out_t[,"state"]%in% c("Subsequent CHD","Stroke+CHD"), u_post_CHD_sim[s],
                                                         ifelse(sim_out_t[,"state"]== "Death", 0, 1))))))
    sim_out_t[,"HRQOL_scores"] =sim_out_t[,"HRQOL_scores"]+HRQOL_scores_t1
    sim_out_t[,"effect_disc"] <- sim_out_t[,"effect_disc"]+HRQOL_scores_t1/((1+beta_QALY)^(t-1))
    
    
    #将处于健康或死亡状态的HCE设置为0，有DM或者CVD的则模拟费用分布
    HCE_DM_inpatient_t1 <- calc_HCE_DM_inpatient(raw.input.data, HCE_DM_inpatient_parameter_sim[,s])
    
    HCE_DM_outpatient_t1 <- calc_HCE_DM_outpatient(raw.input.data, HCE_DM_outpatient_parameter_sim[,s])
    
    HCE_DM_t1 <- HCE_DM_inpatient_t1 + HCE_DM_outpatient_t1
    
    HCE_predict_t1 <- ifelse(sim_out_t[,"state"]%in% c("First Stroke", "Subsequent Stroke","CHD+Stroke"), c_stroke_sim[s], 
                             ifelse(sim_out_t[,"state"]%in% c("First CHD", "Subsequent CHD","Stroke+CHD"), c_CHD_sim[s],
                                    ifelse(sim_out_t[,"state"]%in% c("No CVD, With Diabetes", "Stroke History, With Diabetes", "CHD History, With Diabetes","Stroke&CHD History, With Diabetes"), HCE_DM_t1,0)))
    sim_out_t[,"HCE_predict"]=sim_out_t[,"HCE_predict"]+HCE_predict_t1
    sim_out_t[,"HCE_disc"] <- sim_out_t[,"HCE_disc"]+HCE_predict_t1/((1+beta_cost)^(t-1))
    
    #ppp Add productivity costs associated with stroke, and CHD respectively 
    Prod_cost_t1<- ifelse(sim_out_t[,"state"]%in% c("First Stroke", "Subsequent Stroke","CHD+Stroke"),c_prod_stroke_sim[s], 
                          ifelse(sim_out_t[,"state"]%in% c("First CHD", "Subsequent CHD","Stroke+CHD"), c_prod_CHD_sim[s],
                                 ifelse(sim_out_t[,"state"]%in% c("No CVD, With Diabetes", "Stroke History, With Diabetes", "CHD History, With Diabetes","Stroke&CHD History, With Diabetes"), c_prod_DM_sim[s],      
                                        ifelse(sim_out_t[,"state"]== "Death", 0, 0))))
    
    sim_out_t[,"Prod_cost"] =as.numeric(sim_out_t[,"Prod_cost"])+Prod_cost_t1
    #ppp
    sim_out_t[,"Prod_cost_disc"] <- as.numeric(sim_out_t[,"Prod_cost_disc"])+Prod_cost_t1/((1+beta_cost)^(t-1))
    
    if (intervention == "Policy") {
      Policy_cost_t1 <- ifelse(sim_out_t[,"state"]!= "Death", c_policy[s], 0)
    } else {
      Policy_cost_t1 <- 0
    }
    
    sim_out_t[,"Policy_cost"]<-sim_out_t[,"Policy_cost"]+Policy_cost_t1
    sim_out_t[,"Policy_cost_disc"] <- sim_out_t[,"Policy_cost_disc"]+Policy_cost_t1/((1+beta_cost)^(t-1))
    
    # Discount effects and costs
    
    sim_out_t[, "Total_cost_health"]<-sim_out_t[,"Policy_cost_disc"]+ sim_out_t[,"HCE_disc"]
    sim_out_t[, "Total_cost_societ"]<-sim_out_t[,"Policy_cost_disc"]+ sim_out_t[,"HCE_disc"]+sim_out_t[,"Prod_cost_disc"]
    
    
    sim_out_t[,"Life Years"]<-sim_out_t[,"Life Years"]+(sim_out_t[,"state"]!="Death")
    
    sim_out_t[,"Incident DM"] = sim_out_t[,"Incident DM"]+ ifelse(sim_out_t[,"state"]=="No CVD, With Diabetes"|sim_out_t[,"state"]== "Stroke History, With Diabetes"|sim_out_t[,"state"]=="CHD History, With Diabetes"|sim_out_t[,"state"]=="Stroke&CHD History, With Diabetes",1, 0)
    
    sim_out_t[,"Incident First CHD"] = sim_out_t[,"Incident First CHD"]+ ifelse(sim_out_t[,"state"]=="First CHD",1, 0)
    sim_out_t[,"Incident First Stroke"] = sim_out_t[,"Incident First Stroke"]+ ifelse(sim_out_t[,"state"]=="First Stroke",1, 0)
    
    sim_out_t[,"Incident Recurrent CHD"] = sim_out_t[,"Incident Recurrent CHD"]+ ifelse(sim_out_t[,"state"]=="Recurrent CHD",1, 0)
    sim_out_t[,"Incident Recurrent Stroke"] = sim_out_t[,"Incident Recurrent Stroke"]+ ifelse(sim_out_t[,"state"]=="Recurrent Stroke",1, 0)
    
    sim_out_t[,"Incident Stroke+CHD"] = sim_out_t[,"Incident Stroke+CHD"]+ ifelse(sim_out_t[,"state"]== "Stroke+CHD",1,0)
    sim_out_t[,"Incident CHD+Stroke"] = sim_out_t[,"Incident CHD+Stroke"] + ifelse(sim_out_t[,"state"]== "CHD+Stroke",1,0)
    
    
    sim_out_t[,"Incident CHD"] = sim_out_t[,"Incident First CHD"] + sim_out_t[,"Incident Recurrent CHD"] + sim_out_t[,"Incident Stroke+CHD"] + sim_out_t[,"Incident CHD+Stroke"]
    sim_out_t[,"Incident Stroke"] = sim_out_t[,"Incident First Stroke"] + sim_out_t[,"Incident Recurrent Stroke"]+ sim_out_t[,"Incident Stroke+CHD"] + sim_out_t[,"Incident CHD+Stroke"]
    
    sim_out_t[,"Death"] <-  ifelse(sim_out_t[,"state"] %in% c("Non_DM_Non_CVD_Death", "CHD_Death", "Stroke_Death", "DM_Death"), 1,0)
    
    sim_out_t[,"CHD_Death"] <-  ifelse(sim_out_t[,"state"]  %in% c("CHD_Death"), 1,0)
    
    sim_out_t[,"Stroke_Death"] <-  ifelse(sim_out_t[,"state"]  %in% c("Stroke_Death" ), 1,0)
    
    sim_out_t[,"DM_Death"] <-  ifelse(sim_out_t[,"state"] =="DM_Death", 1,0)
    
    sim_out_t[,"Non_DM_Non_CVD_Death"] <-  ifelse(sim_out_t[,"state"]=="Non_DM_Non_CVD_Death", 1,0)
  }
  
  sim_out_mean<-sim_out_t[ ,c(Out_variables)] %>% 
    group_by(Subject_ID)%>% 
    summarize_at(Out_variables[-1], mean, na.rm=TRUE)
  return(sim_out_mean)
}


