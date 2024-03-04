setwd("C:/Users/ASUS/Desktop/data_v3/01_Input")
data<- read.csv("CHNS_Imp.csv",header=T)
##############################计算按age和gender分层的risk factors的mean和sd值##############################
# 将年龄分组为每五年一组
data$AGE_GROUP <- cut(data$AGE, breaks = c(seq(35, 95, by = 5), Inf), right = FALSE)
# 按性别和年龄组进行分组，并计算每组的各变量的平均值和标准差
summary_stats <- aggregate(cbind(BMI, SBP, TC, SMOKING, HDL_C, DM, WBC, TG, GLUCOSE) ~ GENDER + AGE_GROUP, 
                           data = data, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))
#输出trend的mean和sd值
trend_filepath = "./Trend_mean&sd.csv"
write.csv(summary_stats, trend_filepath, row.names = F)


#############################模拟5年后的数据集，保证各年龄段的mean和sd与初始数据分布一致##############################
set.seed(123)

# 初始化一个空数据框来存储模拟数据
simulated_data <- data.frame(AGE = numeric(0), GENDER = character(0), BMI = numeric(0), SBP = numeric(0), 
                             TC = numeric(0), SMOKING = numeric(0), HDL_C = numeric(0), DM = numeric(0), WBC = numeric(0),TG = numeric(0), GLUCOSE = numeric(0))

# 遍历每个年龄组和性别，生成模拟数据
for (gender in unique(data$GENDER)) {
  for (age_group in unique(data$AGE_GROUP)) {
    # 获取当前性别和年龄组的均值和标准差
    mean_sd_subset <- summary_stats[summary_stats$GENDER == gender & summary_stats$AGE_GROUP == age_group, ]
    
    # 从均值和标准差中提取所需信息
    bmi_mean <- mean_sd_subset$BMI[1]
    bmi_sd <- mean_sd_subset$BMI[2]
    sbp_mean <- mean_sd_subset$SBP[1]
    sbp_sd <- mean_sd_subset$SBP[2]
    tc_mean <- mean_sd_subset$TC[1]
    tc_sd <- mean_sd_subset$TC[2]
    smoking_mean <- mean_sd_subset$SMOKING[1]
    smoking_sd <- mean_sd_subset$SMOKING[2]
    hdl_c_mean <- mean_sd_subset$HDL_C[1]
    hdl_c_sd <- mean_sd_subset$HDL_C[2]
    dm_mean <- mean_sd_subset$DM[1]
    dm_sd <- mean_sd_subset$DM[2]
    wbc_mean <- mean_sd_subset$WBC[1]
    wbc_sd <- mean_sd_subset$WBC[2]
    tg_mean <- mean_sd_subset$TG[1]
    tg_sd <- mean_sd_subset$TG[2]
    glucose_mean <- mean_sd_subset$GLUCOSE[1]
    glucose_sd <- mean_sd_subset$GLUCOSE[2]
    
    
    # 生成当前性别和年龄组的模拟数据
    simulated_subset <- data[data$GENDER == gender & data$AGE_GROUP == age_group, ]
    
    # 生成模拟数据
    simulated_subset$AGE <- simulated_subset$AGE + 5 # 模拟数据的年龄增加5岁
    simulated_subset$AGE_GROUP <- cut(simulated_subset$AGE, breaks = c(seq(40, 100, by = 5), Inf), right = FALSE) # 更新模拟数据的年龄组
    
    # 获取新的年龄组
    new_age_group_index <- match(age_group, levels(summary_stats$AGE_GROUP)) + 1
    new_age_group <- levels(summary_stats$AGE_GROUP)[new_age_group_index]
    
    # 获取新的年龄组的均值和标准差
    new_mean_sd_subset <- summary_stats[summary_stats$GENDER == gender & summary_stats$AGE_GROUP == new_age_group, ]
    
    # 从新的均值和标准差中提取所需信息
    bmi_mean <- new_mean_sd_subset$BMI[1]
    bmi_sd <- new_mean_sd_subset$BMI[2]
    sbp_mean <- new_mean_sd_subset$SBP[1]
    sbp_sd <- new_mean_sd_subset$SBP[2]
    tc_mean <- new_mean_sd_subset$TC[1]
    tc_sd <- new_mean_sd_subset$TC[2]
    smoking_mean <- new_mean_sd_subset$SMOKING[1]
    smoking_sd <- new_mean_sd_subset$SMOKING[2]
    hdl_c_mean <- new_mean_sd_subset$HDL_C[1]
    hdl_c_sd <- new_mean_sd_subset$HDL_C[2]
    dm_mean <- new_mean_sd_subset$DM[1]
    dm_sd <- new_mean_sd_subset$DM[2]
    wbc_mean <- new_mean_sd_subset$WBC[1]
    wbc_sd <- new_mean_sd_subset$WBC[2]
    tg_mean <- new_mean_sd_subset$TG[1]
    tg_sd <- new_mean_sd_subset$TG[2]
    glucose_mean <- new_mean_sd_subset$GLUCOSE[1]
    glucose_sd <- new_mean_sd_subset$GLUCOSE[2]
    
    # 生成模拟数据
    simulated_subset$BMI <- rnorm(nrow(simulated_subset), mean = bmi_mean, sd = bmi_sd)
    simulated_subset$SBP <- rnorm(nrow(simulated_subset), mean = sbp_mean, sd = sbp_sd)
    simulated_subset$TC <- rnorm(nrow(simulated_subset), mean = tc_mean, sd = tc_sd)
    simulated_subset$SMOKING <- rnorm(nrow(simulated_subset), mean = smoking_mean, sd = smoking_sd)
    simulated_subset$HDL_C <- rnorm(nrow(simulated_subset), mean = hdl_c_mean, sd = hdl_c_sd)
    simulated_subset$DM <- rnorm(nrow(simulated_subset), mean = dm_mean, sd = dm_sd)
    simulated_subset$WBC <- rnorm(nrow(simulated_subset), mean = wbc_mean, sd = wbc_sd)
    simulated_subset$TG <- rnorm(nrow(simulated_subset), mean = tg_mean, sd = tg_sd)
    simulated_subset$GLUCOSE <- rnorm(nrow(simulated_subset), mean = glucose_mean, sd = glucose_sd)
    
    # 确保吸烟或糖尿病状态保持不变
    simulated_subset$SMOKING[simulated_subset$SMOKING > 1] <- 1
    simulated_subset$DM[simulated_subset$DM > 1] <- 1
    
    # 将生成的数据添加到模拟数据框中
    simulated_data <- rbind(simulated_data, simulated_subset)
  }
}

# 将模拟数据写入csv文件
simulated_data_filepath <- "./simulated_data_2014.csv"
write.csv(simulated_data, simulated_data_filepath, row.names = FALSE)

##################计算模拟后的分布#########################################
## 将年龄分组为每五年一组
simulated_data$AGE_GROUP <- cut(simulated_data$AGE, breaks = c(seq(35, 95, by = 5), Inf), right = FALSE)
# 按性别和年龄组进行分组，并计算每组的各变量的平均值和标准差
summary_stats <- aggregate(cbind(BMI, SBP, TC, SMOKING, HDL_C, DM, WBC, TG, GLUCOSE) ~ GENDER + AGE_GROUP, 
                           data = simulated_data, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))
#输出trend的mean和sd值
trend_filepath = "./Trend_mean&sd_5 years later.csv"
write.csv(summary_stats, trend_filepath, row.names = F)

#############################模拟10年后的数据集，保证各年龄段的mean和sd与初始数据分布一致##############################
set.seed(123)

# 初始化一个空数据框来存储模拟数据
simulated_data <- data.frame(AGE = numeric(0), GENDER = character(0), BMI = numeric(0), SBP = numeric(0), 
                             TC = numeric(0), SMOKING = numeric(0), HDL_C = numeric(0), DM = numeric(0), WBC = numeric(0),TG = numeric(0), GLUCOSE = numeric(0))

# 遍历每个年龄组和性别，生成模拟数据
for (gender in unique(data$GENDER)) {
  for (age_group in unique(data$AGE_GROUP)) {
    # 获取当前性别和年龄组的均值和标准差
    mean_sd_subset <- summary_stats[summary_stats$GENDER == gender & summary_stats$AGE_GROUP == age_group, ]
    
    # 从均值和标准差中提取所需信息
    bmi_mean <- mean_sd_subset$BMI[1]
    bmi_sd <- mean_sd_subset$BMI[2]
    sbp_mean <- mean_sd_subset$SBP[1]
    sbp_sd <- mean_sd_subset$SBP[2]
    tc_mean <- mean_sd_subset$TC[1]
    tc_sd <- mean_sd_subset$TC[2]
    smoking_mean <- mean_sd_subset$SMOKING[1]
    smoking_sd <- mean_sd_subset$SMOKING[2]
    hdl_c_mean <- mean_sd_subset$HDL_C[1]
    hdl_c_sd <- mean_sd_subset$HDL_C[2]
    dm_mean <- mean_sd_subset$DM[1]
    dm_sd <- mean_sd_subset$DM[2]
    wbc_mean <- mean_sd_subset$WBC[1]
    wbc_sd <- mean_sd_subset$WBC[2]
    tg_mean <- mean_sd_subset$TG[1]
    tg_sd <- mean_sd_subset$TG[2]
    glucose_mean <- mean_sd_subset$GLUCOSE[1]
    glucose_sd <- mean_sd_subset$GLUCOSE[2]
    
    
    # 生成当前性别和年龄组的模拟数据
    simulated_subset <- data[data$GENDER == gender & data$AGE_GROUP == age_group, ]
    
    # 生成模拟数据
    simulated_subset$AGE <- simulated_subset$AGE + 10 # 模拟数据的年龄增加5岁
    simulated_subset$AGE_GROUP <- cut(simulated_subset$AGE, breaks = c(seq(40, 100, by = 5), Inf), right = FALSE) # 更新模拟数据的年龄组
    
    # 获取新的年龄组
    new_age_group_index <- match(age_group, levels(summary_stats$AGE_GROUP)) + 2
    new_age_group <- levels(summary_stats$AGE_GROUP)[new_age_group_index]
    
    # 获取新的年龄组的均值和标准差
    new_mean_sd_subset <- summary_stats[summary_stats$GENDER == gender & summary_stats$AGE_GROUP == new_age_group, ]
    
    # 从新的均值和标准差中提取所需信息
    bmi_mean <- new_mean_sd_subset$BMI[1]
    bmi_sd <- new_mean_sd_subset$BMI[2]
    sbp_mean <- new_mean_sd_subset$SBP[1]
    sbp_sd <- new_mean_sd_subset$SBP[2]
    tc_mean <- new_mean_sd_subset$TC[1]
    tc_sd <- new_mean_sd_subset$TC[2]
    smoking_mean <- new_mean_sd_subset$SMOKING[1]
    smoking_sd <- new_mean_sd_subset$SMOKING[2]
    hdl_c_mean <- new_mean_sd_subset$HDL_C[1]
    hdl_c_sd <- new_mean_sd_subset$HDL_C[2]
    dm_mean <- new_mean_sd_subset$DM[1]
    dm_sd <- new_mean_sd_subset$DM[2]
    wbc_mean <- new_mean_sd_subset$WBC[1]
    wbc_sd <- new_mean_sd_subset$WBC[2]
    tg_mean <- new_mean_sd_subset$TG[1]
    tg_sd <- new_mean_sd_subset$TG[2]
    glucose_mean <- new_mean_sd_subset$GLUCOSE[1]
    glucose_sd <- new_mean_sd_subset$GLUCOSE[2]
    
    # 生成模拟数据
    simulated_subset$BMI <- rnorm(nrow(simulated_subset), mean = bmi_mean, sd = bmi_sd)
    simulated_subset$SBP <- rnorm(nrow(simulated_subset), mean = sbp_mean, sd = sbp_sd)
    simulated_subset$TC <- rnorm(nrow(simulated_subset), mean = tc_mean, sd = tc_sd)
    simulated_subset$SMOKING <- rnorm(nrow(simulated_subset), mean = smoking_mean, sd = smoking_sd)
    simulated_subset$HDL_C <- rnorm(nrow(simulated_subset), mean = hdl_c_mean, sd = hdl_c_sd)
    simulated_subset$DM <- rnorm(nrow(simulated_subset), mean = dm_mean, sd = dm_sd)
    simulated_subset$WBC <- rnorm(nrow(simulated_subset), mean = wbc_mean, sd = wbc_sd)
    simulated_subset$TG <- rnorm(nrow(simulated_subset), mean = tg_mean, sd = tg_sd)
    simulated_subset$GLUCOSE <- rnorm(nrow(simulated_subset), mean = glucose_mean, sd = glucose_sd)
    
    # 确保吸烟或糖尿病状态保持不变
    simulated_subset$SMOKING[simulated_subset$SMOKING > 1] <- 1
    simulated_subset$DM[simulated_subset$DM > 1] <- 1
    
    # 将生成的数据添加到模拟数据框中
    simulated_data <- rbind(simulated_data, simulated_subset)
  }
}

# 将模拟数据写入csv文件
simulated_data_filepath <- "./simulated_data_2019.csv"
write.csv(simulated_data, simulated_data_filepath, row.names = FALSE)

##################计算模拟后的分布#########################################
## 将年龄分组为每五年一组
simulated_data$AGE_GROUP <- cut(simulated_data$AGE, breaks = c(seq(35, 95, by = 5), Inf), right = FALSE)
# 按性别和年龄组进行分组，并计算每组的各变量的平均值和标准差
summary_stats <- aggregate(cbind(BMI, SBP, TC, SMOKING, HDL_C, DM, WBC, TG, GLUCOSE) ~ GENDER + AGE_GROUP, 
                           data = simulated_data, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))
#输出trend的mean和sd值
trend_filepath = "./Trend_mean&sd_10 years later.csv"
write.csv(summary_stats, trend_filepath, row.names = F)

####################拟合对数回归模型，计算AAPC & AAPC_SE###############################
# 导入数据
data_2009 <- read.csv("rf_2009.csv")
data_2014 <- read.csv("rf_2014.csv")
data_2019 <- read.csv("rf_2019.csv")

# 添加年份列
data_2009$YEAR <- 2009
data_2014$YEAR <- 2014
data_2019$YEAR <- 2019
# 合并数据
all_data <- rbind(data_2009, data_2014, data_2019)

# 计算ln(Y)
all_data$log_Y <- log(all_data$mean)

# 拟合log-linear regression model
model <- lm(log_Y ~ YEAR + factor(AGE_GROUP) + factor(GENDER) + factor(AGE_GROUP):factor(GENDER), data = all_data)

# 提取系数
coefficients <- coef(model)

# 提取年份与风险因素之间的系数
beta_year <- coefficients["YEAR"]

# 计算AAPC
AAPC <- 100 * (exp(-beta_year) - 1)

# 提取年份与风险因素之间的系数的标准误差
se_beta_year <- summary(model)$coefficients["YEAR", "Std. Error"]

# 计算AAPC的标准误差
AAPC_SE <- 100 * se_beta_year * exp(-beta_year)

# 将结果输出为csv文件
result <- data.frame(AAPC = AAPC, AAPC_SE = AAPC_SE)
write.csv(result, "AAPC_results.csv", row.names = FALSE)

# 定义函数计算AAPC和AAPC_SE
calculate_AAPC <- function(data) {
  # 计算ln(Y)
  data$log_Y <- log(data$mean)
  
  # 拟合log-linear regression model
  model <- lm(log_Y ~ YEAR, data = data)
  
  # 提取年份与风险因素之间的系数
  beta_year <- coef(model)["YEAR"]
  
  # 计算AAPC
  AAPC <- 100 * (exp(-beta_year) - 1)
  
  # 提取年份与风险因素之间的系数的标准误差
  se_beta_year <- summary(model)$coefficients["YEAR", "Std. Error"]
  
  # 计算AAPC的标准误差
  AAPC_SE <- 100 * se_beta_year * exp(-beta_year)
  
  return(c(AAPC, AAPC_SE))
}

# 计算每个风险因素按AGE_GROUP和GENDER组合的AAPC和AAPC_SE
AAPC_results <- list()

for (risk_factor in unique(all_data$RISK_FACTOR)) {
  # 提取相应的风险因素数据
  risk_factor_data <- all_data[all_data$RISK_FACTOR == risk_factor, ]
  
  # 按AGE_GROUP和GENDER组合进行计算
  for (age_group in unique(risk_factor_data$AGE_GROUP)) {
    for (gender in unique(risk_factor_data$GENDER)) {
      # 提取相应的数据
      subset_data <- risk_factor_data[risk_factor_data$AGE_GROUP == age_group & risk_factor_data$GENDER == gender, ]
      
      # 计算AAPC和AAPC_SE
      if (nrow(subset_data) > 1) {
        AAPC_results[[paste(risk_factor, age_group, gender, sep = "_")]] <- calculate_AAPC(subset_data)
      }
    }
  }
}

# 将结果转换为数据框
AAPC_df <- data.frame(Risk_Factor_Age_Group_Gender = names(AAPC_results), 
                      AAPC = sapply(AAPC_results, function(x) x[1]), 
                      AAPC_SE = sapply(AAPC_results, function(x) x[2]))

# 将结果输出为csv文件
write.csv(AAPC_df, "AAPC_results1.csv", row.names = FALSE)

# 加载所需的库
library(dplyr)
library(stringr)

# 读取数据
data_2009 <- read.csv("rf_2009.csv")
data_2014 <- read.csv("rf_2014.csv")
data_2019 <- read.csv("rf_2019.csv")

# 为每个数据集添加年份信息
data_2009$Year <- 2009
data_2014$Year <- 2014
data_2019$Year <- 2019
# 更正列名
colnames(data_2009)[grepl("^锘縂ENDER", colnames(data_2009))] <- "GENDER"
colnames(data_2014)[grepl("^锘縂ENDER", colnames(data_2014))] <- "GENDER"
colnames(data_2019)[grepl("^锘縂ENDER", colnames(data_2019))] <- "GENDER"
# 合并数据
all_data <- bind_rows(data_2009, data_2014, data_2019)

# 根据您的描述，假设风险因素的列名是 BMI, SBP, TC, SMOKING, HDL_C, DM.mean, DM.sd, WBC, TG, GLUCOSE
# 请根据实际情况调整列名
risk_factors <- c("BMI.mean", "SBP.mean", "TC.mean", "SMOKING.mean", "HDL_C.mean", "DM.mean", "WBC.mean", "TG.mean", "GLUCOSE.mean")

# 针对含有Inf值的列，将Inf值替换为NA
for (col in risk_factors) {
  all_data[is.infinite(all_data[[col]]), col] <- NA
}
# 计算ln(Y)并将结果存储在新列中
for (factor in risk_factors) {
  all_data <- all_data %>%
    mutate(!!paste0("ln_", factor) := log(!!sym(factor)))
}
# 删除包含缺失值的行

all_data <- all_data %>%
  filter(!(ln_DM.mean == -Inf))

all_data <- all_data %>%
  filter(!(ln_SMOKING.mean == -Inf))

# 计算每个分组中的观测数量
group_counts <- all_data %>%
  group_by(AGE_GROUP, GENDER) %>%
  summarize(n = n())

# 筛选出足够数据点的分组
valid_groups <- group_counts %>%
  filter(n >= 3)  # 假设 min_obs 是一个你认为足够的观测数量的阈值

# 仅保留足够数据点的分组
all_data_filtered <- all_data %>%
  inner_join(valid_groups, by = c("AGE_GROUP", "GENDER"))

# 选择以"ln_"开头的变量列
selected_vars <- grep("^ln_", colnames(all_data), value = TRUE)

# 拟合对数线性回归模型
models <- all_data_filtered %>%
  group_by(AGE_GROUP, GENDER) %>%
  summarize(model = list(lm(paste(selected_vars, collapse = " + ") ~ Year, data = .)))
# 过滤掉缺失值
all_data_filtered <- all_data %>%
  filter(!is.na(Year), !is.na(AGE_GROUP), !is.na(GENDER))

# 计算每个分组的数据点数量
group_counts <- all_data_filtered %>%
  group_by(AGE_GROUP, GENDER) %>%
  summarize(count = n())

# 选择数据点数量足够的分组
selected_groups <- group_counts %>%
  filter(count >= 3) # 根据需要设置足够的数据点数量阈值

# 从原始数据中筛选出选定的分组
all_data_selected <- all_data_filtered %>%
  semi_join(selected_groups, by = c("AGE_GROUP", "GENDER"))

# 拟合对数线性回归模型
models <- all_data_selected %>%
  group_by(AGE_GROUP, GENDER) %>%
  filter(n() >= 10) %>%
  summarize(model = list(lm(paste(selected_vars, collapse = " + ") ~ Year, data = cur_data(), na.action = na.exclude)))

这个修改会在 summarize 函数内部的 lm 函数中添加 na.action = na.exclude，以确保在建模

# 检查各组别的数据点数量
group_counts <- all_data_selected %>%
  group_by(AGE_GROUP, GENDER) %>%
  summarize(count = n())

# 打印出现不一致的组别
print(group_counts[group_counts$count != max(group_counts$count), ])

# 拟合对数线性回归模型
models <- all_data %>%
  group_by(AGE_GROUP, GENDER) %>%
  filter(n() >= 10) %>%  # 假设数据点数量至少为10个
  summarize(model = list(lm(paste(selected_vars, collapse = " + ") ~ Year, data = ., na.action = na.exclude)))

# 提取每个分层组合的回归系数
models <- models %>%
  mutate(
    beta = map_dbl(model, ~ coef(.)[2]),
    AAPC = 100 * (exp(beta) - 1),
    AAPC_SE = map_dbl(model, ~ summary(.)$coefficients[2, "Std. Error"])
  )

# 创建结果数据框
result <- models %>%
  select(AGE_GROUP, GENDER, AAPC, AAPC_SE)

# 输出结果为CSV文件
write.csv(result, "AAPC_results.csv", row.names = FALSE)

# 选择仅包含完整数据的行
complete_cases <- complete.cases(all_data)

# 使用完整数据重新定义 all_data
all_data_complete <- all_data[complete_cases, ]

# 重新运行分析代码
models <- all_data_complete %>%
  group_by(AGE_GROUP, GENDER) %>%
  summarize(model = list(lm(paste(selected_vars, collapse = " + ") ~ Year, data = ., na.action = na.exclude)))

# 提取每个分层组合的回归系数
models <- models %>%
  mutate(
    beta = map_dbl(model, ~ coef(.)[2]),
    AAPC = 100 * (exp(beta) - 1),
    AAPC_SE = map_dbl(model, ~ summary(.)$coefficients[2, "Std. Error"])
  )

# 创建结果数据框
result <- models %>%
  select(AGE_GROUP, GENDER, AAPC, AAPC_SE)

# 输出结果为CSV文件
write.csv(result, "AAPC_results.csv", row.names = FALSE)

# 拟合对数线性回归模型
models <- all_data %>%
  group_by(AGE_GROUP, GENDER) %>%
  filter(n() >= 10) %>%  # 假设数据点数量至少为10个
  summarize(model = list(lm(paste(selected_vars, collapse = " + ") ~ Year, data = cur_data(), na.action = na.exclude)))

# 提取每个分层组合的回归系数
models <- models %>%
  mutate(
    beta = map_dbl(model, ~ coef(.)[2]),
    AAPC = 100 * (exp(beta) - 1),
    AAPC_SE = map_dbl(model, ~ summary(.)$coefficients[2, "Std. Error"])
  )

# 创建结果数据框
result <- models %>%
  select(AGE_GROUP, GENDER, AAPC, AAPC_SE)

# 定义一个函数，用于拟合模型并返回AAPC及其标准误差
calculate_AAPC <- function(data) {
  if(nrow(data) >= 10) {  # 仅当数据点数量大于等于10时进行拟合
    model <- lm(paste(selected_vars, collapse = " + ") ~ Year, data = data, na.action = na.exclude)
    beta <- coef(model)[2]
    AAPC <- 100 * (exp(beta) - 1)
    AAPC_SE <- summary(model)$coefficients[2, "Std. Error"]
    return(c(AAPC, AAPC_SE))
  } else {
    return(c(NA, NA))  # 如果数据点数量不足，则返回NA
  }
}

# 拟合对数线性回归模型并计算AAPC及其标准误差
models <- all_data %>%
  group_by(AGE_GROUP, GENDER) %>%
  summarize(AAPC = calculate_AAPC(.))

# 创建结果数据框
result <- models %>%
  filter(!is.na(AAPC))  # 去除AAPC为NA的行
