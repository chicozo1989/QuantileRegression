


############################## Script 1 : Model ####################################################
############# Study: Weather impact on air pollution and land use variables #######################
###################################################################################################


# 1) Open the data
##############################
setwd("C:/Users/Micro/Documents/FGV/Artigo_2")
data <- read.csv("dados.csv")
library(plyr)

# 2) Estimate the 95%CI
##############################
data$Lower95CI_penalty_pm <- (data$pm_penalty-1.96*data$pm_std_penalty)
data$Upper95CI_penalty_pm <- (data$pm_penalty+1.96*data$pm_std_penalty) 


# 3) Categorize the 95%CI that includes zero 
###############################################
data$test_CI_penalty_pm[data$Lower95CI_penalty_pm<"0" & data$Upper95CI_penalty_pm<"0"] <- "Ok"
data$test_CI_penalty_pm[data$Lower95CI_penalty_pm>"0" & data$Upper95CI_penalty_pm>"0"] <- "Ok"

data$test_CI_penalty_pm[data$Lower95CI_penalty_pm<"0" & data$Upper95CI_penalty_pm>"0"] <- "no_signif"
data$test_CI_penalty_pm[data$Lower95CI_penalty_pm>"0" & data$Upper95CI_penalty_pm<"0"] <- "no_signif"

head(data)

table(data$test_CI_penalty_pm)


# 4) Filter the data only for the significant penalties 
#######################################################
library(tidyverse)
data_o3 <- data %>% filter(test_CI_penalty_pm=="Ok")
remove.packages(tidyverse)

# 5) Quantile regression model 
#######################################################


# Model for NO2
p = "o3_penalty"
data = "data_O3"

library(quantreg)

# To get the coeffitients
taus <- c(.05,.10,.25,.75,.9,.95)

mod<-rq(pm_penalty ~ transporte,tau=taus,data=data_o3,subset = pm_penalty, method="fn")

# Check the results
summary(mod)
mod$coefficients

# Extract the beta
beta_tau005 <- mod$coefficients[2]
beta_tau010 <- mod$coefficients[4]
beta_tau025 <- mod$coefficients[6]
beta_tau075 <- mod$coefficients[8]
beta_tau090 <- mod$coefficients[10]
beta_tau095 <- mod$coefficients[12]

# Extract the SE
a <- summary(mod)

SE1_tau005 <- a[[1]][["coefficients"]]
SE2_tau005 <- SE1_tau005[2,2]

SE1_tau010 <- a[[2]][["coefficients"]]
SE2_tau010 <- SE1_tau010[2,2]

SE1_tau025 <- a[[3]][["coefficients"]]
SE2_tau025 <- SE1_tau025[2,2]

SE1_tau075 <- a[[4]][["coefficients"]]
SE2_tau075 <- SE1_tau075[2,2]

SE1_tau090 <- a[[5]][["coefficients"]]
SE2_tau090 <- SE1_tau090[2,2]

SE1_tau095 <- a[[6]][["coefficients"]]
SE2_tau095 <- SE1_tau095[2,2]

# Save the results in a data frame
results <- data.frame()

results[1,1] <- "pm"
results[1,2] <- "transporte"
results[1,3] <- beta_tau005
results[1,4] <- SE2_tau005 
results[1,5] <- beta_tau010
results[1,6] <- SE2_tau010
results[1,7] <- beta_tau025
results[1,8] <- SE2_tau025 
results[1,9] <- beta_tau075
results[1,10] <- SE2_tau075 
results[1,11] <- beta_tau090
results[1,12] <- SE2_tau090
results[1,13] <- beta_tau095
results[1,14] <- SE2_tau095

colnames(results)<- c("Pollutant", "Variable", 
                      "Beta_tau005", "SE_tau005",
                      "Beta_tau010", "SE_tau010",
                      "Beta_tau025", "SE_tau025",
                      "Beta_tau075", "SE_tau075",
                      "Beta_tau090", "SE_tau090",
                      "Beta_tau095", "SE_tau095")

write.csv(results, file = "pm_transporte.csv")


#### Things to do before running the model
# There are NA for some observations (e.g., Agriculture area, first observation)


### Modeling strategies:
# Stratify analyzes by regions







