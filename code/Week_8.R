library(tidyverse)
library(ggplot2)

murders_data <- read_csv("/Users/rahulbanala/Documents/OMSBA 5112/week-8-rbanala15/data/murders.csv")

#filtering data to only include 1995 data
murders_data_1995 <- murders_data %>% filter(year == 1995)

#summary of descriptive stats
summary(murders_data_1995)

#Calculates correlation coefficients between variables
cor(murders_data_1995$murders, murders_data_1995$arrests, use = "complete.obs")

cor(murders_data_1995$popul, murders_data_1995$arrests, use = "complete.obs")

cor(murders_data_1995$murders, murders_data_1995$popul, use = "complete.obs")

cor(murders_data_1995$murdrate, murders_data_1995$arrestrate, use = "complete.obs")

cor(murders_data_1995$percblack, murders_data_1995$murders, use = "complete.obs")

#Defining variables more effectively 
num_murders <- murders_data_1995$murders

pop_teens <- murders_data_1995$perc1019

pop_twenties <- murders_data_1995$perc2029

perc_black <- murders_data_1995$percblack

perc_male <- murders_data_1995$percmale

real_pcinc <- murders_data_1995$rpcpersinc

murder_rate <- murders_data_1995$murdrate

#first linear model
murders_lm <- lm(num_murders ~ pop_teens + pop_twenties + perc_black + perc_male + real_pcinc,
                 data = murders_data_1995)
summary(murders_lm)

#calculates standardized residuals and creates histogram of values
murders_stnd_residual <- rstandard(murders_lm)
num_murders_histogram <- hist(murders_stnd_residual, xlab = "Residuals", breaks = 150)

#residual vs. fitted graph for num_murders
plot(murders_lm, which = 1)

#second linear model for murder rate
murder_rate_lm <- lm(murder_rate ~ pop_teens + pop_twenties + perc_black + perc_male + real_pcinc,
                 data = murders_data_1995)
summary(murder_rate_lm)

#calculates standardized residuals for murder rate model w/ histogram
murd_rate_stnd_residual <- rstandard(murder_rate_lm)
murder_rate_histogram <- hist(murd_rate_stnd_residual, xlab = "Residuals", breaks = 150)

#residual vs. fitted graph for murder rate
plot(murder_rate_lm, which = 1)

