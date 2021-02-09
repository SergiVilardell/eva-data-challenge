library(tidyverse)
library(data.table)
library(ercv)
library(quantreg)
library(randomForest)
load("data_train.RData")

# show severity thresholds:
u_ba # for BA
u_cnt # for CNT
# show weights used for the weighted RPS:
weights_ba
weights_cnt

# explore the data:
dim(data_train_DF)
names(data_train_DF)

# data points to predict:
sum(is.na(data_train_DF$BA))
sum(is.na(data_train_DF$CNT))

# histogram of training data on log-scale for better readability:
hist(log(1+data_train_DF$CNT), xlab = "log(1+CNT)", main = "Histogram of log-transformed CNT")
hist(log(1+data_train_DF$BA), xlab = "log(1+BA)", main = "Histogram of log-transformed BA")

# Check independence
CNT <- data_train_DF %>% 
  drop_na() %>% 
  pull(CNT)

acf(CNT, lag = 1000)
pacf(CNT, lag = 1000)

# CV plot
cvplot(CNT, nextremes = 1000)

# Check independence
data_clean <- data_train_DF %>% 
  drop_na() %>% 
  select(-BA)

lm_fit <- lm(data = data_train_DF_clean, CNT ~.)
summary(lm_fit)
correlated_variables <- cor(data_train_DF_clean)
correlated_variables[lower.tri(correlated_variables, diag = T)] <- NA

#Only two variables are correlated more than 0.7
which(correlated_variables > 0.7)



