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


# Clean and separate data -------------------------------------------------

# Check independence
CNT <- data_train_DF %>% 
  drop_na() %>% 
  pull(CNT)

# Check independence
data_clean <- data_train_DF %>% 
  drop_na() %>% 
  select(-BA)

# Separate train and validation data

sample <- sample.int(n = nrow(data_clean), size = floor(.75*nrow(data_clean)), replace = F)
train <- data_clean[sample, ]
test  <- data_clean[-sample, ]

sub_train <- train %>% sample_n(100000)
sub_test <- test %>% sample_n(30000)


cnt_ecdf <- ecdf(train$CNT)
cnt_severity_threshold <- cnt_ecdf(u_cnt)

# Baseline Model ----------------------------------------------------------

#Baseline model
fit <-  glm(CNT ~ clim5, data = sub_train, family = poisson(link = "log"))
summary(fit)

# calculate estimated Poisson intensities (=means of predictive distributions):
pred_mean_cnt <-  predict(fit, sub_test[,-1], type = "response")

# calculate the matrix with estimated exceedance probability of the severity thresholds:
prediction_cnt <-  matrix(nrow = nrow(sub_test[,-1]), ncol = length(u_cnt))
for(k in 1:length(u_cnt)){
  prediction_cnt[,k] <-  ppois(u_cnt[k], lambda = pred_mean_cnt)
}
# prediction_cnt has to be submitted for the competition
apply(prediction_cnt, 2, mean)

# score
score(prediction_matrix = prediction_cnt, train_data = train$CNT, variable = "CNT")

# Quantile Regression -----------------------------------------------------

#quantile regression 
#qr_fit <- quantreg::rq(data = sub_train, CNT ~., tau = cnt_severity_threshold)

qr_fit <- readRDS("quantile_regression_CNT.RDS")

#Predict
qr_predict <- predict(qr_fit, newdata = sub_test[,-1])

#Change to ECDF
qr_predict_cdf <- apply(qr_predict, 2, cnt_ecdf)

#score
score(prediction_matrix = qr_predict_cdf, train_data = train$CNT, variable = "CNT")




# Plots -------------------------------------------------------------------

mean_qr <- apply(qr_predict, 2, mean)

plot(x = apply(prediction_cnt, 2, mean), y = cnt_ecdf(u_cnt), xlab = "Model Base", ylab = "Quantils a predir", log = "xy")
abline(a = 0, b = 1)

plot(mean_qr, u_cnt, xlab = "Regresió Quantils", ylab = "Quantils a predir", log = "xy")
abline(a = 0, b = 1)



# Variable importance -----------------------------------------------------

fit_rf <- randomForest(CNT ~., data = sub_train %>% sample_n(10000))
varImpPlot(fit_rf, type=2)

imp_df <- tibble(imp = fit_rf$importance, rownames(fit_rf$importance)) %>% 
  filter(imp > 20000)
imp_vars <- imp_df$`rownames(fit_rf$importance)`

#quantile regression 
qr_fit <- quantreg::rq(data = sub_train[, c("CNT", imp_vars)], CNT ~., tau = cnt_severity_threshold)

#Predict
qr_predict <- predict(qr_fit, newdata = sub_test[, imp_vars])

#Change to ECDF
qr_predict_cdf <- apply(qr_predict, 2, cnt_ecdf)

#score
score(prediction_matrix = qr_predict_cdf, train_data = train$CNT, variable = "CNT")

