library(dplyr)
library(data.table)
library(fasttime)
library(lubridate)
library(xgboost)
bike = data.table::fread("train.csv")
bike_mean = data.table::fread("train.csv") 
bike_mean =bike_mean[1:5000,]
test = bike[5001:10886,1:9]
bike1 = bike[1:5000,]
bike1$count = log1p(bike1$count)
bike1 %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> bike1

test %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> test
head(bike,3)
X_train = bike1 %>% select(-count, - datetime, -registered, - casual) %>% as.matrix()
y_train = bike1$count
dtrain = xgb.DMatrix(X_train, label = y_train)
model = xgb.train(data = dtrain, nround = 150, max_depth = 5, eta = 0.1, subsample = 0.9)
X_test = test %>% select(- datetime) %>% as.matrix()
y_test = test$count
preds = predict(model, X_test)
preds = expm1(preds)
solution = data.frame(datetime = test$datetime, count = preds)
################################################################
bike_mean %>% 
  mutate(datetime = fastPOSIXct(datetime, "GMT")) %>% 
  mutate(hour = hour(datetime),
         month = month(datetime),
         year = year(datetime),
         wday = wday(datetime)) -> bike_mean

X_train_mean = bike_mean %>% select(-count, - datetime, -registered, - casual) %>% as.matrix()
y_train_mean = bike_mean$count
dtrain_mean = xgb.DMatrix(X_train_mean, label = y_train_mean)
model_mean = xgb.train(data = dtrain_mean, nround = 150, max_depth = 5, eta = 0.1, subsample = 0.9)
X_test_mean = test %>% select(- datetime) %>% as.matrix()
y_test_mean = test$count
preds_mean = predict(model_mean, X_test_mean)
solution_mean = data.frame(datetime = test$datetime, count = preds_mean)
###########################################################
real = bike[5001:10886,12]
rms1 = sqrt(mean((solution$count-real$count)^2,na.rm=TRUE))
rms2 = sqrt(mean((solution_mean$count-real$count)^2,na.rm=TRUE))
