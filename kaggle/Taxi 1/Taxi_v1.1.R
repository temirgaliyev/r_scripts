library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(xgboost)
library(rpart)
library(randomForest)
library(ROSE)
library(car)
library(caret)
library(lubridate)

earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

## Import datasets
train <- read.csv("kaggle\\Taxi 1\\train.csv")
test <- read.csv("kaggle\\taxi 1\\test.csv")

## Create some save
reinsurance_train <- train
reinsurance_test <- test

## If needed
train <- reinsurance_train
test <- reinsurance_test

## Save id of the test, but NULL both of them
test_id <- test$id
train$id <- NULL
test$id <- NULL

# Zero NA`s`
sapply(test, function(x){sum(is.na(x))})
sapply(train, function(x){sum(is.na(x))})

## Factor to Formal Class 'Period'
train$dropoff_datetime <- NULL
train$pickup_datetime <- lubridate::hms(sapply(strsplit(as.character(train$pickup_datetime), "\\s+"), "[", 2))
test$pickup_datetime <- lubridate::hms(sapply(strsplit(as.character(test$pickup_datetime), "\\s+"), "[", 2))

## It might be better to replace $pickup_datetime to some factors, as c("Morning", "Rush Hour", "Dinner", "Day", "Evening", "Night")
## But I`m lazy right now

## Replace $passenger_count from numeric to factor
train$passenger_count <- ifelse(train$passenger_count < 4, "1", ifelse(train$passenger_count < 7, "2", "3"))
test$passenger_count <- ifelse(test$passenger_count < 4, "1", ifelse(test$passenger_count < 7, "2", "3"))

## All integers to numeric
train <- train %>% mutate_if(is.integer, as.numeric)
test <- test %>% mutate_if(is.integer, as.numeric)

# Определеним зависимую переменную
# одну количественную $price
# другую качественную $cut

# rpart
# lm
# glm
# randomForest regression
# ramdonForest classification
# xgboost


# Поделим данные 70%/30%
# 30% для объективной проверки эффективности модели

# Bad Idea
train$dist <- earth.dist(train$pickup_longitude, train$pickup_latitude, train$dropoff_longitude, train$dropoff_latitude)
train$pickup_longitude <- NULL
train$pickup_latitude <- NULL
train$dropoff_longitude <- NULL
train$dropoff_latitude <- NULL

test$dist <- earth.dist(test$pickup_longitude, test$pickup_latitude, test$dropoff_longitude, test$dropoff_latitude)
test$pickup_longitude <- NULL
test$pickup_latitude <- NULL
test$dropoff_longitude <- NULL
test$dropoff_latitude <- NULL

###
set.seed(1)
index <- createDataPartition(train$trip_duration,p=0.7,list=F)

train_ <- train[index,]
test_ <- train[-index,]
sample4999 <- sample_n(train_, 4999)

# lm
fit <- lm(trip_duration~., sample4999)
shapiro.test(fit$residuals)
boxplot(fit$residuals)
hist(fit$residuals)
summary(fit)

log_method <- function(x){
  ifelse(abs(x)>0, log(abs(x)), 0)  
}
cols_num <- sapply(train_, function(x){is.numeric(x)})
train_log <- as.data.frame(sapply(train_[,cols_num], log_method))
test_log <- as.data.frame(sapply(test_[,cols_num], log_method))
str(train_log)

fit_log <- lm(trip_duration~., sample4999)
shapiro.test(fit_log$residuals)
summary(fit_log)

pred <- predict(fit, test_)
pred_log <- exp(predict(fit_log, test_log))

RMSE(test_$trip_duration, pred)
RMSE(exp(test_$trip_duration), pred_log)

## 
sample_150k <- sample_n(train_, 150000)

# randomForest

## logtitude-longtitude+lattitude-lattitude...
rf <- randomForest(trip_duration~., sample_150k, type="regression", ntree=300, do.trace=T)
predict_rf <- predict(rf, test_)


predict_rf <- predict(rf, test)
df_test.pred <- data.frame(id = test_id, trip_duration = predict_rf)
write.csv(df_test.pred, "D:\\Windows Default\\desktop\\output_taxt_v1.3.csv", row.names=F)

RMSE(test_$trip_duration, predict_rf)
varImpPlot(rf)

# XGBOOST
train_matrix <- data.matrix(select(train_, -trip_duration))
test_matrix <- data.matrix(select(test_, -trip_duration))

train_target <- train_$trip_duration
test_target <- test_$trip_duration

dtrain <- xgb.DMatrix(data=train_matrix, label=train_target)
ctest <- xgb.DMatrix(data=test_matrix, label=test_target)

watchlist <- list(train=dtrain, test=ctest)
bst <- xgb.train(data=dtrain,
                 nround=500,
                 maximize =F,
                 early_stopping_rounds = 10,
                 watchlist = watchlist,
                 max_depth=7,
                 objective="reg:linear",
                 eval_metric = "rmse",
                 alpha=0.01,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample=0.7
                 )

predict <- predict(bst, data.matrix(test))
RMSE(test_target, predict)