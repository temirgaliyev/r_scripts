library(readxl)
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
library(missForest)

train <- read_excel("trains/task1/task1.xlsx",sheet = "TRAIN")
sum(is.na(test))
test <- read_excel("trains/task1/task1.xlsx",sheet = "TEST")
sum(is.na(train))
train <- na.omit(train)

test_id <- test$ID
train$ID <- NULL
test$ID <- NULL

train <- train %>% mutate_if(is.character, as.factor)
train <- train %>% mutate_if(is.factor, as.numeric)

index <- createDataPartition(train$ESTIM_COST,p=0.7,list=F)
train_ <- train[index,]
test_ <- train[-index,]

# rf
rf <- randomForest(ESTIM_COST~., train_, type="regression", ntree=1000, do.trace=T)
pred_rf <- predict(rf, test_, type="class")
RMSE(test_$ESTIM_COST, pred_rf) # 330810.5
varImpPlot(rf)

# y_train
y_train <- train
y_train$ID <- NULL
y_train$TRANSM_TYPE <- NULL
y_train$VIN_17 <- NULL
y_train$VIN_15 <- NULL
y_train$VIN_16 <- NULL
y_train$FUEL_TYPE <- NULL

y_train_ <- y_train[index,]
y_test_ <- y_train[-index,]

# rf
y_rf <- randomForest(ideal_model, y_train_, type="regression", ntree=1000, do.trace=T)
y_pred_rf <- predict(y_rf, y_test_, type="class")
RMSE(y_test_$ESTIM_COST, y_pred_rf) # 190920.1
varImpPlot(rf)

y_train <- y_train[y_train$ESTIM_COST<=mean(y_train$ESTIM_COST)+sd(y_train$ESTIM_COST),]
boxplot(y_train$ESTIM_COST)

## Brute Force 'Best' Formulae
model_full <- lm(ESTIM_COST ~ ., data = y_train_) 
model_null <- lm(ESTIM_COST ~ 1, data = y_train_)
scope = list(lower = model_null, upper = model_full) # Пространство моделей
ideal_model <- step(model_full, scope = scope, direction = 'backward')

ideal_model <- formula(ESTIM_COST ~ YEAR + VIN_1 + VIN_3 + ENGINE_VOLUME + BODY_TYPE + 
                         INTERIOR_TYPE + AUTO_CONDITION + AVG_COST)
# rf
y_rf <- randomForest(ESTIM_COST~., y_train_, type="regression", ntree=1000, do.trace=T)
y_pred_rf <- predict(y_rf, y_test_, type="class")
RMSE(y_test_$ESTIM_COST, y_pred_rf) # 198049.9
varImpPlot(rf)

y_train <- y_train[y_train$ESTIM_COST<=mean(y_train$ESTIM_COST)+sd(y_train$ESTIM_COST),]
boxplot(y_train$ESTIM_COST)

## lm
fit_reg <- lm(ideal_model, y_train_)
shapiro.test(fit_reg$residuals)
boxplot(fit_reg$residuals)
hist(fit_reg$residuals)
summary(fit_reg)

pred_lm <- predict(fit_reg, y_test_)
RMSE(y_test_$ESTIM_COST, pred_lm) # 225299.3

## XGBOOST
train_matrix <- data.matrix(select(y_train_, -ESTIM_COST))
test_matrix <- data.matrix(select(y_test_, -ESTIM_COST))

train_target <- y_train_$ESTIM_COST
test_target <- y_test_$ESTIM_COST

dtrain <- xgb.DMatrix(data=train_matrix, label=train_target)
ctest <- xgb.DMatrix(data=test_matrix, label=test_target)

watchlist <- list(train=dtrain, test=ctest)
bst <- xgb.train(data=dtrain,
                 nround=500,
                 maximize =F,
                 watchlist = watchlist,
                 early_stopping_rounds = 10,
                 #nfold=6,
                 max_depth=7,
                 objective="reg:linear",
                 eval_metric = "rmse",
                 alpha=0.07,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample=0.7)

pred_xgboost <- predict(bst, ctest)
RMSE(test_target, pred_xgboost) # 205025