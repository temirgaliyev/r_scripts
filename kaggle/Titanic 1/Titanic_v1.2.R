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
library(missForest)

df_train <- read.csv("kaggle\\Titanic 1\\train.csv")
df_test <- read.csv("kaggle\\Titanic 1\\test.csv")
###
df_train <- df_train %>% mutate_if(is.character, as.factor)
df_train <- df_train %>% mutate_if(is.integer,as.numeric)
df_test <- df_test %>% mutate_if(is.character, as.factor)
df_test <- df_test %>% mutate_if(is.integer,as.numeric)

df_train$Survived <- as.factor(df_train$Survived)
###
df_train$Name <- NULL
df_train$PassengerId <- NULL
df_train$Cabin <- NULL
df_train$Ticket <- NULL
df_train$Embarked <- NULL
df_test$Name <- NULL
df_test$PassengerId <- NULL
df_test$Cabin <- NULL
df_test$Ticket <- NULL
df_test$Embarked <- NULL
###

sum(is.na(df_train))
sapply(df_train, function(x){sum(is.na(x))})
df_train <- missForest(df_train, maxiter = 10, ntree=100, verbose=T)$ximp
sum(is.na(df_test))
sapply(df_test, function(x){sum(is.na(x))})
df_test <- missForest(df_test, maxiter = 10, ntree=100, verbose=T)$ximp
###


# Определим зависимую переменную
# одну количественную price
# другую качественную cut

# rpart +  
# lm
# glm
# randomForest regression
# randomForest classification
# xgboost

# Поделим данные 70 на 30, 30 нужна
# для объективной проверки эф-ти модели.
decision_tree <- rpart(as.factor(Survived)~.,df_train)
pred_dt_fac <- predict(decision_tree, df_test)
summary(pred_dt_fac)

# glm
fit_fac <- glm(as.factor(Survived)~.,df_train,family = "binomial")
summary(fit_fac)
predict_glm <- predict(fit_fac,df_test)
summary(predict_glm)


# randomforest 
rf_fac <- randomForest(as.factor(Survived)~.,df_train,
                       type="classification",
                       ntree=500,
                       do.trace=TRUE)

predict_rf_fac <- predict(rf_fac,df_test,type="class")
#confusionMatrix(as.factor(df_test$Survived), predict_rf_fac)
#varImpPlot(rf_fac)
#new_test_df <- read.csv("kaggle\\Titanic 1\\test.csv")

output <- data.frame(PassengerId=new_test_df$PassengerId,Survived=predict_rf_fac)
write.csv(output, "output_v2.0.csv", row.names=F)

# XGBOOST

train_matrix <- data.matrix(select(df_train,-Survived))
train_target <- as.factor(df_train$Survived)

dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
#ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)

bst <- xgb.train(data=dtrain,
                 nround=500,
                 #maximize = FALSE,
                 #early_stopping_rounds = 10,
                 #watchlist = watchlist,
                 max_depth=7,
                 objective = "multi:softprob",
                 num_class=3,
                 eval_metric = "rmse",
                 alpha=0.01,
                 lambda=0.01
                 )

View(bst)
ctest <- xgb.DMatrix(data=as.matrix(df_test))
View(as.matrix(df_test_num))
predict_rf_fac <- predict(bst, as.matrix(df_test_num))
View(as.matrix(df_test_num))
length(predict_rf_fac)
pred <- ifelse(predict_rf_fac>0.5, 1, 0)
confusionMatrix(df_test$Survived, predict_rf_fac)
#varImpPlot(rf_fac)
new_test_df <- read.csv("kaggle\\Titanic 1\\test.csv")

View(df_test_num)
df_test_num <- df_test
df_test_num$Sex <- ifelse(df_test$Sex=="male", 1, 0)
nrow(pred)
output <- data.frame(PassengerId=new_test_df$PassengerId,Survived=pred)
write.csv(output, "output_v2.1.csv", row.names=F)

predict <- predict(bst,ctest)
RMSE(test_target,predict)

cv <- xgb.cv(data=dtrain,
             nround=500,
             #maximize = FALSE,
             #early_stopping_rounds = 10,
             #watchlist = watchlist,
             nfold=6,
             max_depth=7,
             objective = "reg:linear",
             eval_metric = "rmse",
             alpha=0.01,
             lambda=0.01,
             colsample_bytree=0.7,
             subsample = 0.7
)

pred <- predict(cv, df_test)