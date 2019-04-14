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
df_test$Name <- NULL
df_test$PassengerId <- NULL
###
df_train <- dummies::dummy.data.frame(df_train, sep="_", names=c("Ticket", "Cabin", "Embarked"))
df_test <- dummies::dummy.data.frame(df_test, sep="_", names=c("Ticket", "Cabin", "Embarked"))
### 
df_test$Survived <- NA
cols_to_keep <- intersect(colnames(df_train), colnames(df_test))

df_train <- df_train[,cols_to_keep, drop=FALSE]
colnames(df_train) <- gsub(" ", "_", colnames(df_train), fixed=T)
colnames(df_train) <- gsub("/", "_", colnames(df_train), fixed=T)

df_test <- df_test[,cols_to_keep, drop=FALSE]
colnames(df_test) <- gsub(" ", "_", colnames(df_test), fixed=T)
colnames(df_test) <- gsub("/", "_", colnames(df_test), fixed=T)

colnames(df_train)
###

sum(is.na(df_train))
df_train <- missForest(df_train, maxiter = 10, ntree=100, verbose=T)$ximp
sum(is.na(df_test))
df_test <- missForest(df_test, maxiter = 10, ntree=100, verbose=T)$ximp
###

### TEST
test_df <- prodNA(iris, noNA = 0.2)
summary(test_df)
sapply(test_df, function(x){sum(is.na(x))})

m_test_df <- missForest(test_df, maxiter = 10, xtrue=iris, ntree=100, verbose=T)
### TEST


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
decision_tree <- rpart(Survived~.,df_train)
pred_dt_fac <- predict(decision_tree, df_test, type="class")
summary(pred_dt_fac)

# glm

fit_fac <- glm(Survived~.,df_train,family = "binomial")
summary(fit_fac)
predict_glm <- predict(fit_fac,df_test,type="response")
summary(predict_glm)

# randomforest 
rf_fac <- randomForest(Survived~.,df_train,
                       type="classification",
                       ntree=500,
                       do.trace=TRUE)

predict_rf_fac <- predict(rf_fac,df_test,type="class")
confusionMatrix(df_test$Survived, predict_rf_fac)
#varImpPlot(rf_fac)
new_test_df <- read.csv("kaggle\\Titanic 1\\test.csv")

output <- data.frame(PassengerId=new_test_df$PassengerId,Survived=predict_rf_fac)
write.csv(output, "output_v2.0.csv", row.names=F)

# XGBOOST

train_matrix <- data.matrix(select(df_train,-Survived))
test_matrix <- data.matrix(select(test_num,-price))

train_target <- df_train$Survived
test_target <- test_num$price

dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)
bst <- xgb.train(data=dtrain,
                 nround=500,
                 #maximize = FALSE,
                 #early_stopping_rounds = 10,
                 #watchlist = watchlist,
                 max_depth=7,
                 objective = "multi:softprob",
                 num_class=2,
                 eval_metric = "rmse",
                 alpha=0.01,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample = 0.7
)

predict_rf_fac <- predict(bst,df_test)
confusionMatrix(df_test$Survived, predict_rf_fac)
#varImpPlot(rf_fac)
new_test_df <- read.csv("kaggle\\Titanic 1\\test.csv")

output <- data.frame(PassengerId=new_test_df$PassengerId,Survived=predict_rf_fac)
write.csv(output, "output_v2.0.csv", row.names=F)

predict <- predict(bst,ctest)
RMSE(test_target,predict)

cv <- xgb.cv(data=dtrain,
             nround=500,
             #maximize = FALSE,
             #early_stopping_rounds = 10,
             watchlist = watchlist,
             nfold=6,
             max_depth=7,
             objective = "reg:linear",
             eval_metric = "rmse",
             alpha=0.01,
             lambda=0.01,
             colsample_bytree=0.7,
             subsample = 0.7
)