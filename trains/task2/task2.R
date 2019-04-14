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

train <- read.csv("trains\\task2\\train_k2.csv")
test <- read.csv("trains\\task2\\test_k2.csv")

test_num <- test$NUM

train$NUM <- NULL
test$NUM <- NULL

miss_train <- missForest(train, maxiter = 10, ntree=1000, verbose=T)$ximp
miss_test <- missForest(test, maxiter = 10, ntree=1000, verbose=T)$ximp

train$Age <- ifelse(train$Age < 13, "1",ifelse(train$Age < 20, "2",ifelse(train$Age < 60, "3", "4")))
test$Age <- ifelse(test$Age < 13, "1",ifelse(test$Age < 20, "2",ifelse(test$Age < 60, "3", "4")))

train$Embarked <- replace(train$Embarked, which(is.na(train$Embarked)), 'S')
test$Embarked <- replace(test$Embarked, which(is.na(test$Embarked)), 'S')
levels(test$Embarked) <- levels(train$Embarked)

train$rels <- (train$SibSp + train$Parch)
test$rels <- (test$SibSp + test$Parch)

train$rels <- ifelse(train$SibSp + train$Parch == 0, "1", ifelse(train$SibSp + train$Parch < 4, "2", "3"))
test$rels <- ifelse(test$SibSp + test$Parch == 0, "1", ifelse(test$SibSp + test$Parch < 4, "2", "3"))
train$SibSp <- NULL
train$Parch <- NULL
test$SibSp <- NULL
test$Parch <- NULL

t_train <- select(train, -Survived)
t_m <- as.factor(train$Survived)
t_train$Survived = as.factor(train$Survived)

index=createDataPartition(t_train$Survived,times=1,p=0.7,list=FALSE)
train_t=t_train[index,]
test_t=t_train[-index,]

train$Age <- as.numeric(train$Age)
test$Age <- as.numeric(test$Age)
train$Sex <- as.numeric(train$Sex)
test$Sex <- as.numeric(test$Sex)
train$rels <- as.numeric(train$rels)
test$rels <- as.numeric(test$rels)

### ridge!
library(glmnet)
y <- train$Survived
x <- train %>% select(Pclass, Sex, Age, Fare, rels) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x, y, alpha = 0, lambda = lambdas)

cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min

test_x <- test %>% select(Pclass, Sex, Age, Fare, rels) %>% data.matrix()

pred <- predict(fit, s = opt_lambda, newx = test_x)

###

desicion_tree=rpart(Survived ~ log(Pclass) + Sex + Age + log(Fare) + rels,
                    data=train,
                    method="class")

pred_rpart=predict(desicion_tree, data=test_t,type="class")
confusionMatrix(pred_rpart, test_t$Survived)

######

rf <- randomForest(Survived ~ log(Pclass) + Sex + Age + Fare + rels, t_train, 
                   type="regression", 
                   ntree = 1000, 
                   do.trace=T)

pred_rf = predict(rf, newdata=test_t, type="class")
confusionMatrix(pred_rf, train_t$Survived)

###
m_train <- train_t %>% select(-Survived)
m_test  <- test_t
z_train <- train_t$Survived

m_train$Sex <- as.numeric(m_train$Sex)
m_train$Embarked <- as.numeric(m_train$Embarked)
m_train$rels <- as.numeric(m_train$rels)
m_train$Age <- as.numeric(m_train$Age)

m_test$Sex <- as.numeric(m_test$Sex)
m_test$Embarked <- as.numeric(m_test$Embarked)
m_test$rels <- as.numeric(m_test$rels)
m_test$Age <- as.numeric(m_test$Age)

dtrain <- xgb.DMatrix(data=as.matrix(m_train), label=z_train)
dtest <- xgb.DMatrix(data=as.matrix(m_test))

xgb <- xgb.train(eta=0.001,
                 max_depth=8,
                 min_child_weight=1,
                 objective="reg:linear",
                 eval_metric="rmse",
                 data=dtrain,
                 maximize=F,
                 nrounds=500,
                 alpha=0.001,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample=0.7
)


pred_xgb <- predict(xgb, xgb.DMatrix(data=as.matrix(m_test)))

new_df_test <- read.csv("kaggle\\Titanic 1\\test.csv")$PassengerId
output <- data.frame(PassengerId=new_df_test,Survived=ifelse(pred>0.43,1,0))
write.csv(output, "D:\\Windows Default\\desktop\\output_xgboost_v2.12.csv", row.names=F)


