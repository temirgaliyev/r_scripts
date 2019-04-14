setwd("D:/Programs/R Scripts")
mtcars[, 1:3]
mtcars[,c(1, 4)]
mtcars[1,c(1, 4)]
# formula to split data 80/20
# install.packages("caret", dependencies = T)

data("mtcars")
head(mtcars)
mtcars$new <- ifelse(mtcars$mpg > mean(mtcars$mpg), 1, 0)
mtcars$mpg <- NULL

library(caret)
index <- createDataPartition(mtcars$new, p=0.8, list=FALSE)
train <- mtcars[index,]
test <- mtcars[-index,]
summary(train)

library(rpart)
decision_tree <- rpart(new~.,train)
predicted <- predict(decision_tree, test)

library(ROSE)
roc.curve(test$new, predicted)
###----------------------------
data("mtcars")
head(mtcars)

#mtcars[,1:3]
#mtcars[,c(1,4)]
#mtcars[1,c(1,4)]

mtcars$new <- ifelse(mtcars$mpg>mean(mtcars$mpg),1,0)
#mtcars$mpg <- NULL

# formula to split data 80/20
# install.packages("caret")
#library(caret)
index <- createDataPartition(mtcars$new,p=0.8,list=FALSE)
train <- mtcars[index,]
test <- mtcars[-index,]

#library(rpart)
# . Означает все как НП
# rpart(ЗП~.,data=)
# rpart(ЗП~a+s+c+d+w,data=)
#
decision_tree <- rpart(new~.,train)
# predict(model, test)
predicted <- predict(decision_tree,test)
#library(ROSE)
roc.curve(test$new,predicted)

###----------------------------
# Дерево решений
# Index Gini (0-1)
# Модель делит только по явным признакам

data('diamonds')
head(diamonds)

index <- createDataPartition(diamonds$cut,p=0.8,list=FALSE)
train <- diamonds[index,]
test <- diamonds[-index,]

decision_tree = rpart(cut~.,train)
predicted <- predict(decision_tree,test, type="class")
#predicted <- predict(decision_tree,test)

confusionMatrix(test$cut, predicted)

#Roc Curve - метрика исчисления эфф-ти модели
#Confusion Matrix - метрика исчисления эфф-ти модели
roc.curve(test$cut,predicted)



