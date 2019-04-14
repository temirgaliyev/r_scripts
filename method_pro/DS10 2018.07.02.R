library(keras)
install_keras(method="conda")
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(caret)


data("BostonHousing")
?BostonHousing
data <- BostonHousing
str(data)

data %<>% mutate_if(is.factor,as.numeric)
attach(data)

rf <- randomForest::randomForest(medv~.,data)
randomForest::varImpPlot(rf)
# Neural network visualization
n <- neuralnet(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat,
               data = data,
               hidden = c(10,5),
               linear.output = F,
               lifesign = "full",
               rep=1)

plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')


# Matrix 
data <- as.matrix(data)
dimnames(data) <- NULL

# partition
set.seed(1234)
index <- createDataPartition(data[,14],p=0.8,list=FALSE)
train <- data[index,c(1:13)]
test <- data[-index,c(1:13)]

traintarget <- data[index,14]
testtarget <- data[-index,14]

# Normalize 
m <- colMeans(train)
s <- apply(train, 2, sd)

train <- scale(train,center = m,scale = s)
test <- scale(test,center = m,scale = s)

# Create model

model <- keras_model_sequential() # Среда для реализации
# Сделать каркас нейронной сети
model %>%
  layer_dense(units = 5, activation = "relu", input_shape = c(13)) %>%
  layer_dense(units = 1)

# Compile  (определяются метрики исчисления эффективности)
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(train,
      traintarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate

model %>%  evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred))

plot(testtarget,pred)


(range(df$medv))

#
#
#
#
# Fine tune model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = "relu", input_shape = c(13)) %>%
  layer_dense(units = 1)
summary(model)
# Compile

model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model

mymodel <- model %>%
  fit(train,
      traintarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate

model %>%  evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred))

plot(testtarget,pred)


# Fine tune model 2

model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)
s
summary(model)
# Compile

model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model

mymodel <- model %>%
  fit(train,
      traintarget,
     epochs = 100,
     batch_size = 32,
     validation_split = 0.2)

# Evaluate

model %>%  evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred))

plot(testtarget,pred)