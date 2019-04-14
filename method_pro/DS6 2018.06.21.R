plot(mtcars)

cor(mtcars)

cols <- sapply(df, function(x){sum(is.na(x)/nrow(df))}) <= 0.4
df <- df[, cols]
df <- df[df$SalePrice <= mean(df$SalePrice) + sd(df$SalePrice),]
boxplot(df$SalePrice)
shapiro.test(df$SalePrice)
# extrapolation
m <- missForest(df)
df <- m$ximp
m$OOBerror
sum(is.na(df))

# ulovki
# в зависимости от задачи, регрессия решает две проблемы
# взаимосвязь
#гомо
# гетеро никогда не должно быть.
# мультиколинеарность можно оставить при прогнозе, при интерпретации - нет.
#отсутст мульти
#отсутст


fit <- lm(SalePrice~., df)
summary(fit)

check <- function(x, data){
  lm <- lm((x$residuals)^2~., data)
  summary(lm)
}

check(fit, df)

fit2 <- lm(log(SalePrice)~., df)
check(fit2, df)
plot(fit2)

# one hot encoding
library(dummies)
#    - get factor columns
library(dplyr)
df <- df %>% mutate_if(is.character, as.factor)
df <- df %>% mutate_if(is.integer, as.numeric)

df_fac <- df[, sapply(df, function(x){is.factor(x)})]
df_num <- df[, sapple(df, function(x){is.numeric(x)})]

# model
# evaluate model
#

head(df_fac)
df_dummy <- dummy.data.frame(data = df_fac)

# log NP
# nelzya
sapply(df_num, function(x){log(x)})

#hudsee
sapply(df_num, function(x){log(x+1)})

# bolee menee
sapply(df_num, function(x){log(abs(x)+1)})

df_num <- sapply(df_num, function(x){log(abs(x)+1)})
df_total <- cbind(df_num, df_fac)

fit_3 <- lm(SalePrice~., df_total)
summary(fit_3)
check(fit_3, df_total)
plot(fit_3)
#step(fit_3, direction = "backward")

diamonds %>% group_by(cut, color) %>% summarise(mean(price), max(carat))


sample_n()
sample_frac(diamonds, 3)
df_total$Id <- NULL
library(caret)
index <- createDataPartition(df_total$)
text <- df_total[-index,]
library(randomForest)

# ntree - кол-вол деревьев
# do.trace - трассировка результата, чтобы видеть процесс
# type - "regression", "classification"
rf <- randomForest(SalePrice~.,df_total, ntree = 500, do.trace = T)

fit4 <- lm(SalePrice~., df_total)

# varImpPlot - FeatureImportance
varImpPlot(rf)

pred_rf <- predict(rf, test)
pred-lm <- predict(lm, test)

RMSE(test$SalePrice, pred_rf)
RMSE(test$SalePrice, pred_lm)

pearson




View(mtcars)