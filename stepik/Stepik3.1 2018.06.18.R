setwd("D:/Programs/R Scripts")
library(ggplot2)
library(psych)
df <- mtcars
View(df)

fit <- cor.test(x = df$mpg, y = df$hp) # Показывает коэф. корелляции между 2 переменными
cor.test(~ mpg + hp, df) # Тоже самое, что и выше

str(fit)
fit$p.value


plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl))) + 
  geom_point(size = 5)


df_numeric <- df[,c(1,3:7)]

pairs(df_numeric) # Очень много графиков всех со всеми.

cor(df_numeric) # Показывает коэф. корелляции между всеми.

fit <- corr.test(df_numeric)

fit$r
fit$p

# ---------------------------------------

#> corr.calc( mtcars[, c(1,5)] )  # на вход подаем данные mtcars только с переменными mpg и drat
#[1] 0.6811719078 0.0000177624

#> corr.calc( iris[,1:2] ) # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
#[1] -0.1175698 0.1518983


corr.calc( iris[,1:2] )
corr.calc(mtcars[, c(1,5)])

corr.calc <- function(n){
  x <- cor.test(x = n[,1], y = n[,2])
  return <- c(x$estimate, x$p.value)
}

# ---------------------------------------

step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )
step6 <- step6[, sapply(step6, is.numeric)] # все столбцы класса numeric
filtered.cor <- function(x){
  step6 <- x[, sapply(x, is.numeric)]
  cor <- cor(step6)
  diag(cor) <- 0
  max_ <- max(cor)
  min_ <- min(cor)
  return(ifelse(max_>abs(min_), max_, min_))
}


# ---------------------------------------
test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
#sha <- shapiro.test(test_data[,1])
#sha$p.value
#cor(x = test_data, method = "pearson")
smart_cor(test_data)

#[1] -0.1031003

smart_cor <- function(x){
  if(shapiro.test(x[,1])$p.value < 0.05 | shapiro.test(x[,2])$p.value < 0.05){
    return(cor(x = x, method = "spearman")[1,2])
  } else return(cor(x = x, method = "pearson")[1,2])
}

# ---------------------------------------
df <- mtcars
df_numeric <- df[,c(1,3:7)]

fit <- lm( mpg ~ hp, df)
fit

summary(fit)


ggplot(df, aes(hp, mpg)) +
  geom_point(size = 5) + 
  geom_smooth(method="lm") +
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg)) +
  geom_smooth(method="lm", se=F) +
  facet_grid(.~cyl)


fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg <- predict(fit, new_hp)

predict(fit, new_hp)

# -----------------------------------
library(ggplot2)
my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eight"))

fit <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl, mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25,face="bold"))

ggplot(my_df, aes(cyl, mpg)) + 
  geom_point() + 
  theme(axis.text = element_text(size=25), 
        axis.title = element_text(size=25, face="bold"))


aggregate(mpg ~ cyl, my_df, mean)


#--------------------------------------------
# lm - линейная регрессия
dataframe1 <- read.table("dataset_11508_12.txt", sep=' ' )     
mod1 <- lm(dataframe1[,1]~dataframe1[,2], dataframe1)
print(mod1$coefficients)


#--------------------------------------------
library(ggplot2)
df <- diamonds
df <- df[df$cut == "Ideal",]
df <- df[df$carat == 0.46, ]
fit <- lm(price ~ depth, df)
fit_coef <- fit$coefficients # коэффициенты модели



#--------------------------------------------

#regr.calc <- function(df){
#  if(cor(x = df, method = "pearson")[1, 2] > 0.05){
#    l <- lm(df[,1] ~ df[,2], df)
#    df$fit <- l$fitted.values
#    return(df)
#  } else return("There is no sense in prediction")
#}


my_df = iris[,1:2]
regr.calc(iris[,1:2])

my_df = iris[,c(1,4)]
regr.calc(my_df)

regr.calc <- function(df){
  if(cor.test(df[,1],df[,2])$p.value < 0.05){
    l <- lm(df[,1] ~ df[,2], df)
    df$fit <- l$fitted.values
    return(df)
  } else return("There is no sense in prediction")
}


# ---------------------------------

df <- iris
df
ggplot(df, aes(Sepal.Width, Petal.Width, col = Species)) +
  geom_point() + 
  geom_smooth(method="lm")



























