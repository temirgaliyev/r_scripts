?swiss


# numeric predictors

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination * Catholic, data = swiss)
summary(fit2)

confint(fit2)

#------------------------------
df <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na(df)

fill_na <- function(df){
  fit <- lm(y ~ x_1 + x_2, df, na.action = 'na.exclude')
  
  new_data <- df[is.na(df$y),]
  new_data$y_full <- predict(fit, new_data)
  
  df$y_full <- df$y
  df$y_full[is.na(df$y)] <- new_data$y_full
  return(df)
}
#------------------------------

df <- attitude
l <- lm(rating ~ complaints * critical, df)
summary(l)

#------------------------------


df <- mtcars
model <- lm(wt ~ mpg+disp+hp, df)
model
summary(model)



# -------------------------------------------------------------

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = "lm")

religious = factor(ifelse(swiss$Catholic > 50, "Catholic", "Not Catholic"))

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() + 
  geom_smooth(method = "lm")

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
carslm <- lm(mpg ~ wt*am, mtcars)
summary(carslm)

# -------------------------------------------------------------


library(ggplot2)
# сначала переведем переменную am в фактор
mtcars$am <- factor(mtcars$am)

# теперь строим график
my_plot <- ggplot(mtcars, aes(wt,mpg, col = am)) + 
  geom_smooth(method = "lm")
my_plot

