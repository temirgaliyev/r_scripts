setwd("D:/Programs/R Scripts")


# linearity
library(ggplot2)
ggplot(data = swiss, aes(Examination, Education)) + 
  geom_point() +
  geom_smooth()



lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


swiss$Examination_squared <- (swiss$Examination)^2
lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm2, lm1)


swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) +
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd = 1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd = 1)


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) +
  geom_hline(col = "red", lwd = 1, yintercept = 0)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) +
  geom_hline(col = "red", lwd = 1, yintercept = 0)




# independence of errors
# now plot residuals against case number

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + 
  geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + 
  geom_smooth()


# Homoscedasticity
# plot residuals against redicted Y

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)


# ------------------------------
library(gvlma)

homosc <- read.csv("homosc.csv")
x <- gvlma(DV ~ IV, homosc)
x
summary(x)

# ----------------------------------

#Errors Normally Distributed
ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = "white", col = "black")

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)



ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = "white", col = "black")


qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)

# ----------------------------------------

fit <- lm(mpg ~ disp, mtcars)
my_plot <- resid.norm(fit)
my_plot

fit <- lm(mpg ~ wt, mtcars)
my_plot <- resid.norm(fit)
my_plot

resid.norm <- function(fit){
  ifelse(shapiro.test(fit$residuals)$p.value < 0.05, col <-  "red", col <-  "green")
  ret <- ggplot(fit, aes(x = fit$residuals)) + geom_histogram(fill = col)
}

# -----------------------------------------------

df <- swiss
df <- iris[,-5]

pairs(df)

high.corr(df)

high.corr <- function(x){
  step6 <- x[, sapply(x, is.numeric)]
  cor <- cor(step6)
  diag(cor) <- 0
  max_ <- max(cor)
  min_ <- min(cor)
  abs_max <- ifelse(max_>abs(min_), max_, min_)
  #print(abs_max)
  for (i in 1:nrow(cor)) {
    for(j in 1:ncol(cor)){
      #print(x[i,j])
      if (cor[i,j]==abs_max) {
        print(i)
        print(j)
        return (c(colnames(cor)[i],rownames(cor)[j]))
      }
    }
  }
}












