### ANOVA

library(ggplot2)

# formulae

DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way

DV ~ IV1:IV2  # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures

#----------------------

# Reading Data
mydata <- read.csv('shops.csv')

# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()

fit <- aov(price ~ origin, data = mydata)
summary(fit)


#Two-Way ANOVA

fit1 <- aov(price ~ origin + store, data = mydata)
summary(fit1)

model.tables(fit, "means")


#------------------------
fit3 <- aov(price ~ origin + store + origin : store, data = mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data = mydata) # same as prev
summary(fit4)
#------------------------
fit5 <- aov(yield ~ N * P, data = npk)
summary(fit5)
#------------------------
fit6 <- aov(yield ~ N + P + K, data = npk)
summary(fit6)
#------------------------

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit7 <-aov(price ~ food, data = mydata)
summary(fit7)

TukeyHSD(fit7)
#------------------------

ggplot(iris, aes(y = Sepal.Width, x = Species)) + 
  geom_boxplot()

fit8 <- aov(Sepal.Width ~ Species, data = iris)
TukeyHSD(fit8)

#------------------------


df <- mtcars

mydata2 <- read.csv('therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)

fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)

fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)

fit2 <- aov(well_being ~ therapy * price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()


fit2b <- aov(well_being ~ therapy * price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)

fit3 <- aov(well_being ~ therapy * price * sex, data = mydata2)
summary(fit3)

fit3b <- aov(well_being ~ therapy * price * sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)

#------------------------

mydata3 <- read.csv('Pillulkin.csv')
str(mydata2)

mydata3$patient <- as.factor(mydata3$patient)

fit <- aov(temperature ~ pill + Error(patient/pill), data = mydata3)
summary(fit)

#------------------------

fit <- aov(temperature ~ doctor*pill + Error(patient/doctor*pill), data = mydata3)
summary(fit)
#------------------------
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj












