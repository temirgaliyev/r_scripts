setwd("D:/Programs/R Scripts")

library(ggplot2)
library(dplyr)

data("diamonds")
head(diamonds)

data("mtcars")
head(mtcars)

general <- diamonds
sample <- sample_frac(diamonds, 0.2)


# Визуализация

# qplot - quick plot
# qplot(x,y,)
?qplot
qplot(x = carat, 
      y = price,
      color = cut,
      data = sample)

boxplot(sample$carat)
hist(sample$carat)

#---------------------------------------------------
#---------------------------------------------------

head(mtcars)
summary(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$carb <- as.factor(mtcars$carb)

#---------------------------------------------------
qplot(x = mtcars$mpg, y = mtcars$disp, color = mtcars$cyl)
ggplot(mtcars, aes(mpg, disp)) + 
  geom_point(aes(color = carb), size = 2.5, alpha = 0.8)

#---------------------------------------------------

ggplot(mtcars, aes(x = mpg, fill = cyl)) + 
  geom_histogram(binwidth = 2)

#---------------------------------------------------

ggplot(mtcars, aes(x = mpg, y = disp, color = cyl, size = qsec)) + 
  geom_point()

#---------------------------------------------------

ggplot(mtcars, aes(x = mpg, fill = cyl, alpha = 0.8)) + 
  geom_density()

#---------------------------------------------------

ggplot(mtcars, aes(x = mpg, y = disp, group = cyl)) + 
  geom_boxplot(position = "identity")


#---------------------------------------------------

ggplot(data=mpg, aes(manufacturer)) + 
  geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle = 65))

#---------------------------------------------------

# grid
# wrap

# scale_color_continuous...
#  
#  

#ggsave("myplot.pdf")

#---------------------------------------------------



#---------------------------------------------------


output <- read.csv("newsAPI.csv", encoding = "UTF-8", quote = "")

