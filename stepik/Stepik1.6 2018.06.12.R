



df <- mtcars

df$vs <- factor(df$vs, labels = c("V", "S")) 
df$am <- factor(df$am, labels = c("AUTO", "MANUAL")) 


hist(df$mpg, breaks = 20, xlab = "MPG")

boxplot(mpg ~ am, df, ylab = "MPG")

plot(df$mpg, df$hp)

plot(df$mpg, df$am)


library(ggplot2)
ggplot(df, aes(x = mpg)) + 
  geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df, aes(x = mpg, fill = am)) + 
  geom_dotplot()

ggplot(df, aes(x = mpg, fill = am)) + 
  geom_density(alpha = 0.2)


ggplot(df, aes(x = am, y = hp, col = vs)) +
  geom_boxplot()


ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec)) + 
  geom_point()

my_plot <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec)) +
  geom_point()

my_plot2 <- ggplot(df, aes(x = am, y = hp, col = vs))

my_plot2 + geom_boxplot()


ggplot(airquality, aes(x = airquality$Month, y = airquality$Ozone, group = Month)) + 
  geom_boxplot()


boxplot(Ozone ~ Month, airquality)






plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, color = hp))+
geom_dotplot()
plot1


ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point(aes(size = Petal.Length))

ggplot(aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point(iris, size = Petal.Length)




