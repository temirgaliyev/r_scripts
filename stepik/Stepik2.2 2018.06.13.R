library(ggplot2)

setwd("D:/Programs/R Scripts")

df <- iris
str(df)

df1 <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.4, fill = "white", col = 'black') +
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length)) +
  geom_boxplot()

# ----------------------------------------------

shapiro.test(df1$Sepal.Length) # Cлучайная величина X распределена нормально

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"]) 
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])


bartlett.test(Sepal.Length ~ Species, df1) # Гомогенность дисперсий

test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value

test1 <- t.test(Sepal.Length ~ Species, df, var.equal = T)

t.test(Sepal.Length ~ Species, df1, var.equal = T)
mean(df1$Sepal.Length)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)


# ----------------------------------------------

#t-Критерий Стьюдента для независимых выборок | Насколько наши данные отличаются от всего
t.test(Var1 ~ Var2, data) # если первая переменная количественная, а вторая фактор
t.test(data$Var1, data$Var2) # если обе переменные количественные

#t-Критерий Стьюдента для зависимых выборок
t.test(data$Var1, data$Var2, paired = T)

#Проверка на нормальность распределения
shapiro.test(Var1) # проверка на нормальность распределения переменной Var1
# но не удобно когда есть группирующая факторная переменная

#Поможет функция by(), которая применяет различные функции на каждом уровне фактора.  
by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test) # проверка на нормальность переменной 
# Sepal.Length в трех разных группах в соответствии с переменной Species


#Проверка на гомогенность дисперсий
bartlett.test(mpg ~ am, mtcars) #Критерий Бартлетта 
# ----------------------------------------------
ToothGrowth
oj <- subset(ToothGrowth, ToothGrowth$supp == "OJ" & ToothGrowth$dose == "0.5")
vc <- subset(ToothGrowth, ToothGrowth$supp == "VC" & ToothGrowth$dose == "2")
t_stat <- t.test(oj$len, vc$len)[1]
# ----------------------------------------------

med <- read.csv("lekarstva.csv")
dimnames(med)
t.test(med$Pressure_before, med$Pressure_after, paired = T)

# ----------------------------------------------

df <- iris
df1 <- subset(df, Species != "setosa")

# Доверительный Интервал

ggplot(df1, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun.y = mean, geom = "point", size = 4)


ggplot(df1, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 2)
  
# ----------------------------------------------

test2 <- wilcox.test(Petal.Length ~ Species, df1)
test2$statistic

ggplot(df1, aes(Species, Petal.Length)) + 
  geom_boxplot()

pared_wtest <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)
#----------------------------------
df <- read.table("dataset_11504_15.txt")
df

bartlett.test(V1 ~ V2, df) #Критерий Бартлетта 

t.test(df$V1, df$V2, paired = T)

wilcox.test(V1 ~ V2, df)
#----------------------------------
df <- read.table("dataset_11504_16.txt")
df

t.test(df$V1, df$V2) # если обе переменные количественные




