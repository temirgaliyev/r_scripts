

df <- mtcars

str(df)

df$vs <- factor(df$vs, labels = c("V", "S") )
df$am <- factor(df$am, labels = c("Auto", "Manual") )


View(df)



median(df$mpg)
mean(df$disp) 
sd(df$hp) # Standart Deviation
range(df$disp) 

mean_disp <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == 'V'])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])

?aggregate
mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
colnames(mean_hp_vs) <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean) # Same as aggregate(x = df$hp, by = list(df$vs), FUN = mean)
aggregate(hp ~ vs+am, df, mean) # aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)

aggregate(x = df[, -c(8, 9)], by = list(df$am), FUN = median)
aggregate(df[, c(1, 3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)
descriptions_stat

library(psych)
?describe
describe(x = df)
descr <- describe(x = df[, -c(8, 9)])

?describeBy
descr2 <- describeBy(x = df[, -c(8, 9)], group = df$vs)
descr2$V
descr2$S

descr2 <- describeBy(x = df[, -c(8, 9)], group = df$vs, mat = T, digits = 1)

descr3 <- describeBy(x = df[, -c(8, 9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, fast = T)

sum(is.na(df))

df$mpg[1:10] <- NA
mean(df$mpg, na.rm = T)

aggregate(mpg ~am, df, sd)

lmn <- subset(airquality, !is.na(airquality$Ozone))
lmn <- subset(lmn, lmn$Month == 7 | lmn$Month == 8 | lmn$Month == 9)
result <- aggregate(x = lmn$Ozone, by = list(lmn$Month), FUN = length)
result


airquality
describeBy(airquality, group = list(airquality$Month))

aggregate(x = iris, by = list(iris$ri), FUN = sd)

median(iris$Petal.Length)
median(iris$Sepal.Length)
median(iris$Sepal.Width)
median(iris$Petal.Width)



my_vector <- c(23, 10, 16, 19, 23, 22, 16, 21, 24, 20, 22, 21, 19, 25, 22, 14, 22, 14, 16, 15, NA, 24, NA, NA, NA, 23, 15, 21, 24, NA, NA, NA, 18, 21, 18, NA, 17, 20, 17, NA)
fixed_vector <- my_vector
mean(fixed_vector, na.rm = T)
replace(fixed_vector, is.na(fixed_vector), mean(fixed_vector, na.rm = T, digits = 3))
fixed_vector











