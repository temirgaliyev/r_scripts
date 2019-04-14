library(ggplot2)

df <- read.csv('grants.csv')

str(df)

# df$statuc <- factor(df$status, labels = c("Not funded", "Funded"))
df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")


# 1D Table

t1 <- table(df$status)
t1

dim(t1) # dimensions

# 2D Table

t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)
t2
dim(t2)
prop.table(t2) # Процентное соотношение от общего
prop.table(t2, 1) # Процентное соотношение по строке
prop.table(t2, 2) # Процентное соотношение по строке

# 3D Table

t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3
dim(t3)
#-------------------------------


hair_df <- HairEyeColor
View(hair_df)
dimnames(hair_df)
red_men <- prop.table(HairEyeColor[, "Blue",], 2)[3]
red_men
sum(HairEyeColor[, "Green","Female"])

#-------------------------------


# Plots

barplot(t1)
barplot(t2, legend.text = T, args.legend = list(x = "topright"))
barplot(t2, legend.text = T, args.legend = list(x = "topright"), beside = T)

mosaicplot(t2)


library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = subset(mydata, Sex=="Female"), aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

#-------------------------------

# Binomial Test

binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)

# Chi-Square

t1
chisq.test(t1)

chi <- chisq.test(t1)
chi

chi$exp
chi$obs

t2
chisq.test(t2)


# Fisher's Exact Test

fisher.test(t2)

#-------------------------------

dimnames(HairEyeColor)
new_tab <- HairEyeColor["Brown",,"Female"]
chisq.test(new_tab)

#-------------------------------

tb <-  table(diamonds$cut, diamonds$color)
main_stat  <- chisq.test(tb)[1]

#-------------------------------

tb <- table(ifelse(diamonds$price>mean(diamonds$price), 1, 0), 
            ifelse(diamonds$carat>mean(diamonds$carat), 1, 0))
main_stat <- chisq.test(tb)[1]
#-------------------------------

tb <- table(mtcars$vs, mtcars$am)
fisher_test <- fisher.test(tb)
fisher_test
p <- fisher_test[1]
p














