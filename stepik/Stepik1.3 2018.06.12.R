# Reading Data

mydata <- read.csv('evals.csv')

# Summaries

head(mydata, 3)
tail(mydata, 3)

View(mydata)

str(mydata)

names(mydata)

summary(mydata)



# Variables
ten_point_scale <- mydata$score * 2
mydata$ten_point_scale <- ten_point_scale
summary(mydata$ten_point_scale)
mydata$number <- 1:nrow(mydata)
ncol(mydata)


# Subsetting
mydata[1, 1]
mydata[c(2, 193, 255), 1]
mydata[101:200, 1]

mydata[5, ]
mydata[ ,1] # == mydata$score

# Subsettings with condition
mydata$gender
mydata$gender == 'female'
head(mydata[mydata$gender == 'female', 1:3])

head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))

#rbind, cbind

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)
mydata4

mydata5 <- mydata[, 1:10]
mydata6 <- mydata[, 11:22]
mydata7 <- cbind(mydata5, mydata6)

#library(help = "datasets")



mtcars$even_gear <- c(1-mtcars$gear%%2)
View(mtcars)

mpg_4 <- subset(mtcars$mpg, mtcars$cyl == 4)
mpg_4

mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]
mini_mtcars
mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]

