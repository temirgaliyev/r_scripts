

mydata <- read.csv('evals.csv')



# if

a <- 10
a <- c(1, -1)

ifelse(a > 0, 'positive', 'not positive')

# loops
for (i in 1:100) {
  print(i)  
}

for (i in 1:nrow(mydata)) {
  if(mydata$gender[i] == 'male')print(mydata$score[i])
}


mydata$quality <- rep(NA, nrow(mydata))
for (i in 1:nrow(mydata)) {
  if(mydata$score[i] > 4)mydata$quality[i]<- 'good'
  else mydata$quality[i] <- 'bad'
}

mydata$quality <- ifelse(mydata$score > 4, 'good', 'bad')

View(mydata)

i <- -1
while(i<51){
  print(mydata$score[i])
  i <- i+1
}


mtcars$new_var <- ifelse(mtcars$carb>=4 | mtcars$cyl > 6, 1, 0)

View(mtcars)

if (mean(my_vector) > 20) {
  result <- 'My mean is great'
} else {
  result <- "My mean is not so great"
} 


good_months <- c()

for (i in 1:(length(AirPassengers)-1)) {
  if(AirPassengers[i] < AirPassengers[i+1]) good_months <- c(good_months, AirPassengers[i+1]) 
}

good_months




moving_average <- c()

for(i in 1:135){
  moving_average <- c(moving_average, mean(AirPassengers[i:(i+9)]))
}

moving_average






