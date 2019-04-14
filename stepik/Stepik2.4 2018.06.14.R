my_calc <- function(x, y){
  s <- x + y
  return(s)
}
res <- my_calc(10, 15)
res

my_calc_2 <- function(x, y){
  s <- x + y
  d <- x - y
  return(c(s, d))
}

my_calc_3 <- function(x, y, z = 0){
  s <- x + y + z
  d <- x - y - z
  return(c(s, d))
}

#-------------------------------
distr1 <- rnorm(100)
distr1[1:30] <- NA

distr1[is.na(distr1)] <- mean(distr1, na.rm = T)

my_na_rm <- function(x){
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
}

distr1 <- my_na_rm(distr1)

#-------------------------------

my_na_rm(x = c("2", "3", NA))

my_na_rm <- function(x){
  if(is.numeric(x)){
    stat_test <- shapiro.test(x)
    if(stat_test$p.value > 0.05){
      x[is.na(x)] <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)] <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    stop("X is not numeric")
  }
}

d1 <- rnorm(2000)
d2 <- runif(2000)

d1[1:10] <- NA
d2[1:10] <- NA

d1 <- my_na_rm(d1)
d2 <- my_na_rm(d2)

# Посмотреть Shapiro.Test

source("my_na_rm.R")

#-------------------------------

NA.position <- function(x){
  return(which(is.na(x)))
  # put your code here  
  
}

#-------------------------------
NA.counter <- function(x){
  return(sum(is.na(x)))
  # put your code here  
  
}


#-------------------------------

dir(pattern = "*.csv")
grants <- data.frame()
for(i in dir(pattern = "*.csv")){
  temp_df <- read.csv(i)
  grants <- rbind(temp_df, grants)
}

read_data <- function(){
  df <- data.frame()
  number <<- 0 # number changed
  for(i in dir(pattern = "*.csv")){
    number <<- number +1
    temp_df <- read.csv(i)
    df <- rbind(temp_df, df)
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}
 
grants2 <- read_data()


#-------------------------------
filtered.sum <- function(x){
  x <- x[!is.na(x)]
  x <- x[x == abs(x)]
  x <- sum(x)
  return(x)
}

filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))
  }

#-------------------------------

outliers.rm <- function(x){
  iqr <-  IQR(x)
  q <- quantile(x, probs = c(0.25, 0.75))
  x <- x[x>=q[[2]]+iqr*1.5 | x<=q[[1]]-iqr*1.5]
}
#-------------------------------












