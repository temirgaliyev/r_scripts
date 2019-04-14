my_vector1 <- 1:67
my_vector2 <- c(-32, 156, 456, -0.2, 22)

my_vector1[1]
my_vector1[3]

my_vector2[2]

my_vector2[1, 2, 4]
my_vector2[c(1, 2, 4)]

my_vector2[1:3]

my_vector2[c(1, 5, 6, 7, 10)] #NA NA NA

the_best_vector <- c(1:5000, 7000:10000)

my_numbers <- c(6, 13, 13, 1, 18, 15, 6, 13, 19, 17, 19, 17, 18, 6, 0, 15, 10, 19, 10, 15)
my_numbers
my_numbers_2 <- c(my_numbers[c(2, 5, 7, 9, 12, 16, 20)])
my_numbers_2

my_numbers + 10

my_vector2 > 0
my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]

my_nums <-my_vector1[my_vector1 > 20 & my_vector1 < 30]


heights <- c(165, 148, 175, 200, 195, 180, 182)
mean_heights <- mean(heights)
greater_than_maean <- heights[heights > mean_heights]

my_vector <- c(8, 13, 9, 18, 7, 2, 15, 2, 8, 18, 6, 8, 9, 6, 12, 11, 3, 1, 2, 14)
my_sum <- sum(my_vector[my_vector>10])
my_sum


age <- c(16, 18, 22, 27)
is_married <- c(F, F, T, T)
name <- c("Olga", "Mariya", "Anastasya", "Polina")
data <- list(age, is_married, name)

data
data[1]
data[2]
data[[1]][1]
data[[2]][3]
data[[3]][1]

df <- data.frame(Name = name, Age = age, Status = is_married)

typeof(df)
class(df)

my_vector <- c(21, 18, 21, 19, 25, 20, 17, 17, 18, 22, 17, 18, 18, 19, 19, 27, 21, 20, 24, 17, 15, 24, 24, 29, 19, 14, 21, 17, 19, 18, 18, 20, 21, 21, 19, 19, 17, 21, 13, 17, 13, 23, 15, 23, 24, 16, 17, 25, 24, 22)
my_vector_2 <- my_vector[my_vector < mean(my_vector) + sd(my_vector) & my_vector > mean(my_vector) - sd(my_vector) ]

my_vector_2
test_vector <- c(21, 18, 21, 19, 20, 17, 17, 18, 22, 17, 18, 18, 19, 19, 21, 20, 17, 19, 21, 17, 19, 18, 18, 20, 21, 21, 19, 19, 17, 21, 17, 23, 23, 17, 22)

my_vector_2 == test_vector

