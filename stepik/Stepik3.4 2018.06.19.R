#
# regression  diagnostics
#

data(swiss)
str(swiss)


# relationships between all variables
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25, face="bold"))


# Outliers

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=25, face="bold")) +
  geom_smooth(method = "lm")

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()  

ggplot(swiss, aes(x = log(Education))) + 
  geom_histogram()  

# --------------------------------------

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

shapiro.test(my_vector)
shapiro.test(1/my_vector)
shapiro.test(sqrt(my_vector))
shapiro.test(log(my_vector))


# -----------------------------------------------------
beta.coef(mtcars[,c(1,3)])
lm(scale(mtcars[,c(1,3)])[, 1]~ scale(mtcars[,c(1,3)])[, 2])$coefficients

beta.coef(swiss[,c(1,4)])
lm(scale(swiss[,c(1,4)])[, 1]~ scale(swiss[,c(1,4)])[, 2])$coefficients

beta.coef <- function(x){
  return(lm(scale(x)[, 1]~ scale(x)[, 2])$coefficients)
}

# ----------------------------------------------------------

normality.test(mtcars[,1:6])

normality.test <- function(df){
  vector <- c()
  for (i in 1:ncol(df)) {
    vector <- c(vector, shapiro.test(df[,i])$p.value)
  }
  names(vector) <- names(df) 
  return(vector)
}


median(c(1, 4, 5, 1000))






