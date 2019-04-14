# Decision tree
# Regression (Linear, Logistic, Mixed, Generalized)
# Random Forest (regression, classification)
# 
# Time-Series
#   Prophet
#
# xgboost
#
# flexdashboard
# plotly

news = read.csv("2.csv", quote = "", sep=",", row.names=NULL, encoding = "UTF-8", na.strings=c("NA",""," "))
View(news)
sapply(news, function(x)sum(is.na(x))/length(x))
       
library(crypto) # Coinmarketcup
library(prophet) # Time Series
library(lubridate) # Handle date
library(dplyr)

# Crypto
df <- crypto::crypto_history(coin = "xrp")
df <- df[, c(4,9)]
View(df)
str(df)
head(df)

# Lubridate
df$date <- lubridate::ymd(df$date)

# Для работы с prophet, необходимы две колонки:
# ds - data, y = показатель
# prophet Хорошо работает с дневными данными

colnames(df) <- c('ds', 'y')
?prophet
m <- prophet(df)
future <- make_future_dataframe(m, periods = 100)
forecast <- predict(m, future)
View(future)

plot(m, forecast)

prophet_plot_components(m, forecast)

f <- select(forecast, c(ds, yhat))
f %>% top_n(60, ds)


# Bayes classification
# Greed search 
#

# Data Engineering
# Data Vizualization
# Modelling

# Python

# Жизнь замечательных людей.























