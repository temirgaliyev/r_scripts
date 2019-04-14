library(ggplot2)

my_df <- read.csv("train.csv", sep = ";")
my_df

ggplot(my_df, aes(read, math, col = gender)) + 
  geom_point(size = 5) + 
  facet_grid(.~hon) + 
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

fit <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(fit))
head(predict(fit, type = "response")) # Шансы того, что он закончит с красным дипломом

my_df$prob  <- predict(object = fit, type = "response")

View(my_df)

# -------------------------------------

df <- mtcars
df$log_coef

fit <- glm(am ~ disp + vs + mpg, df, family = "binomial")
log_coef <- fit$coefficients
# --------------------------------------

library("ggplot2")

obj <- ggplot(data = ToothGrowth, aes(x = supp, y = len, fill = factor(dose))) + geom_boxplot()
obj
# --------------------------------------

library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)

#Precision = TP/(TP+FP) (Доля позитивных угаданных) 
perf3 <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
#Recall = Sensitivity = TP/(TP+FN) (Доля негативных угаданных)
perf4 <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
#   Accuracy = (TP+TN)/(TP+TN+FP+FN) (Сравнение факта и прогноза)
perf5 <- performance(pred_fit, x.measure = "cutoff", measure = "acc") 

plot(perf3, col = "red", lwd = 2)
plot(add = T, perf4, col = "green", lwd = 2)
plot(add = T, perf5,  lwd = 2)

legend(x = 0.6, y = 0.3, c("spec", "sens", "acc"),
       lty = 1, col = c("red", "green", "black"), bty = 'n', cex = 1, lwd = 2)


abline(v = 0.225, lwd = 2)

my_df$pred_resp <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N","Y"))
#my_df$pred_resp <- factor(ifelse(my_df$prob > 0.4, 1, 0), labels = c("N","Y"))

my_df$correct <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)

View(my_df)


ggplot(my_df, aes(prob, fill = factor(correct))) + 
  geom_dotplot() + 
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

mean(my_df$correct)


# ----------------------------------------------------------------------------------




test_df <- read.csv("test.csv", sep = ";")
test_df$hon <- NA

test_df$hon <- predict(fit, newdata = test_df, type = "response")
View(test_df)


# ----------------------------------------------------

question_df <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv")

without_na <- subset(question_df, !is.na(question_df$admit))
only_na <- subset(question_df, is.na(question_df$admit))

nrow(question_df)
View(question_df)

nrow(without_na)
View(without_na)

nrow(only_na)
View(only_na)


fit <- glm(admit ~ rank*gpa, without_na, family = "binomial")
c <- subset(only_na, predict(fit, only_na, type="response") > 0.4)
nrow(c)




