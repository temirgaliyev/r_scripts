train <- read.csv("trains\\train.csv")
View(train)

#test <- read.csv("trains\\test.csv")
#View(test)

# Remove col with more than 0.5 NA
train <- train[, -which(colMeans(is.na(train)) > 0.5)]

# Mode of data.frame
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

col2[which(is.nan(col2))] = NA
col2[which(col2==Inf)] = NA

# Replace:
#  Factor NA  <-  Mode;
#  Numeric NA <- Median;
train[-1] <- lapply(train[-1], function(x) {
  if(is.factor(x)) replace(x, is.na(x), Mode(na.omit(x)))
  else if(is.numeric(x)) replace(x, is.na(x), mean(x, na.rm=TRUE))
  else x
})

# Показать кол-во отсутствующих
sum(sapply(train,function(x){sum(is.na(x))}))

boxplot(train$SalePrice)
fit <- lm(SalePrice~.,train) # R^2 0.9121
# without 50% / - 0.9143
summary(fit)

# if R-sq big - hetero
check <- lm((fit$residuals)^2~.,train)
# without 50% / - 0.6042
summary(check) # 0.6227

# Remove Outliers
df_tiny <- train[train$SalePrice<=mean(train$SalePrice)+sd(train$SalePrice),]
boxplot(df_tiny$SalePrice)

model_full <- lm(SalePrice ~ ., data = train) 
model_null <- lm(SalePrice ~ 1, data = train)
scope = list(lower = model_null, upper = model_full) # Пространство моделей
ideal_model <- step(model_full, scope = scope, direction = 'backward')

fit_full <- lm(SalePrice ~. , data = df_tiny)
optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit) # 0.918 !!!!!!!!!
#optimal_fit_log = lm(log(SalePrice) ~ log(MSSubClass) + MSZoning + log(LotFrontage) + log(LotArea) + Street + 
#                       LandContour + Utilities + LandSlope + Neighborhood + Condition1 + 
#                       Condition2 + HouseStyle + log(OverallQual) + log(OverallCond) + log(YearBuilt) + 
#                       log(YearRemodAdd) + RoofStyle + RoofMatl + Exterior1st + MasVnrType + 
#                       ExterQual + ExterCond + BsmtQual + BsmtCond + BsmtFinType1 + 
#                       log(BsmtFinSF1) + log(BsmtFinSF2) + log(BsmtUnfSF) + Heating + HeatingQC + 
#                       Electrical + log(X1stFlrSF) + log(X2ndFlrSF) + log(LowQualFinSF) + log(BsmtFullBath) + 
#                       KitchenQual + Functional + log(Fireplaces) + GarageType + GarageFinish + 
#                       log(GarageArea) + log(WoodDeckSF) + log(OpenPorchSF) + log(EnclosedPorch) + log(X3SsnPorch) + 
#                       log(ScreenPorch) + log(MoSold) + SaleCondition
#                     , data = df_tiny)


fit_tiny <- lm(SalePrice~.,df_tiny) # 0.9178
# Если не убирать 50% NA - 0.9174
summary(fit_tiny)

check_tiny <- lm((fit_tiny$residuals)^2~.,df_tiny) # 0.0155
# Если не убирать 50% NA - 0,005533
summary(check_tiny)

fit_tiny_log <- lm(log(SalePrice)~.,df_tiny) # 0.9065
#0.9062
summary(fit_tiny_log)

check_tiny_log <- lm((fit_tiny_log$residuals)^2~.,df_tiny) # 0.09519
#0.08564
summary(check_tiny_log)

###

test <- read.csv("trains\\test.csv")
View(test)

#test <- test[, -which(colMeans(is.na(test)) > 0.5)]

# Mode of data.frame
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Replace:
#  Factor NA  <-  Mode;
#  Numeric NA <- Median;
test[-1] <- lapply(test[-1], function(x) {
  if(is.factor(x)) replace(x, is.na(x), Mode(na.omit(x)))
  else if(is.numeric(x)) replace(x, is.na(x), mean(x, na.rm=TRUE))
  else x
})


#train$Condition2 <- as.factor( ifelse(is.na(df$y), as.character(df$x), as.character(df$y)) )


#train <- subset(train, select = -c(FireplaceQu))

#without_na <- subset(question_df, !is.na(question_df$admit))
#only_na <- subset(question_df, is.na(question_df$admit))

#mod2 <- lm(z~., data=train[,!colnames(train) %in% c("Condition2")], family="binomial")

df_tiny <- df_tiny[,!colnames(df_tiny) %in% c("Condition2")]
df_tiny <- df_tiny[,!colnames(df_tiny) %in% c("RoofMatl")]
df_tiny <- df_tiny[,!colnames(df_tiny) %in% c("Exterior1st")]
df_tiny <- df_tiny[,!colnames(df_tiny) %in% c("ExterCond")]
df_tiny <- df_tiny[,!colnames(df_tiny) %in% c("Heating")]
df_tiny <- df_tiny[,!colnames(df_tiny) %in% c("SaleType")]

#fit_tiny_log <- lm(log(SalePrice)~.,df_tiny) # 0.885
fit_tiny <- lm(SalePrice~.,df_tiny) # 0.9178
summary(fit_tiny) 
# Уменьшился до 0,8937, если убрать "Condition2"
# Уменьшился до 0,875, если убрать "RoofMatl"
# Уменьшился до 0,8739, если убрать "Exterior1st"
# Уменьшился до 0,8733, если убрать "ExterCond"
# Уменьшился до 0,8723, если убрать "Heating"
# Уменьшился до 0,872, если убрать "SaleType"
## Уменьшился до 0,8723, если убрать "Heating"
## Уменьшился до 0,8723, если убрать "Heating"
## Уменьшился до 0,8723, если убрать "Heating"
## Уменьшился до 0,8723, если убрать "Heating"
## Уменьшился до 0,8723, если убрать "Heating"

#fit <- glm(SalePrice ~., train)
pred <- exp(predict(fit_tiny, test))
View(pred)
write.csv(pred, file = "mpro_prediction.csv")

for(i in 1:nrow(pred)){
  
}
#c <- subset(only_na, predict(fit, only_na, type="response") > 0.4)
#nrow(c)

for (n in names(train)){
  if (is.factor(train[[n]])) {
    print(n)
    print(levels(train[[n]]))
    print("")
  }
}


