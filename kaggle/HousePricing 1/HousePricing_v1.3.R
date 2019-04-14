
train <- read.csv("2018.06.20\\train.csv")
#empty_train <- read.csv("2018.06.20\\train.csv")
#write.csv(empty_train, "empty_train.csv")
#empty_test <- read.csv("2018.06.20\\test.csv")
#write.csv(empty_test, "empty_test.csv")
#View(test)

# 'Smart' Remove all NA
train <- removeNa(train)
# Показать кол-во отсутствующих
sum(sapply(train,function(x){sum(is.na(x))}))


#train <- create_df_without_factors(train)
#View(train)
#sum(sapply(train,function(x){sum(is.factor(x))}))

boxplot(train$SalePrice)
#fit <- lm(SalePrice~.,train)
#summary(fit)

# if R-sq big - hetero
#check <- lm((fit$residuals)^2~.,train)
# without 50% / - 0.6042
#summary(check) # 0.6227

# Remove Outliers
train <- train[train$SalePrice<=mean(train$SalePrice)+sd(train$SalePrice),]
boxplot(train$SalePrice)
summary(lm(SalePrice~.,train))

# Compute Optimal Formula
#fit_full <- lm(SalePrice ~. , data = df_tiny)
#optimal_fit <- step(fit_full, direction = 'backward')
#summary(optimal_fit) # 0.9231 !!!!!!!!!

#pred <- exp(predict(optimal_fit_log_formula, test))


# Get all factor names from df
#names(Filter(is.factor, df_tiny))

###

test <- read.csv("2018.06.20\\test.csv")
View(test)

#test <- test[, -which(colMeans(is.na(test)) > 0.5)]


# Replace:
#  Factor NA  <-  Mode;
#  Numeric NA <- Median;
test <- removeNa(test)
#test <- create_df_without_factors(test)


#fit <- lm(SalePrice~.,train) # R^2 0.9121
# without 50% / - 0.9143
#summary(fit)

#fit1 <- lm(SalePrice~.,train_for_test) # R^2 0.9121
#summary(fit1)

# if R-sq big - hetero
#check <- lm((fit$residuals)^2~.,train)
# without 50% / - 0.6042
#summary(check) # 0.6227

library(ggplot2)
qplot(x=hp,y=mpg,data=mtcars)+ 
  geom_smooth(method = 'lm') + 
  theme(panel.background = element_rect(fill = 'gray'))

cor.test(train_for_test)

shapiro.test(fit$residuals)

train_for_test <- train[, intersect(colnames(train), colnames(test))]
train_for_test$SalePrice <- train$SalePrice

# Compute Optimal Formula
fit_full <- lm(SalePrice ~., data = train)
optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit) # 0.9231 !!!!!!!!!

model_full <- lm(SalePrice ~ ., data = train) 
model_null <- lm(SalePrice ~ 1, data = train)

scope = list(lower = model_null, upper = model_full) # Пространство моделей
ideal_model <- step(model_full, scope = scope, direction = 'backward')

train <- create_df_without_factors(train)
optimal_fit_f = lm(SalePrice ~ MSZoningFV + MSZoningRM + LotFrontage + 
                     LotArea + LandSlopeSev + NeighborhoodCrawfor + 
                     NeighborhoodEdwards + NeighborhoodMeadowV + Condition1Norm + 
                     NeighborhoodNridgHt + BldgTypeTwnhs  + BldgTypeTwnhsE + OverallQual +
                     OverallCond + YearBuilt + YearRemodAdd + RoofMatlMembran^2 + RoofMatlCompShg^2 + 
                     RoofMatlMetal^2 + RoofMatlRoll + RoofMatlWdShake^2 + RoofMatlWdShngl^2 + 
                     ExterQualFa + ExterQualGd + ExterQualTA + FoundationPConc + 
                     BsmtCondPo + BsmtFinType1Unf + BsmtFinSF1 + 
                     BsmtFinType2BLQ + BsmtFinSF2 + BsmtUnfSF + HeatingQCTA + 
                     CentralAirY + X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + FullBath +
                     HalfBath + KitchenAbvGr + KitchenQualFa + KitchenQualGd^2 + KitchenQualTA + 
                     FunctionalMaj2 + FunctionalSev + FunctionalTyp + Fireplaces + 
                     GarageCars + GarageArea + WoodDeckSF + EnclosedPorch + ScreenPorch + 
                     SaleConditionAdjLand + SaleConditionNormal + SaleConditionPartial +
                     + RoofMatlTarGrv^2
  ,data = train)

summary(optimal_fit_f)
#8751
fit1 <- lm(SalePrice ~. , data = optimal_fit)
summary(fit1)


#if(ncol(new_train) == 0){
#  new_train <- data.frame(sapply(train, function(x){ifelse(is.numeric(x), log(abs(x)+1), x)}))
#} else{
#  new_train <- cbind(new_train, data.frame(sapply(train, function(x){ifelse(is.numeric(x), log(abs(x)+1), x)})))
#}

new_train <- train[,1]
for(i in 2:ncol(train)){
  x <- train[,i]
  if(is.numeric(x)){
    new_train <- rbind(new_train, log(abs(x)+1))
  } else{
    new_train <- rbind(new_train, x)
  }
}

optimal_fit_f_n = lm(SalePrice ~ MSZoningFV + MSZoningRM + LotFrontage + 
                     LotArea + LandSlopeSev + NeighborhoodCrawfor + 
                     NeighborhoodEdwards + NeighborhoodMeadowV + Condition1Norm + 
                     NeighborhoodNridgHt + BldgTypeTwnhs  + BldgTypeTwnhsE + OverallQual +
                     OverallCond + YearBuilt + YearRemodAdd + RoofMatlMembran + RoofMatlCompShg + 
                     RoofMatlMetal + RoofMatlRoll + RoofMatlWdShake + RoofMatlWdShngl + 
                     ExterQualFa + ExterQualGd + ExterQualTA + FoundationPConc + 
                     BsmtCondPo + BsmtFinType1Unf + BsmtFinSF1 + 
                     BsmtFinType2BLQ + BsmtFinSF2 + BsmtUnfSF + HeatingQCTA + 
                     CentralAirY + X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + FullBath +
                     HalfBath + KitchenAbvGr + KitchenQualFa + KitchenQualGd + KitchenQualTA + 
                     FunctionalMaj2 + FunctionalSev + FunctionalTyp + Fireplaces + 
                     GarageCars + GarageArea + WoodDeckSF + EnclosedPorch + ScreenPorch + 
                     SaleConditionAdjLand + SaleConditionNormal + SaleConditionPartial +
                     + RoofMatlTarGrv
                   ,data = new_train)

summary(optimal_fit_f_n)


train <- train[,!colnames(train) %in% c("FireplaceQu")]
train <- train[,!colnames(train) %in% c("Condition2")]
fit <- lm(log(SalePrice)~.,train) # R^2 0.9121
summary(fit)
fit1 <- lm(SalePrice~.,train) # R^2 0.9121
summary(fit1)

test$pred <- exp(predict(ideal_fit_f, test))
train$pred <- predict(optimal_fit, train)
output <- data.frame(Id = test$Id, SalePrice = test$pred)
write.csv(output, "output.csv", row.names = FALSE)

View(train)
View(pred)
write.csv(pred, file = "kaggle_prediction_2.csv")


create_df_without_factors <- function(df){
  
  df <- df %>% mutate_if(is.character, as.factor)
  df <- df %>% mutate_if(is.integer, as.numeric)
  
  new_df <- data.frame()
  for(i in 1:ncol(df)){
    if(is.factor(df[,i])){
      for (level in unique(df[,i])){
        n_df <- data.frame(ifelse(as.character(df[,i]) == level, 1, 0))
        level <- gsub("&", "", level)
        colnames(n_df) <- c(paste(colnames(df)[i], level, sep=""))
        ifelse(nrow(new_df) == 0, new_df <- data.frame(n_df), new_df <- cbind(new_df, n_df))
      }
    }
    else{
      n_df <- data.frame(df[, i])
      colnames(n_df) <- colnames(train)[i]
      ifelse(nrow(new_df) == 0, new_df <- n_df, new_df <- cbind(new_df, n_df))
    } 
  }
  return(new_df)
}

removeNa <- function(df){
  # Remove col with more than 0.5 NA
  df <- df[, -which(colMeans(is.na(df)) > 0.5)]
  
  # mode of data.frame
  mode_ <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  replace_factor <- function(x) {
    if(is.factor(x)) replace(x, is.na(x), mode_(na.omit(x)))
    else if(is.numeric(x)) replace(x, is.na(x), mean(x, na.rm=TRUE))
    else x
  }
  
  # Replace:
  #  Factor NA  <-  Mode;
  #  Numeric NA <- Median;
  df[-1] <- lapply(df[-1], replace_factor)
  
  return(df)
}
