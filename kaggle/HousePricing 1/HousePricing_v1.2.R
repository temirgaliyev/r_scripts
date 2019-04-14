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
test <- create_df_without_factors(test)
colnames(test)

fit <- lm(SalePrice~.,train) # R^2 0.9121
# without 50% / - 0.9143
summary(fit)


fit1 <- lm(SalePrice~.,train_for_test) # R^2 0.9121
summary(fit1)

# if R-sq big - hetero
check <- lm((fit$residuals)^2~.,train)
# without 50% / - 0.6042
summary(check) # 0.6227

library(ggplot2)
qplot(x=hp,y=mpg,data=mtcars)+ 
  geom_smooth(method = 'lm') + 
  theme(panel.background = element_rect(fill = 'gray'))

cor.test(train_for_test)

shapiro.test(fit$residuals)

train_for_test <- train[, intersect(colnames(train), colnames(test))]
train_for_test$SalePrice <- train$SalePrice

# Compute Optimal Formula
fit_full <- lm(SalePrice ~. , data = train_for_test)
optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit) # 0.9231 !!!!!!!!!

fit1 <- lm(SalePrice ~. , data = train_for_test)
summary(fit1)

summary(optimal_fit_formula)
optimal_fit_formula <- lm(SalePrice ~ MSZoning_RM + `MSZoning_C (all)` + MSZoning_FV + 
                            LotShape_Reg + LotShape_IR1 + Utilities_AllPub + LotConfig_Inside + 
                            LotConfig_Corner + LotConfig_CulDSac + LandSlope_Gtl + LandSlope_Mod + 
                            Neighborhood_Veenker + Neighborhood_Crawfor + Neighborhood_NoRidge + 
                            Neighborhood_Somerst + Neighborhood_NWAmes + Neighborhood_OldTown + 
                            Neighborhood_Sawyer + Neighborhood_NridgHt + Neighborhood_NAmes + 
                            Neighborhood_IDOTRR + Neighborhood_MeadowV + Neighborhood_Edwards + 
                            Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
                            Neighborhood_NPkVill + Neighborhood_Blmngtn + Condition1_Norm + 
                            Condition1_PosN + Condition1_PosA + Condition2_Artery + Condition2_PosN + 
                            BldgType_1Fam + BldgType_2fmCon + BldgType_Duplex + BldgType_TwnhsE + 
                            HouseStyle_2Story + HouseStyle_1Story + HouseStyle_1.5Fin + 
                            HouseStyle_1.5Unf + HouseStyle_SFoyer + HouseStyle_SLvl + 
                            RoofStyle_Gable + RoofStyle_Hip + RoofStyle_Mansard + RoofStyle_Flat + 
                            RoofMatl_CompShg + RoofMatl_WdShake + Exterior1st_VinylSd + 
                            Exterior1st_MetalSd + `Exterior1st_Wd Sdng` + Exterior1st_HdBoard + 
                            Exterior1st_WdShing + Exterior1st_Plywood + Exterior1st_AsbShng + 
                            Exterior1st_Stucco + Exterior1st_BrkComm + Exterior1st_CBlock + 
                            Exterior2nd_VinylSd + `Exterior2nd_Wd Sdng` + MasVnrType_BrkFace + 
                            MasVnrType_Stone + ExterQual_Gd + ExterCond_Fa + ExterCond_Po + 
                            Foundation_CBlock + Foundation_Slab + BsmtQual_Gd + BsmtQual_Ex + 
                            BsmtExposure_No + BsmtExposure_Gd + BsmtFinType1_GLQ + BsmtFinType1_ALQ + 
                            BsmtFinType1_Rec + BsmtFinType1_BLQ + BsmtFinType2_Unf + 
                            BsmtFinType2_BLQ + BsmtFinType2_ALQ + BsmtFinType2_Rec + 
                            BsmtFinType2_LwQ + Heating_GasW + Heating_Grav + HeatingQC_Gd + 
                            HeatingQC_TA + HeatingQC_Fa + CentralAir_Y + Electrical_SBrkr + 
                            Electrical_FuseF + Electrical_FuseA + KitchenQual_Gd + KitchenQual_TA + 
                            KitchenQual_Ex + Functional_Typ + Functional_Min1 + Functional_Maj1 + 
                            Functional_Min2 + Functional_Mod + GarageType_Attchd + GarageType_Basment + 
                            GarageFinish_Unf + GarageCond_Fa + PavedDrive_N + SaleType_COD + 
                            SaleType_ConLD + SaleCondition_Abnorml, data = train_for_test)


pred <- predict(optimal_fit_formula, test)
View(pred)
write.csv(pred, file = "kaggle_prediction_2.csv")


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
