library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(xgboost)
library(rpart)
library(randomForest)
library(ROSE)
library(car)
library(caret)
library(missForest)

train <- read.csv("kaggle\\HousePricing 2\\train.csv")
test <- read.csv("kaggle\\HousePricing 2\\test.csv")

test_id <- test$Id
train$Id <- NULL
test$Id <- NULL

## mutate
train <- train %>% mutate_if(is.character, as.factor)
train <- train %>% mutate_if(is.integer, as.numeric)

test <- test %>% mutate_if(is.character, as.factor)
test <- test %>% mutate_if(is.integer, as.numeric)

## missForest
train <- missForest(train, maxiter=10, ntree=300, verbose=T)$ximp
test <- missForest(test, maxiter=10, ntree=500, verbose=T)$ximp
## or just mean/mode

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


## One hot encoding
dum_train <- dummies::dummy.data.frame(train, sep="_", dummy.classes = "factor")
dum_test <- dummies::dummy.data.frame(test, sep="_", dummy.classes = "factor")

## Remove vars that are not intercept
dum_test$SalePrice <- NA
cols_to_keep <- intersect(colnames(dum_train), colnames(dum_test))

vars_train <- dum_train[,cols_to_keep, drop=FALSE]
colnames(vars_train) <- gsub(" ", "_", colnames(vars_train), fixed=T)
colnames(vars_train) <- gsub("/", "_", colnames(vars_train), fixed=T)

vars_test <- dum_test[,cols_to_keep, drop=FALSE]
colnames(vars_test) <- gsub(" ", "_", colnames(vars_test), fixed=T)
colnames(vars_test) <- gsub("/", "_", colnames(vars_test), fixed=T)

## remove outliers
train <- train[train$SalePrice<=mean(train$SalePrice)+sd(train$SalePrice),]
boxplot(train$SalePrice)

## Brute Force 'Best' Formulae
model_full <- lm(SalePrice ~ ., data = log_train) 
model_null <- lm(SalePrice ~ 1, data = log_train)
scope = list(lower = model_null, upper = model_full) # Пространство моделей
ideal_model <- step(model_full, scope = scope, direction = 'backward')

## formula  of ideal_model
formulae <- (SalePrice ~ MSSubClass + `MSZoning_C_(all)` + MSZoning_FV + MSZoning_RL + 
               LotArea + Street_Grvl + LotShape_IR3 + LandContour_Bnk + 
               LotConfig_Corner + LotConfig_CulDSac + LotConfig_FR2 + LotConfig_FR3 + 
               Neighborhood_BrDale + Neighborhood_CollgCr + Neighborhood_Crawfor + 
               Neighborhood_Edwards + Neighborhood_Gilbert + Neighborhood_IDOTRR + 
               Neighborhood_MeadowV + Neighborhood_Mitchel + Neighborhood_NAmes + 
               Neighborhood_NoRidge + Neighborhood_NridgHt + Neighborhood_NWAmes + 
               Neighborhood_OldTown + Neighborhood_Sawyer + Neighborhood_SawyerW + 
               Neighborhood_StoneBr + Neighborhood_Timber + Condition1_Artery + 
               Condition1_Feedr + Condition1_RRAe + Condition1_RRAn + Condition2_PosA + 
               Condition2_PosN + OverallQual + OverallCond + YearBuilt + 
               YearRemodAdd + RoofStyle_Flat + RoofMatl_CompShg + `RoofMatl_Tar&Grv` + 
               RoofMatl_WdShake + RoofMatl_WdShngl + Exterior1st_AsbShng + 
               Exterior1st_BrkComm + Exterior1st_BrkFace + Exterior1st_HdBoard + 
               Exterior1st_MetalSd + Exterior1st_Plywood + Exterior1st_Stucco + 
               Exterior1st_WdShing + Exterior2nd_CmentBd + Exterior2nd_ImStucc + 
               Exterior2nd_VinylSd + Exterior2nd_Wd_Sdng + MasVnrType_BrkCmn + 
               ExterQual_Fa + ExterCond_Fa + Foundation_BrkTil + Foundation_CBlock + 
               Foundation_PConc + Foundation_Slab + Foundation_Stone + BsmtQual_Ex + 
               BsmtCond_Fa + BsmtExposure_Av + BsmtExposure_Gd + BsmtFinType1_BLQ + 
               BsmtFinType1_LwQ + BsmtFinSF1 + BsmtFinType2_BLQ + BsmtUnfSF + 
               TotalBsmtSF + Heating_GasW + Heating_Grav + HeatingQC_Ex + 
               HeatingQC_Gd + CentralAir_N + Electrical_FuseA + Electrical_FuseF + 
               X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + FullBath + 
               KitchenAbvGr + KitchenQual_Ex + Functional_Maj1 + Functional_Maj2 + 
               Functional_Min1 + Functional_Min2 + Functional_Mod + Functional_Sev + 
               Fireplaces + FireplaceQu_Po + GarageCars + GarageQual_Fa + 
               GarageQual_Gd + GarageQual_Po + GarageQual_TA + GarageCond_Ex + 
               WoodDeckSF + ScreenPorch + PoolQC_Ex + PoolQC_Gd + Fence_GdPrv + 
               Fence_MnPrv + SaleType_Con + SaleType_ConLD + SaleType_New + 
               SaleType_Oth + SaleCondition_AdjLand + SaleCondition_Alloca + 
               SaleCondition_Normal)
## divide train to test and train
index <- createDataPartition(vars_train$SalePrice,p=0.7,list=F)

vars_train_train <- vars_train[index,]
vars_test_train <- vars_train[-index,]

## lm
fit_reg <- lm(ideal_model, vars_train_train)

shapiro.test(fit_reg$residuals)
boxplot(fit_reg$residuals)
hist(fit_reg$residuals)
summary(fit_reg)

pred_lm <- predict(fit_reg, vars_test_train)
RMSE(vars_test_train$SalePrice, pred_lm)

## randomForest
rf <- randomForest(ideal_model, vars_train, type="regression", ntree=300, do.trace=T)
pred_rf <- predict(rf, vars_test_train, type="class")
RMSE(vars_test_train$SalePrice, pred_rf)

## XGBOOST
train_matrix <- data.matrix(select(vars_train_train, -SalePrice))
test_matrix <- data.matrix(select(vars_test_train, -SalePrice))

train_target <- vars_train_train$price
test_target <- vars_test_train$price

dtrain <- xgb.DMatrix(data=train_matrix, label=train_target)
ctest <- xgb.DMatrix(data=test_matrix, label=test_target)

watchlist <- list(train=dtrain, test=ctest)
bst <- xgb.train(data=dtrain,
                 nround=500,
                 maximize =F,
                 early_stopping_rounds = 10,
                 #nfold=6,
                 max_depth=7,
                 objective="reg:linear",
                 eval_metric = "rmse",
                 alpha=0.07,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample=0.7)

pred_xgboost <- predict(bst, ctest)
RMSE(test_target, pred_xgboost)