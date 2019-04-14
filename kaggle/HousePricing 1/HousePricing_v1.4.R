library(dplyr)

train <- read.csv("2018.06.20\\train.csv")

removeNa <- function(df){
  # Remove col with more than 0.5 NA
  #df <- df[, -which(colMeans(is.na(df)) > 0.5)]
  
  df <- df %>% mutate_if(is.character, as.factor)
  df <- df %>% mutate_if(is.integer, as.numeric)
  
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

# 'Smart' Remove all NA
train <- removeNa(train)
# Показать кол-во отсутствующих
sum(sapply(train,function(x){sum(is.na(x))}))

#train <- train[train$SalePrice<=mean(train$SalePrice)+sd(train$SalePrice),]
#boxplot(train$SalePrice)
summary(lm(SalePrice~.,train)) #0.9186

model_full <- lm(SalePrice ~ ., data = train) 
model_null <- lm(SalePrice ~ 1, data = train)
scope = list(lower = model_null, upper = model_full) # Пространство моделей
ideal_model <- step(model_full, scope = scope, direction = 'backward')

df_fac <- train[, sapply(test, function(x){is.factor(x)})]
df_num <- train[, sapply(test, function(x){is.numeric(x)})]
df_num <- sapply(df_num, function(x){log(abs(x)+1)})
train <- cbind(df_num, df_fac)

ideal_model_formula <- lm(SalePrice ~ MSSubClass + MSZoning + LotArea + Street + LandContour + 
                            Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
                            Condition2 + BldgType + OverallQual + OverallCond + YearBuilt + 
                            YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + MasVnrType + 
                            MasVnrArea + ExterQual + BsmtQual + BsmtCond + BsmtExposure + 
                            BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
                            X2ndFlrSF + FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
                            TotRmsAbvGrd + Functional + Fireplaces + GarageCars + GarageArea + 
                            GarageQual + GarageCond + WoodDeckSF + ScreenPorch + PoolArea + 
                            PoolQC + Fence + MoSold + SaleCondition, data = train)

test <- read.csv("2018.06.20\\test.csv")
test <- removeNa(test)

df_fac <- test[, sapply(test, function(x){is.factor(x)})]
df_num <- test[, sapply(test, function(x){is.numeric(x)})]
df_num <- sapply(df_num, function(x){log(abs(x)+1)})
test <- cbind(df_num, df_fac)

test$pred <- exp(predict(ideal_model_formula, test))
df_total$pred <- predict(ideal_model_formula, df_total)
output <- data.frame(Id = test$Id, SalePrice = test$pred)
write.csv(output, "output.csv", row.names = FALSE)



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

train_n <- create_df_without_factors(train)

colnames(train_n)




df <- train_n
df_fac <- df[, sapply(df, function(x){is.factor(x)})]
df_num <- df[, sapply(df, function(x){is.numeric(x)})]
df_num <- sapply(df_num, function(x){log(abs(x)+1)})
df_total <- cbind(df_num, df_fac)
df_total <- train_n

splitted_ideal_model_formula_total <- lm(SalePrice ~ +MSSubClass	+ MSZoningFV	+ MSZoningRH + MSZoningRL	+ MSZoningRM	+ LotArea	+ StreetPave	+ LandContourHLS	+ 	 LandContourLvl		+ LotConfigCulDSac	+ LotConfigFR2	+ 	 	 LandSlopeMod	+ LandSlopeSev	+ 	 	 	 NeighborhoodClearCr	+ NeighborhoodCollgCr	+ NeighborhoodCrawfor	+ NeighborhoodEdwards	+ NeighborhoodGilbert	+ 	 	 NeighborhoodMitchel	+ NeighborhoodNAmes	+ NeighborhoodNoRidge	+ NeighborhoodNPkVill	+ NeighborhoodNridgHt	+ NeighborhoodNWAmes	+ NeighborhoodOldTown	+ NeighborhoodSawyer	+ 	 	 NeighborhoodStoneBr	+ NeighborhoodSWISU	+ NeighborhoodTimber	+ 	 Condition1Feedr	+ Condition1Norm	+ 	 Condition1PosN	+ Condition1RRAe	+ Condition1RRAn	+ Condition2PosA	+ Condition2PosN	+ 	 	 BldgType2fmCon	+ 	 BldgTypeTwnhs	+ BldgTypeTwnhsE	+ OverallQual	+ OverallCond	+ YearBuilt	+ YearRemodAdd	+ 	 	 	 RoofStyleMansard	+ RoofStyleShed	+ RoofMatlCompShg	+ RoofMatlTarGrv	+ RoofMatlWdShake	+ RoofMatlWdShngl	+ 	 	 Exterior1stBrkFace		+ 	 Exterior1stHdBoard	+ 	 	 Exterior1stPlywood 	+ MasVnrTypeNone	+ MasVnrTypeStone	+ MasVnrArea	+ 	 ExterQualGd	+ ExterQualTA	+ BsmtQualFa	+ BsmtQualGd	+ BsmtQualTA	+ 	 BsmtCondPo	+ BsmtCondTA	+ BsmtExposureGd	+ BsmtExposureMn	+ BsmtExposureNo	+ BsmtFinType1BLQ	+ BsmtFinType1GLQ	+ 	 	 BsmtFinType1Unf	+ BsmtFinSF1	+ BsmtFinSF2	+ BsmtUnfSF	+ X1stFlrSF	+ X2ndFlrSF	+ FullBath	+ BedroomAbvGr	+ KitchenAbvGr	+ KitchenQualFa	+ KitchenQualGd	+ KitchenQualTA	+ TotRmsAbvGrd	+ 	 FunctionalMin1	+ FunctionalMin2	+ 	 FunctionalSev	+ FunctionalTyp	+ Fireplaces	+ GarageCars	+ GarageArea	+ GarageQualFa	+ GarageQualGd	+ GarageQualPo	+ GarageQualTA	+ GarageCondFa	+ GarageCondGd	+ GarageCondPo	+ GarageCondTA	+ WoodDeckSF	+ ScreenPorch	+ PoolArea	+ PoolQCGd	+ FenceGdWo	+ FenceMnPrv	+ 	 MoSold		+ 	 	 SaleConditionNormal	+ SaleConditionPartial
                                   ,data = df_total)

summary(splitted_ideal_model_formula_total)
# 0.9234
# UtilitiesNoSeWa 0.9233
# Condition2RRAe 0.9232
#RoofMatlMembran 0.917


test <- read.csv("2018.06.20\\test.csv")
test <- removeNa(test)
test <- create_df_without_factors(test)

df_fac <- test[, sapply(test, function(x){is.factor(x)})]
df_num <- test[, sapply(test, function(x){is.numeric(x)})]
df_num <- sapply(df_num, function(x){log(abs(x)+1)})
test <- cbind(df_num, df_fac)

test$pred <- predict(splitted_ideal_model_formula_total, test)
df_total$pred <- predict(splitted_ideal_model_formula_total, df_total)
output <- data.frame(Id = test$Id, SalePrice = test$pred)
write.csv(output, "output.csv", row.names = FALSE)





