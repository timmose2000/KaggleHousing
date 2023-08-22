library(stats)
library(dplyr)
library(tidyr)
library(ggplot2) 
library(onehot)  
library(MASS)
library(fastDummies)
library(moments)
library(car)
library(leaps)
library(glmnet)
library(MASS)
library(tree)

# Configure select() so that it works with MASS package loaded
select <- dplyr::select

#Read the training set into R
raw_train_data <-read.csv("train.csv")
raw_test_data <-read.csv("test.csv")
raw_test_data$SalePrice <- "NA"

combined <- rbind(raw_train_data,raw_test_data)

#Set the seed to create reproducible results 
set.seed(123)

##COLUMN 1: Id
sum(is.na(combined$Id))

##COLUMN 2: MSSubClass
sum(is.na(combined$MSSubClass))
combined$MSSubClass <- as.factor(combined$MSSubClass)
combined <- dummy_cols(combined, select_columns = "MSSubClass")

##COLUMN 3: MSZoning
#4 NA Values
sum(is.na(combined$MSZoning))
#replaced NA with mode
combined$MSZoning[is.na(combined$MSZoning)] <- "RL"
levels(combined$MSZoning)[levels(combined$MSZoning) == "C (all)"] <- "C"
combined$MSZoning <- as.factor(combined$MSZoning)

##COLUMN 4: LotFrontage
combined$LotFrontage <- as.numeric(combined$LotFrontage)
sum(is.na(combined$LotFrontage))
#486 NA values
combined %>%
  group_by(MSZoning) %>%
  summarize(mean_LotFrontage = mean(LotFrontage, na.rm = TRUE), .groups = "drop")

#Impute the mean LotFrontage by MSZoning into all NAs of LotFrontage
combined$LotFrontage[which((combined$MSZoning == "C" & is.na(combined$LotFrontage)))] <- 65.6
combined$LotFrontage[which((combined$MSZoning == "FV" & is.na(combined$LotFrontage)))] <- 59.5
combined$LotFrontage[which((combined$MSZoning == "RH" & is.na(combined$LotFrontage)))] <- 55.4
combined$LotFrontage[which((combined$MSZoning == "RL" & is.na(combined$LotFrontage)))] <- 74.1
combined$LotFrontage[which((combined$MSZoning == "RM" & is.na(combined$LotFrontage)))] <- 52.2
combined$LotFrontage[is.na(combined$LotFrontage)] <- mean(combined$LotFrontage, na.rm = TRUE)

##COLUMN 5: LotArea
combined$LotArea <- as.numeric(combined$LotArea)
combined$LotArea <- ifelse(combined$LotArea > 100000, mean(combined$LotArea), combined$LotArea)

##COLUMN 6: Street
#0 NA Values
sum(is.na(combined$Street))
combined$Street <- as.factor(combined$Street)

##COLUMN 7: Alley
sum(is.na(combined$Alley))
#1369 NA <-- will clean
combined$Alley[is.na(combined$Alley)] <- "No alley access"
combined$Alley <- as.factor(combined$Alley)


##COLUMN 8: LotShape
#0 NA
sum(is.na(combined$LotShape))
combined$LotShape<- factor(combined$LotShape, order = TRUE, 
                                     levels = c("IR3", "IR2", "IR1", "Reg"))

##COLUMN 9: LandContour
sum(is.na(combined$LandContour))
combined$LandContour <- as.factor(combined$LandContour)

##COLUMN 10: Utilities
summary(factor(combined$Utilities))
#2 NA values, 1 NOSeWa and rest AllPub --> will NOT include in final model

##COLUMN 11: LotConfig
sum(is.na(combined$LotConfig)) #0 NA Values
combined$LotConfig <- as.factor(combined$LotConfig)

##COLUMN 12: LandSlope
sum(is.na(combined$LandSlope)) #0 NA
summary(factor(combined$LandSlope)) #Almost entirely Gtl --> will NOT include in final model

##COLUMN 13: Neighborhood
sum(is.na(combined$Neighborhood)) #0 NA Values
combined$Neighborhood <- as.factor(combined$Neighborhood)

##COLUMN 14: Condition1
sum(is.na(combined$Condition1)) #0 NA Values
summary(factor(combined$Condition1)) #Everything looks fine
combined$Condition1 <- as.factor(combined$Condition1)

##COLUMN 15: Condition2
sum(is.na(combined$Condition2)) #0 NA values
summary(factor(combined$Condition2)) #99% same values --> will NOT include in final model

##COLUMN 16: BldgType
sum(is.na(combined$BldgType)) #0 NA Values
summary(factor(combined$BldgType)) #Majority 1Fam, most likely fine still
combined$BldgType[combined$BldgType == "Twnhs"] <- "TwnhsI" #replacing Twnhs with TwnhsI due to data description
combined$BldgType<-as.factor(combined$BldgType)

##COLUMN 17: HouseStyle
sum(is.na(combined$HouseStyle)) #0 NA
summary(factor(combined$HouseStyle)) #everything looks fine
combined$HouseStyle <- as.factor(combined$HouseStyle)

##COLUMN 18: OverallQual
sum(is.na(combined$OverallQual)) #0 NA
summary(factor(combined$OverallQual)) #Looks fine, but will factor
combined$OverallQual <- factor(combined$OverallQual, order = TRUE, 
                                         levels = c("1", "2", "3","4", "5", "6", "7", "8","9","10"))

##COLUMN 19: OverallCond
sum(is.na(combined$OverallCond)) #0 NA values
summary(factor(combined$OverallCond)) #looks similar to overall qual. Distribution is expected
combined$OverallCond <- factor(combined$OverallCond, order = TRUE, 
                                         levels = c("1", "2", "3","4", "5", "6", "7", "8","9","10"))

##COLUMN 20: YearBuilt
sum(is.na(combined$YearBuilt)) #0 NA values
max(combined$YearBuilt) #highest value is 2010
combined$HomeAge <- (2010-combined$YearBuilt) #converted to number column (dataset was taken in 2010)

##COLUMN 21: YearRemodAdd
sum(is.na(combined$YearRemodAdd)) #0 NA
max(combined$YearRemodAdd) #highest value is 2010
combined$RemodAge <- (2010-combined$YearRemodAdd)

##COLUMN 22: RoofStyle
sum(is.na(combined$RoofStyle)) #0 NA
summary(factor(combined$RoofStyle)) #Majority Gable, otherwise fine
combined$RoofStyle <- as.factor(combined$RoofStyle)

##COLUMN 23: RoofMatl
sum(is.na(combined$RoofMatl)) #0 NA values
summary(factor(combined$RoofMatl)) #Almost entirely CompShg --> will NOT include in model

##COLUMN 24: Exterior1st
sum(is.na(combined$Exterior1st)) #1 NA Value
summary(factor(combined$Exterior1st)) #VinylSd is the mode, will use to repalce NA's
combined$Exterior1st[is.na(combined$Exterior1st)] <- "VinylSd"
combined$Exterior1st <- as.factor(combined$Exterior1st)

##COLUMN 25: Exterior2nd
sum(is.na(combined$Exterior2nd)) #1 NA again
summary(factor(combined$Exterior2nd))
combined$Exterior2nd[is.na(combined$Exterior2nd)] <- "VinylSd"
combined$Exterior2nd <- as.factor(combined$Exterior2nd)

##COLUMN 26: MasVnrType
sum(is.na(combined$MasVnrType)) #24 NA Values
summary(factor(combined$MasVnrType)) #should replace NA with none
combined$MasVnrType[is.na(combined$MasVnrType)] <- "None"
combined$MasVnrType <- as.factor(combined$MasVnrType)

##COLUMN 27: MasVnrArea
sum(is.na(combined$MasVnrArea)) #23 NA values, same as last time. Weird that one less...
combined$MasVnrArea[is.na(combined$MasVnrArea)] <- 0 #missing values most likely same as None in last one. Makes sense that area should be 0
ggplot(combined, aes(SalePrice, MasVnrArea)) + geom_density() #Outliers seem to occur around 1500.Replacing outliers with mean.
combined$MasVnrArea <- ifelse(combined$MasVnrArea > 1500, mean(combined$MasVnrArea), combined$MasVnrArea)
skewness(combined$MasVnrArea) #2.61 skewness
skewness(log(combined$MasVnrArea+0.001)) #0.447084 skewness
combined$MasVnrArea <- log(combined$MasVnrArea+0.001)

##COLUMN 28: ExterQual
sum(is.na(combined$ExterQual)) #0 missing value 
summary(factor(combined$ExterQual)) #no typos
combined$ExterQual <- factor(combined$ExterQual, order = TRUE, levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 29: ExterCond
sum(is.na(combined$ExterCond))
summary(factor(combined$ExterCond)) #0 missing value/typo
combined$ExterCond <- factor(combined$ExterCond, order = TRUE, levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 30: Foundation
sum(is.na(combined$Foundation)) #0 NA
summary(factor(combined$Foundation)) #no issues
combined$Foundation <- as.factor(combined$Foundation)

##COLUMN 31: BsmtQual
sum(is.na(combined$BsmtQual)) #81 NA
summary(factor(combined$BsmtQual)) #likely NA means no basement
combined$BsmtQual[is.na(combined$BsmtQual)] <- "No Basement" 
combined$BsmtQual <- factor(combined$BsmtQual, order = TRUE, 
                                      levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))

#44 NA Values 
sum(is.na(combined$BsmtQual)) #44 NA
combined$BsmtQual[is.na(combined$BsmtQual)] <- "No Basement"
combined$BsmtQual <- factor(combined$BsmtQual, order = TRUE, 
                                     levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 32: BsmtCond
sum(is.na(combined$BsmtCond))
summary(factor(combined$BsmtCond)) #82 NA values again --> will replace with no basement
combined$BsmtCond[is.na(combined$BsmtCond)] <- "No Basement"
combined$BsmtCond <- factor(combined$BsmtCond, order = TRUE, 
                                      levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 33: BsmtExposure
sum(is.na(combined$BsmtExposure)) #again 82 NA --> same cleaning procedure
summary(factor(combined$BsmtExposure))
combined$BsmtExposure[is.na(combined$BsmtExposure)] <- "No Basement"
combined$BsmtExposure <- factor(combined$BsmtExposure, order = TRUE, 
                                          levels = c("No Basement","No", "Mn", "Av", "Gd"))

##COLUMN 34:BsmtFinType1
sum(is.na(combined$BsmtFinType1)) #79 NA values
summary(factor(combined$BsmtFinType1)) #no issues
combined$BsmtFinType1[is.na(combined$BsmtFinType1)] <- "No Basement"
combined$BsmtFinType1 <- factor(combined$BsmtFinType1, order = TRUE, 
                                          levels = c("No Basement","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))

##COLUMN 35:BsmtFinSF1
combined$BsmtFinSF1 <- as.numeric(combined$BsmtFinSF1)
sum(is.na(combined$BsmtFinSF1)) #1 NA value
ggplot(data = combined, aes(SalePrice , BsmtFinSF1)) + geom_point() #outliers above 2000
combined$BsmtFinSF1 <- ifelse(combined$BsmtFinSF1 > 2000, 
                                        mean(combined$BsmtFinSF1),
                                        combined$BsmtFinSF1)
combined$BsmtFinSF1[is.na(combined$BsmtFinSF1)] <- mean(combined$BsmtFinSF1, na.rm = TRUE) 

##COLUMN 36: BsmtFinType2
sum(is.na(combined$BsmtFinType2)) #80 NA
summary(factor(combined$BsmtFinType2)) #replacing NA with no basement, like in previous cols
combined$BsmtFinType2[is.na(combined$BsmtFinType2)] <- "No Basement"
combined$BsmtFinType2 <- factor(combined$BsmtFinType2, order = TRUE, 
                                          levels = c("No Basement","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

##COLUMN 37: BsmtFinSF2
combined$BsmtFinSF2 <- as.numeric(combined$BsmtFinSF2)
sum(is.na(combined$BsmtFinSF2)) #1 NA
skewness(combined$BsmtFinSF2, na.rm = TRUE) #4.145323 skewness is VERY high --> will NOT include in final model

##COLUMN 38: BsmtUnfSF
combined$BsmtUnfSF <- as.numeric(combined$BsmtUnfSF)
sum(is.na(combined$BsmtUnfSF)) #1 NA 
combined$BsmtUnfSF[is.na(combined$BsmtUnfSF)] <- mean(combined$BsmtUnfSF, na.rm = TRUE)
ggplot(data = combined, aes(BsmtUnfSF)) + geom_histogram()
skewness(combined$BsmtUnfSF) #The value is 0.918, moderately skewed.
skewness(combined$BsmtUnfSF^(1/1.7)) #new skewness is 0.04500765

##COLUMN 39: TotalBsmtSF
combined$TotalBsmtSF <- as.numeric(combined$TotalBsmtSF)
sum(is.na(combined$TotalBsmtSF)) #1 NA
combined$TotalBsmtSF[is.na(combined$TotalBsmtSF)] <- mean(combined$TotalBsmtSF, na.rm = TRUE)
ggplot(data = combined, aes(SalePrice, TotalBsmtSF)) + geom_point() #some outliers
combined$TotalBsmtSF <- ifelse(combined$TotalBsmtSF > 3500,
                                         mean(combined$TotalBsmtSF),
                                         combined$TotalBsmtSF)

combined <- combined %>% 
  mutate(Bsmt = ifelse(TotalBsmtSF > 0, "yes", "no")) #will later add second floor, first floor, basement together. Will then remove all from the dataframe

combined$Bsmt<-as.factor(combined$Bsmt)

##COLUMN 40: Heating
sum(is.na(combined$Heating)) #0 NA
summary(factor(combined$Heating)) #~99% GasA --> will REMOVE

##COLUMN 41: HeatingQC
sum(is.na(combined$HeatingQC)) #0 NA
summary(factor(combined$HeatingQC)) #no issues
combined$HeatingQC <- factor(combined$HeatingQC, order = TRUE, 
                                       levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 42: CentralAir
sum(is.na(combined$CentralAir))#0 NA
summary(combined$CentralAir) #looks fine. Mostly Y, but decent amount of N
combined$CentralAir <- as.factor(combined$CentralAir)

##COLUMN 43: Electrical
sum(is.na(combined$Electrical)) #1 NA value
summary(factor(combined$Electrical))#mostly SBrkr
combined$Electrical[is.na(combined$Electrical)] <- "SBrkr"
combined$Electrical <- as.factor(combined$Electrical)

##COLUMN 44: X1stFlrSF
combined$X1stFlrSF <- as.numeric(combined$X1stFlrSF)
sum(is.na(combined$X1stFlrSF)) #0 NA
ggplot(data = combined, aes(SalePrice, X1stFlrSF)) + geom_point()#Outliers seem to be around 3000.
combined$X1stFlrSF <- ifelse(combined$X1stFlrSF > 3000,mean(combined$X1stFlrSF),combined$X1stFlrSF) #replacing outliers with mean

##COLUMN 45: X2ndFlrSF
combined$X2ndFlrSF <- as.numeric(combined$X2ndFlrSF)
sum(is.na(combined$X2ndFlrSF)) #0 NA
ggplot(data = combined, aes(SalePrice, X2ndFlrSF)) + geom_point()#Outliers seem to be around 2000.
combined$X2ndFlrSF <- ifelse(combined$X2ndFlrSF > 2000,mean(combined$X2ndFlrSF),combined$X2ndFlrSF) #replacing outliers with the mean
combined <- combined %>% 
  mutate(secondFlr = ifelse(X2ndFlrSF > 0, "yes", "no")) #will later add second floor, first floor, basement together. Will then remove all from the dataframe

combined$secondFlr<-as.factor(combined$secondFlr)
##COLUMN 46: LowQualFinSF
combined$LowQualFinSF <- as.numeric(combined$LowQualFinSF)
sum(is.na(combined$LowQualFinSF)) #0 NA
ggplot(data = combined, aes(SalePrice, LowQualFinSF)) + geom_point() #almost all values are 0 --> will remove

##COLUMN 47: GrLivArea
combined$GrLivArea <- as.numeric(combined$GrLivArea)
sum(is.na(combined$GrLivArea))#0 NA
ggplot(data = combined, aes(SalePrice, GrLivArea)) + geom_point() #outliers at 4000+
combined$GrLivArea <- ifelse(combined$GrLivArea > 4000,mean(combined$GrLivArea),combined$GrLivArea)
skewness(combined$GrLivArea) #0.8749109 somewhat skewed

##COLUMN 48: BsmtFullBath
combined$BsmtFullBath <- as.numeric(combined$BsmtFullBath)
sum(is.na(combined$BsmtFullBath)) #2 NA values
summary(factor(combined$BsmtFullBath))
combined$BsmtFullBath[is.na(combined$BsmtFullBath)] <- 0 #replacing NA with mode

##COLUMN 49: BsmtHalfBath
combined$BsmtHalfBath <- as.numeric(combined$BsmtHalfBath)
sum(is.na(combined$BsmtHalfBath)) #2 NA values
summary(factor(combined$BsmtHalfBath))
combined$BsmtHalfBath[is.na(combined$BsmtHalfBath)] <- 0 #replacing NA with mode

##COLUMN 50: FullBath
combined$FullBath <- as.numeric(combined$FullBath)
sum(is.na(combined$FullBath)) #0 NA
summary(factor(combined$FullBath)) #looks fine, although most values are 1-2

##COLUMN 51: HalfBath
combined$HalfBath <- as.numeric(combined$HalfBath)
sum(is.na(combined$HalfBath)) #0 NA
summary(factor(combined$HalfBath)) #looks fine

##COLUMN 52: BedroomAbvGr
combined$BedroomAbvGr <- as.numeric(combined$BedroomAbvGr)
sum(is.na(combined$BedroomAbvGr)) #0 NA values
summary(factor(combined$BedroomAbvGr)) #looks fine
skewness(combined$BedroomAbvGr) #The value is  0.3263243, approximately symmetrical.

##COLUMN 53: KitchenAbvGr
combined$KitchenAbvGr <- as.numeric(combined$KitchenAbvGr)
sum(is.na(combined$KitchenAbvGr)) #0 NA values
summary(factor(combined$KitchenAbvGr)) #almost all 1 --> will remove in final models

##COLUMN 54: KitchenQual
sum(is.na(combined$KitchenQual)) #1 NA
summary(factor(combined$KitchenQual)) 
combined$KitchenQual[is.na(combined$KitchenQual)] <- "Gd"
combined$KitchenQual <- factor(combined$KitchenQual, order = TRUE, 
                                         levels = c("Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 55: TotRmsAbvGrd
combined$TotRmsAbvGrd <- as.numeric(combined$TotRmsAbvGrd)
sum(is.na(combined$TotRmsAbvGrd)) #0 NA
summary(factor(combined$TotRmsAbvGrd)) #does not look too skewed

##COLUMN 56: Functional
sum(is.na(combined$Functional)) #2 NA values
summary(factor(combined$Functional))
combined$Functional[is.na(combined$Functional)] <- "Typ"
combined$Functional <- factor(combined$Functional, order = TRUE, 
                                        levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod","Min2","Min1","Typ"))

##COLUMN 57: Fireplaces
combined$Fireplaces <- as.numeric(combined$Fireplaces)
sum(is.na(combined$Fireplaces)) #0 NA
summary(factor(combined$Fireplaces)) #looks slightly skewed towards left 

##COLUMN 58: FireplaceQu
sum(is.na(combined$FireplaceQu)) #1420 NA --> no fireplace
summary(factor(combined$FireplaceQu))
combined$FireplaceQu[is.na(combined$FireplaceQu)] <- "No fireplace"
combined$FireplaceQu <- factor(combined$FireplaceQu, order = TRUE, 
                                         levels = c("No fireplace", "Po", "Fa", "TA", "Gd", "Ex")) #factor

##COLUMN 59: GarageType
sum(is.na(combined$GarageType)) #157 NA values
summary(factor(combined$GarageType))
combined$GarageType[is.na(combined$GarageType)] <- "No Garage"
combined$GarageType <- as.factor(combined$GarageType)

##COLUMN 60: GarageYrBlt
sum(is.na(combined$GarageYrBlt)) #159 NA, however this should be almost 100% correlated with Year Built variable --> will remove

##COLUMN 61: GarageFinish
sum(is.na(combined$GarageFinish)) #159 NA values
summary(factor(combined$GarageFinish))
combined$GarageFinish[is.na(combined$GarageFinish)] <- "No Garage"
combined$GarageFinish <- factor(combined$GarageFinish, order = TRUE, 
                                          levels = c("No Garage", "Unf", "RFn", "Fin"))

##COLUMN 62: GarageCars
combined$GarageCars <- as.numeric(combined$GarageCars)
sum(is.na(combined$GarageCars)) #1 NA
summary(factor(combined$GarageCars)) #will replace NA with 2
combined$GarageCars[is.na(combined$GarageCars)] <- 2
ggplot(data = combined, aes(SalePrice, GarageCars)) + geom_point() #no obvious outliers --> maybe 5 cars?
skewness(combined$GarageCars)
#The value is -0.342, approximately symmetrical.

##COLUMN 63: GarageArea
combined$GarageArea <- as.numeric(combined$GarageArea)
sum(is.na(combined$GarageArea)) #1 NA value, will replace with mean
combined$GarageArea[is.na(combined$GarageArea)] <- mean(combined$GarageArea, na.rm  =TRUE)
ggplot(data = combined, aes(SalePrice, GarageArea)) + geom_point() #outliers at 1300
combined$GarageArea <- ifelse(combined$GarageArea > 1300,mean(combined$GarageArea),combined$GarageArea)
skewness(combined$GarageArea) #The value is  0.1143002 approximately symmetrical.

##COLUMN 64: GarageQual
sum(is.na(combined$GarageQual)) #159 NA
summary(factor(combined$GarageQual))
combined$GarageQual[is.na(combined$GarageQual)] <- "No Garage" #Na means no garage most likely
combined$GarageQual <- factor(combined$GarageQual, order = TRUE, 
                                        levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 65: GarageCond
sum(is.na(combined$GarageCond))
summary(factor(combined$GarageCond)) #81 NAs but they mean "No Garage" not missing values.
combined$GarageCond[is.na(combined$GarageCond)] <- "No Garage" #Replace NA with "No Garage".
combined$GarageCond <- factor(combined$GarageCond, order = TRUE, 
                                        levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex"))

##COLUMN 66: PavedDrive
sum(is.na(combined$PavedDrive)) #No missing valua or typos.
summary(factor(combined$PavedDrive))#looks fine
combined$PavedDrive <- as.factor(combined$PavedDrive)

##COLUMN 67: WoodDeckSF
combined$WoodDeckSF <- as.numeric(combined$WoodDeckSF)
sum(is.na(combined$WoodDeckSF)) #0 NA
ggplot(data = combined, aes(SalePrice, WoodDeckSF)) + geom_point() #outliers seem to be 725+
combined$WoodDeckSF <- ifelse(combined$WoodDeckSF > 725,mean(combined$WoodDeckSF),combined$WoodDeckSF)
skewness(combined$WoodDeckSF) #skewness is 1.398699
skewness(combined$WoodDeckSF^(1/2)) #skewness 0.4690609, slightly better
combined$WoodDeckSF<-combined$WoodDeckSF^(1/2)

##COLUMN 68: OpenPorchSF
combined$OpenPorchSF <- as.numeric(combined$OpenPorchSF)
sum(is.na(combined$OpenPorchSF)) #0 NA values
ggplot(data = combined, aes(SalePrice, OpenPorchSF)) + geom_point() #Outliers seem to be around 500.
combined$OpenPorchSF <- ifelse(combined$OpenPorchSF > 500,mean(combined$OpenPorchSF),combined$OpenPorchSF) #outliers replaced by mean
skewness(combined$OpenPorchSF) #Skewness is 2.535114, will try to reduce
skewness(combined$OpenPorchSF^(1/3)) #skewness is  0.1647242
combined$OpenPorchSF <- combined$OpenPorchSF^(1/3)

##COLUMN 69: EnclosedPorch
combined$EnclosedPorch <- as.numeric(combined$EnclosedPorch)
sum(is.na(combined$EnclosedPorch)) #0 NA values
ggplot(data = combined, aes(SalePrice, EnclosedPorch)) + geom_point() #Outliers seem to be around 500.
combined$EnclosedPorch <- ifelse(combined$EnclosedPorch > 500,mean(combined$EnclosedPorch),combined$EnclosedPorch) #Replace outliers with mean.
skewness(combined$EnclosedPorch) #skewness is 4.003891
ggplot(data = combined, aes(EnclosedPorch)) + geom_density() #cannot transform due to distribution

##COLUMN 70: X3SsnPorch
combined$X3SsnPorch <- as.numeric(combined$X3SsnPorch)
sum(is.na(combined$X3SsnPorch)) #0 NA values
ggplot(data = combined, aes(SalePrice, X3SsnPorch)) + geom_point() #Outliers seem to be around 500.
combined$X3SsnPorch <- ifelse(combined$X3SsnPorch > 500,mean(combined$X3SsnPorch),combined$X3SsnPorch)
skewness(combined$X3SsnPorch) #skewness of 10.29375 --> VERY high

##COLUMN 71: ScreenPorch
combined$ScreenPorch <- as.numeric(combined$ScreenPorch)
sum(is.na(combined$ScreenPorch)) #0 NA 
ggplot(data = combined, aes(SalePrice, ScreenPorch)) + geom_point() #no obvious outliers
skewness(combined$ScreenPorch) #very high skewness --> will transform later after combination

##COLUMN 72: PoolArea
sum(is.na(combined$PoolArea)) #0 NA
summary(factor(combined$PoolArea)) #99.9% have no pool --> will remove

##COLUMN 73: PoolQC
sum(is.na(combined$PoolQC))
summary(factor(combined$PoolQC)) #same as PoolArea --> will be removed

##COLUMN 73: Fence
sum(is.na(combined$Fence)) #2348 NA
summary(factor(combined$Fence))
combined$Fence[is.na(combined$Fence)] <- "No Fence"
combined$Fence <- as.factor(combined$Fence)

##COLUMN 75: MiscFeature
sum(is.na(combined$MiscFeature)) #2814 NA
summary(factor(combined$MiscFeature)) #almost all NA, will remove

##COLUMN 76: MiscVal
sum(is.na(combined$MiscVal)) #0 NA values
ggplot(data = combined, aes(SalePrice, MiscVal)) + geom_point() #almost all 0, will remove in final model

##COLUMN 77: MoSold
sum(is.na(combined$MoSold)) #0 NA
combined$MoSold <- as.factor(combined$MoSold) #makes sense to have as categorical

##COLUMN 78: YrSold
sum(is.na(combined$YrSold)) #0 NA

##COLUMN 79: SaleType
sum(is.na(combined$SaleType)) #1 NA
summary(factor(combined$SaleType)) #will replace the NA with WD (mode)
combined$SaleType[is.na(combined$SaleType)] <- "WD"
combined$SaleType <- as.factor(combined$SaleType)

##COLUMN 80: SaleCondition
sum(is.na(combined$SaleCondition)) #0 NA
summary(factor(combined$SaleCondition)) #No issues
combined$SaleCondition <- as.factor(combined$SaleCondition)

##COLUMN 81: SalePrice
sum(is.na(combined$SalePrice)) #0 NA (there are all the test values labeled NA, but they are not "technically" missing since I inputted them as NA)
skewness(raw_train_data$SalePrice) #1.880941 is skewness of train (test not included)
skewness(log(raw_train_data$SalePrice, 10)) #0.121 is skewness of train (test not included)

##################################################### Additional Columns #############################################################################
#combine living area variables
combined$HomeTotalSF <- combined$TotalBsmtSF + combined$X1stFlrSF + combined$X2ndFlrSF
skewness(combined$HomeTotalSF) #skewness is 0.8396227 --> could be improved slightly, also will remove all three variables that make up this variable
ggplot(combined, aes(HomeTotalSF))+geom_density() #maybe slight transformation
skewness(combined$HomeTotalSF^(1/3.2)) #Skewness very small with this at 0.0003212036
combined$HomeTotalSF <- combined$HomeTotalSF^(1/3.2)

#Combine porch variables
combined$TotalPorchSF <- combined$OpenPorchSF + combined$EnclosedPorch + combined$X3SsnPorch + combined$ScreenPorch
skewness(combined$TotalPorchSF) #skewness at 2.534624
ggplot(combined, aes(TotalPorchSF))+geom_density()
skewness(log10(combined$TotalPorchSF+0.01)) #skewness much lower at -0.3205288
ggplot(combined, aes(log10(TotalPorchSF+0.01)))+geom_density() #does not look like I can use this transformation --> multiple peaks

#Combine bath variables
combined$TotalFullBath <- combined$BsmtFullBath + combined$FullBath 
skewness(combined$TotalFullBath) #skewness is fine at 0.4485035
ggplot(combined, aes(TotalFullBath))+geom_density() #does not make sense to transform 

#Combine half bath variables
combined$TotalHalfBath <- combined$BsmtHalfBath + combined$HalfBath
skewness(combined$TotalHalfBath) #skewness 0.7991092
skewness(log(combined$TotalHalfBath+0.01)) #skewness is 0.341197
combined$TotalHalfBath<-log(combined$TotalHalfBath+0.01)

##################################################### Dropping unnecessary columns and plainvanilla ####################################################################
combined_drop <- combined
#dropping all columns that were found not to be necessary during cleaning. This includes MSSubClass due to me using dummycols on it. Also GrLivArea is replaced by HomeSF
combined_drop <- combined_drop[, -which(colnames(combined_drop) %in% c("Utilities", "LandSlope", "Condition2", "YearBuilt", "YearRemodAdd", "RoofMatl", "BsmtFinSF2", "Heating", "LowQualFinSF", "KitchenAbvGr", "GarageYrBlt", "PoolArea", "PoolQC", 
                                                                       "MiscFeature", "MiscVal", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "OpenPorchSF","EnclosedPorch", "X3SsnPorch","ScreenPorch", "HalfBath", "BsmtHalfBath", 
                                                                       "BsmtFullBath","FullBath","Id","MSSubClass","GrLivArea","BsmtCond","Exterior2nd", "BsmtFinType1","GarageCond","GarageFinish", "GarageType","Bsmt","Electrical"))] 

#Splitting back into test and train datasets
drop_train <- combined_drop[1:1460,]
drop_train$SalePrice<- (as.numeric(drop_train$SalePrice))
drop_test <- combined_drop[1461:2919,]

#creating plainvanilla model
plainvanilla <- lm(log10(SalePrice) ~., data = drop_train)
summary(plainvanilla)
plot(plainvanilla)

drop_test$SalePrice <- 10^(predict(plainvanilla, drop_test))
mean(drop_test$SalePrice)

test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<-drop_test$SalePrice



# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictionsNew.xlsx")
#0.13122 kaggle score for model plainvanilla


##################################################### Performing Diagnostic tests ####################################################################

#1 - Multicolinearity
#vif(plainvanilla)

alias_table <- alias(plainvanilla)
# Print the highest aliases
print(alias_table)
tail(alias_table)
#BsmtCond^4 is giving issues. It has an alias of 1 with BsmtQual^4, which is causing an error. I will remove BsmtQual or BsmtCond, since they are
#highly correlated either way. There are a few other highly aliased variables as well. "BsmtCond", 
#"BsmtFinType1","GarageCond","GarageFinish", "GarageType","Bsmt","Electrical", and "Exterior2nd. I already removed these in the combined_drop columns dropped
#to simplify the code. After removing these, there was still an issue with aliases due to MSSubClass dummy cols. Some of these had perfect negative
#correlations with each other. However, this is because they are dummycols. To test vif, I had to remove this column and test again
combined_vif <- combined_drop
combined_vif <- combined_vif[,-c(48:63)]
train_vif<- combined_vif[1:1460,]
train_vif$SalePrice<- (as.numeric(train_vif$SalePrice))
plainvanilla_vif <- lm(log10(SalePrice) ~., data = train_vif)
alias_table <- alias(plainvanilla_vif)
print(alias_table)
#now there are NO more aliased variables

vif(plainvanilla_vif)
#None of the variables had GVIF above 10. Some were close such as BsmtQual. However, since this is a factored variable, the multiple levels will have
#a correlation with each other, which inflates the GVIF. Therefore we must use the GVIF^(1/(2*Df)) to account for the differing degrees of freedom
#None of the high GVIF had a GVIF^(1/(2*Df)) above 4. Accounting for degrees of freedom, no more variables have to be removed.

#2 - Heteroskedasticity
#Using the residuals vs fitted, we can see that the points show no real discernible pattern. It is not perfectly uniform, but there are many points
#and many variables, so this is expected
#no prescriptions will be issued

#3 - non-linearity
#When plotting residuals vs fitted, the red line is almost perfectly flat, which indicates that there is no non-linearity issue
#no prescriptions will be issued

#4 - Influential points
#There are a few points with a cook's distance above 1. These are 186, 251, 326, 333, 398, 594, 665, 820, 947, 1010, 1186, 1369. 
#Initially, I wanted to remove points, however, after testing, I found out that almost each of these points was due to a factored categorical variable 
#having few levels. For example, OverallCond only had 4 total observations for a score of 1 in the combined dataset. When I removed points from
#drop_train, it gave me an error when creating models because there were new levels in the test dataset. This happened with many of these points
#and replacing them with the median made my model predictions significantly worse. Therefore, I chose to keep these points.




##################################################### Forward AIC ####################################################################
null_model <- lm(log10(SalePrice) ~ 1, data = drop_train) 
Forward_AIC <- stepAIC(null_model, direction = "forward", 
                       scope = list(lower = null_model, upper = plainvanilla, data = drop_train, k = 2))
summary(Forward_AIC) 
Forward_AIC_predict <- 10^predict(Forward_AIC, drop_test)
mean(Forward_AIC_predict)

test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<- Forward_AIC_predict

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_Forward2.xlsx")
#KaggleScore <- 0.13099

##################################################### Backward AIC ####################################################################
Backward_AIC <- stepAIC(plainvanilla, direction = "backward", k = 2)
summary(Backward_AIC)
Backward_AIC_predict <- 10^predict(Backward_AIC, drop_test)
mean(Backward_AIC_predict)


test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<-Backward_AIC_predict

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_backwardFinal.xlsx")
#Kaggle Score of 0.13022


##################################################### Hybrid AIC ####################################################################
null_model <- lm(log10(SalePrice) ~ 1, data = drop_train) 
Hybrid <- stepAIC(null_model, direction = "both", 
                       scope = list(lower = null_model, upper = plainvanilla, data = drop_train, k = 2))
summary(Hybrid) 
Hybrid <- 10^predict(Hybrid, drop_test)
mean(Hybrid)

test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<-Hybrid

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_Hybrid2.xlsx")
#Same model as forward... - 0.13099

##################################################### Ridge Regression ####################################################################
x_train <- model.matrix(log10(SalePrice) ~., data = drop_train)
y_train <- log10(drop_train$SalePrice)
x_test <- model.matrix((SalePrice) ~., data = drop_test)
y_test <- (drop_test$SalePrice)
set.seed(1)

cv_out_ridge <- cv.glmnet(x_train, y_train, alpha = 0, 
                          type.measure = "mse", nfolds = 10)
plot (cv_out_ridge)
bestlam <- cv_out_ridge$lambda.min
bestlam #0.01566057

model_ridge <- glmnet (x_train, y_train, alpha = 0, 
                       standardize = TRUE, lambda = bestlam)
model_ridge$beta
model_ridge$a0 #8.613026

model_ridge_train_predict <- predict(model_ridge, newx = x_train)
MSE_Train_Ridge <- mean((10^y_train - 10^model_ridge_train_predict)^2)
MSE_Train_Ridge
model_ridge_predict <- predict(model_ridge, newx = x_test)

mean(10^model_ridge_predict)
test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<- 10^model_ridge_predict
model_ridge_predict<-10^model_ridge_predict

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_ridge2.xlsx")
#0.12799 for ridge regression

##################################################### Lasso ####################################################################
set.seed(1)
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1, 
                          type.measure = "mse", nfolds = 10)
plot (cv_out_lasso)
bestlam_lasso <- cv_out_lasso$lambda.min
bestlam_lasso
model_lasso <- glmnet(x_train, y_train, alpha = 1, 
                      standardize = TRUE, lambda = bestlam_lasso)
beta_matrix <- as.matrix(model_lasso$beta)
formatted_beta <- format(beta_matrix, scientific = FALSE)
print(formatted_beta)
model_lasso$a0 #7.051207

model_lasso_train_predict <- predict(model_lasso, newx = x_train)
model_lasso_predict <- predict(model_lasso, newx = x_test)
test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
mean(10^model_lasso_predict)
test_df$SalePrice<- 10^model_lasso_predict
model_lasso_predict<-10^model_lasso_predict
# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_lasso2.xlsx")
#0.12193



##################################################### Decision tree ####################################################################
Simple_tree <- tree(formula = log10(SalePrice) ~., data = drop_train)
summary(Simple_tree)
plot(Simple_tree)
text(Simple_tree)
test_pred_simple <- 10^predict(Simple_tree, drop_test)
mean(test_pred_simple)

test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<- test_pred_simple

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_decision_tree2.xlsx")
#0.21732 decision tree is not very good.



library(randomForest)
set.seed(123)
Tree_Bagging <- randomForest(log10(SalePrice) ~ ., data = drop_train,
                             ntrees = 500, mtry = 12, replace = TRUE,
                             importance = TRUE)
importance(Tree_Bagging) #HomeTotalSF is the most important which was to be expected
Test_pred_bag <- 10^predict(Tree_Bagging, drop_test)
mean(Test_pred_bag)

test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<- Test_pred_bag 

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_bagging2.xlsx")
#0.14335 - Kaggle scoring
##################################################### Random Forest ####################################################################
set.seed(1)
Tree_RF <- randomForest(log10(SalePrice) ~ ., data = drop_train,
                        ntrees = 500, mtry = 4, replace = TRUE,
                        importance = TRUE)

Random_forrest_Test_pred_rf <- predict(Tree_RF, drop_test)
test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<- 10^Random_forrest_Test_pred_rf 
Random_forrest_Test_pred_rf <- 10^Random_forrest_Test_pred_rf 

mean(test_df$SalePrice)

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_RF22.xlsx")
#Kaggle score: 0.15281

##################################################### Boosting ####################################################################
Num = 1000000
D = 0
library(gbm)
for(i in 1:6){
set.seed(1) #for k-fold cross validation
Tree_Boosting <- gbm(log10(SalePrice) ~., data = drop_train, distribution = "gaussian",
                       n.trees = 10000, interaction.depth = i, cv.folds = 10,
                       shrinkage = 0.01)
    Best = which.min(Tree_Boosting$cv.error)
    NumNew = Tree_Boosting$cv.error[Best]
  if (Num > NumNew){
    D = i
    i = i + 1
    Num = NumNew
    print(Num)
    print(D)
  }
  else {
    D=D
    i = i+1
    print(Num)
    print(D)
  }
  
}

set.seed(1) #for k-fold cross validation
Tree_Boosting <- gbm(log10(SalePrice) ~., data = drop_train, distribution = "gaussian",
                     n.trees = 10000, interaction.depth = 5, cv.folds = 10,
                     shrinkage = 0.01)

Sale_Price_Test_pred_boost <- 10^predict(Tree_Boosting, drop_test)
test_df <- data.frame(Id = raw_test_data$Id, SalePrice = rep(0, length(raw_test_data$Id)))
test_df$SalePrice<- Sale_Price_Test_pred_boost
mean(test_df$SalePrice)

# save as an excel document
library(writexl)
write_xlsx(test_df, "test_predictions_boosting2.xlsx")




