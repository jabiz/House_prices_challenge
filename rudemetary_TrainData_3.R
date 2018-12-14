install.packages("rpart")
install.packages("randomForest")
install.packages("rattle")
library(rpart)
library(randomForest)
library(rattle)
library(class)
library(ggplot2)
library(e1071)

setwd("C:/Users/Admin/Documents/house_price")
TestData <- read.csv("train.csv")
cleanedtestdata<-as.data.frame(TestData)

cleanedtestdata$Id<-NULL
cleanedtestdata$LotFrontage<-NULL
cleanedtestdata$LotArea<-NULL
cleanedtestdata$Street<-NULL
cleanedtestdata$LotShape<-NULL
cleanedtestdata$LandContour<-NULL
cleanedtestdata$Utilities<-NULL
cleanedtestdata$LotConfig<-NULL
cleanedtestdata$LandSlope<-NULL
cleanedtestdata$Condition1<-NULL
cleanedtestdata$Condition2<-NULL
cleanedtestdata$HouseStyle<-NULL
cleanedtestdata$OverallCond<-NULL
cleanedtestdata$YearBuilt<-NULL
cleanedtestdata$YearRemodAdd<-NULL
cleanedtestdata$RoofStyle<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$Exterior1st<-NULL
cleanedtestdata$Exterior2nd<-NULL
cleanedtestdata$MasVnrType<-NULL
cleanedtestdata$MasVnrArea<-NULL
cleanedtestdata$ExterQual<-NULL
cleanedtestdata$ExterCond<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtCond<-NULL
cleanedtestdata$BsmtExposure<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtFinSF1<-NULL
cleanedtestdata$BsmtFinSF2<-NULL
cleanedtestdata$BsmtFinType2<-NULL
cleanedtestdata$BsmtUnfSF<-NULL
cleanedtestdata$TotalBsmtSF<-NULL
cleanedtestdata$LowQualFinSF<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtFullBath<-NULL
cleanedtestdata$BsmtHalfBath<-NULL
cleanedtestdata$HalfBath<-NULL
cleanedtestdata$BedroomAbvGr<-NULL
cleanedtestdata$KitchenAbvGr<-NULL
cleanedtestdata$Functional<-NULL
cleanedtestdata$FireplaceQu<-NULL
cleanedtestdata$GarageType<-NULL
cleanedtestdata$GarageArea<-NULL
cleanedtestdata$GarageQual<-NULL
cleanedtestdata$GarageCond<-NULL
cleanedtestdata$GarageYrBuilt<-NULL
cleanedtestdata$WoodDeckSF<-NULL
cleanedtestdata$OpenPorchSF<-NULL
cleanedtestdata$EnclosedPorch<-NULL
cleanedtestdata$X3SsnPorch<-NULL
cleanedtestdata$ScreenPorch<-NULL
cleanedtestdata$PoolArea<-NULL
cleanedtestdata$PoolQC<-NULL
cleanedtestdata$Fence<-NULL
cleanedtestdata$MiscFeature<-NULL
cleanedtestdata$MiscVal<-NULL
cleanedtestdata$MoSold<-NULL
cleanedtestdata$YrSold<-NULL
cleanedtestdata$SaleType<-NULL


#cleanedtestdata$MSZoning <- sub("A","1",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("C(all)","2",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("FV","3",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("I","4",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("RH","5",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("RL","6",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("RM","7",cleanedtestdata$MSZoning)

#cleaning ally
cleanedtestdata$Alley <- sub("Grvl",1,cleanedtestdata$Alley)
cleanedtestdata$Alley <- sub("Pave",2,cleanedtestdata$Alley)
cleanedtestdata$Alley <-as.integer(cleanedtestdata$Alley)
cleanedtestdata$Alley[is.na(cleanedtestdata$Alley)] <- 0
cleanedtestdata$Alley <-as.factor(cleanedtestdata$Alley)

#cleaning electrical

cleanedtestdata$Electrical<-sub("1",1,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseA",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseF",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseP",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("Mix",0,cleanedtestdata$Electrical)

#change fireplaces to either do or dont have one

cleaneddata$Fireplaces<-sub(2,1,cleaneddata$Fireplaces)
cleaneddata$Fireplaces<-sub(3,1,cleaneddata$Fireplaces)
#change heating to gas or non gas

cleaneddata$Heating<-sub("GasA","Gas", cleaneddata$Heating)
cleaneddata$Heating<-sub("GasW","Gas", cleaneddata$Heating)
cleaneddata$Heating<-sub("Wall","NonGas", cleaneddata$Heating)
cleaneddata$Heating<-sub("Floor","NonGas", cleaneddata$Heating)
cleaneddata$Heating<-sub("OthW","NonGas", cleaneddata$Heating)
cleaneddata$Heating<-sub("Grav","NonGas", cleaneddata$Heating)
#change paveddrive to paved and not paved

cleaneddata$PavedDrive<-sub("P","N", cleaneddata$PavedDrive)





#replace na values
cleanedtestdata$Electrical[is.na(cleanedtestdata$Electrical)]<-0
#send back to factor
cleanedtestdata$Electrical<-as.factor(cleanedtestdata$Electrical)

#replace other na values by making the variable an integer, sending NA values to 0, then sending variable back to a factor
cleanedtestdata$BsmtQual<-as.integer(cleanedtestdata$BsmtQual)
cleanedtestdata$BsmtQual[is.na(cleanedtestdata$BsmtQual)]<-0
cleanedtestdata$BsmtQual<-as.factor(cleanedtestdata$BsmtQual)

cleanedtestdata$BsmtFinType1<-as.integer(cleanedtestdata$BsmtFinType1)
cleanedtestdata$BsmtFinType1[is.na(cleanedtestdata$BsmtFinType1)]<-0 
cleanedtestdata$BsmtFinType1<-as.factor(cleanedtestdata$BsmtFinType1)

cleanedtestdata$GarageFinish<-as.integer(cleanedtestdata$GarageFinish)
cleanedtestdata$GarageFinish[is.na(cleanedtestdata$GarageFinish)]<-0 
cleanedtestdata$GarageFinish<-as.factor(cleanedtestdata$GarageFinish)

