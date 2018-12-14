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
TrainData <- read.csv("train.csv")
TestData <- read.csv("cleanedTest.csv")
CleanTrainData<-as.data.frame(TrainData)


CleanTrainData$Id<-NULL
CleanTrainData$LotFrontage<-NULL
CleanTrainData$LotArea<-NULL
CleanTrainData$Street<-NULL
CleanTrainData$LotShape<-NULL
CleanTrainData$LandContour<-NULL
CleanTrainData$Utilities<-NULL
CleanTrainData$LotConfig<-NULL
CleanTrainData$LandSlope<-NULL
CleanTrainData$Condition1<-NULL
CleanTrainData$Condition2<-NULL
CleanTrainData$HouseStyle<-NULL
CleanTrainData$OverallCond<-NULL
CleanTrainData$YearBuilt<-NULL
CleanTrainData$YearRemodAdd<-NULL
CleanTrainData$RoofStyle<-NULL
CleanTrainData$RoofMatl<-NULL
CleanTrainData$Exterior1st<-NULL
CleanTrainData$Exterior2nd<-NULL
CleanTrainData$MasVnrType<-NULL
CleanTrainData$MasVnrArea<-NULL
CleanTrainData$ExterQual<-NULL
CleanTrainData$ExterCond<-NULL
CleanTrainData$RoofMatl<-NULL
CleanTrainData$BsmtCond<-NULL
CleanTrainData$BsmtExposure<-NULL
CleanTrainData$RoofMatl<-NULL
CleanTrainData$BsmtFinSF1<-NULL
CleanTrainData$BsmtFinSF2<-NULL
CleanTrainData$BsmtFinType2<-NULL
CleanTrainData$BsmtUnfSF<-NULL
CleanTrainData$TotalBsmtSF<-NULL
CleanTrainData$LowQualFinSF<-NULL
CleanTrainData$RoofMatl<-NULL
CleanTrainData$BsmtFullBath<-NULL
CleanTrainData$BsmtHalfBath<-NULL
CleanTrainData$HalfBath<-NULL
CleanTrainData$BedroomAbvGr<-NULL
CleanTrainData$KitchenAbvGr<-NULL
CleanTrainData$Functional<-NULL
CleanTrainData$FireplaceQu<-NULL
CleanTrainData$GarageType<-NULL
CleanTrainData$GarageArea<-NULL
CleanTrainData$GarageQual<-NULL
CleanTrainData$GarageCond<-NULL
CleanTrainData$GarageYrBlt<-NULL
CleanTrainData$WoodDeckSF<-NULL
CleanTrainData$OpenPorchSF<-NULL
CleanTrainData$EnclosedPorch<-NULL
CleanTrainData$X3SsnPorch<-NULL
CleanTrainData$ScreenPorch<-NULL
CleanTrainData$PoolArea<-NULL
CleanTrainData$PoolQC<-NULL
CleanTrainData$Fence<-NULL
CleanTrainData$MiscFeature<-NULL
CleanTrainData$MiscVal<-NULL
CleanTrainData$MoSold<-NULL
CleanTrainData$YrSold<-NULL
CleanTrainData$SaleType<-NULL

#cleaning ally
CleanTrainData$Alley <- sub("Grvl",1,CleanTrainData$Alley)
CleanTrainData$Alley <- sub("Pave",2,CleanTrainData$Alley)
CleanTrainData$Alley <-as.integer(CleanTrainData$Alley)
CleanTrainData$Alley[is.na(CleanTrainData$Alley)] <- 0
CleanTrainData$Alley <-as.factor(CleanTrainData$Alley)

#cleaning electrical

CleanTrainData$Electrical<-sub("1",1,CleanTrainData$Electrical)
CleanTrainData$Electrical<-sub("FuseA",0,CleanTrainData$Electrical)
CleanTrainData$Electrical<-sub("FuseF",0,CleanTrainData$Electrical)
CleanTrainData$Electrical<-sub("FuseP",0,CleanTrainData$Electrical)
CleanTrainData$Electrical<-sub("Mix",0,CleanTrainData$Electrical)
CleanTrainData$Electrical<-as.factor(CleanTrainData$Electrical)

#replace na values
CleanTrainData$Electrical[is.na(CleanTrainData$Electrical)]<-0
#send back to factor
CleanTrainData$Electrical<-as.factor(CleanTrainData$Electrical)

#replace other na values by making the variable an integer, sending NA values to 0, then sending variable back to a factor
CleanTrainData$BsmtQual<-as.integer(CleanTrainData$BsmtQual)
CleanTrainData$BsmtQual[is.na(CleanTrainData$BsmtQual)]<-0
CleanTrainData$BsmtQual<-as.factor(CleanTrainData$BsmtQual)

CleanTrainData$BsmtFinType1<-as.integer(CleanTrainData$BsmtFinType1)
CleanTrainData$BsmtFinType1[is.na(CleanTrainData$BsmtFinType1)]<-0 
CleanTrainData$BsmtFinType1<-as.factor(CleanTrainData$BsmtFinType1)

CleanTrainData$GarageFinish<-as.integer(CleanTrainData$GarageFinish)
CleanTrainData$GarageFinish[is.na(CleanTrainData$GarageFinish)]<-0 
CleanTrainData$GarageFinish<-as.factor(CleanTrainData$GarageFinish)

#change fireplaces to either do or dont have one

CleanTrainData$Fireplaces<-sub(2,1,CleanTrainData$Fireplaces)
CleanTrainData$Fireplaces<-sub(3,1,CleanTrainData$Fireplaces)
CleanTrainData$Fireplaces<-sub(4,1,CleanTrainData$Fireplaces)
CleanTrainData$Fireplaces<-as.factor(CleanTrainData$Fireplaces)
#change heating to gas or non gas

CleanTrainData$Heating<-sub("GasA","Gas", CleanTrainData$Heating)
CleanTrainData$Heating<-sub("GasW","Gas", CleanTrainData$Heating)
CleanTrainData$Heating<-sub("Wall","NonGas", CleanTrainData$Heating)
CleanTrainData$Heating<-sub("Floor","NonGas", CleanTrainData$Heating)
CleanTrainData$Heating<-sub("OthW","NonGas", CleanTrainData$Heating)
CleanTrainData$Heating<-sub("Grav","NonGas", CleanTrainData$Heating)
CleanTrainData$Heating<-as.factor(CleanTrainData$Heating)
#change paveddrive to paved and not paved

CleanTrainData$PavedDrive<-sub("P","N", CleanTrainData$PavedDrive)
CleanTrainData$PavedDrive<-as.factor(CleanTrainData$PavedDrive)

tree <-rpart(SalePrice~ ., data= CleanTrainData)
forest <-randomForest(SalePrice~. , data = CleanTrainData)
pridicted <- predict(forest,cleanTestData,type = "class")

write.csv(pridicted,"testOne.csv")

#print(tree)
str(CleanTrainData)
write.csv(CleanTrainData,"CleanedTest.csv")
str(cleanTestData)

## attemp to fix errors#####################################################
####################################################################
######################################################################
#########################################################################


cleanTestData <- read.csv("test.csv")


cleanTestData$Id<-NULL
cleanTestData$LotFrontage<-NULL
cleanTestData$LotArea<-NULL
cleanTestData$Street<-NULL
cleanTestData$LotShape<-NULL
cleanTestData$LandContour<-NULL
cleanTestData$Utilities<-NULL
cleanTestData$LotConfig<-NULL
cleanTestData$LandSlope<-NULL
cleanTestData$Condition1<-NULL
cleanTestData$Condition2<-NULL
cleanTestData$HouseStyle<-NULL
cleanTestData$OverallCond<-NULL
cleanTestData$YearBuilt<-NULL
cleanTestData$YearRemodAdd<-NULL
cleanTestData$RoofStyle<-NULL
cleanTestData$RoofMatl<-NULL
cleanTestData$Exterior1st<-NULL
cleanTestData$Exterior2nd<-NULL
cleanTestData$MasVnrType<-NULL
cleanTestData$MasVnrArea<-NULL
cleanTestData$ExterQual<-NULL
cleanTestData$ExterCond<-NULL
cleanTestData$RoofMatl<-NULL
cleanTestData$BsmtCond<-NULL
cleanTestData$BsmtExposure<-NULL
cleanTestData$RoofMatl<-NULL
cleanTestData$BsmtFinSF1<-NULL
cleanTestData$BsmtFinSF2<-NULL
cleanTestData$BsmtFinType2<-NULL
cleanTestData$BsmtUnfSF<-NULL
cleanTestData$TotalBsmtSF<-NULL
cleanTestData$LowQualFinSF<-NULL
cleanTestData$RoofMatl<-NULL
cleanTestData$BsmtFullBath<-NULL
cleanTestData$BsmtHalfBath<-NULL
cleanTestData$HalfBath<-NULL
cleanTestData$BedroomAbvGr<-NULL
cleanTestData$KitchenAbvGr<-NULL
cleanTestData$Functional<-NULL
cleanTestData$FireplaceQu<-NULL
cleanTestData$GarageType<-NULL
cleanTestData$GarageArea<-NULL
cleanTestData$GarageQual<-NULL
cleanTestData$GarageCond<-NULL
cleanTestData$GarageYrBlt<-NULL
cleanTestData$WoodDeckSF<-NULL
cleanTestData$OpenPorchSF<-NULL
cleanTestData$EnclosedPorch<-NULL
cleanTestData$X3SsnPorch<-NULL
cleanTestData$ScreenPorch<-NULL
cleanTestData$PoolArea<-NULL
cleanTestData$PoolQC<-NULL
cleanTestData$Fence<-NULL
cleanTestData$MiscFeature<-NULL
cleanTestData$MiscVal<-NULL
cleanTestData$MoSold<-NULL
cleanTestData$YrSold<-NULL
cleanTestData$SaleType<-NULL

#cleaning ally
cleanTestData$Alley <- sub("Grvl",1,cleanTestData$Alley)
cleanTestData$Alley <- sub("Pave",2,cleanTestData$Alley)
cleanTestData$Alley <-as.integer(cleanTestData$Alley)
cleanTestData$Alley[is.na(cleanTestData$Alley)] <- 0
cleanTestData$Alley <-as.factor(cleanTestData$Alley)

#cleaning electrical

cleanTestData$Electrical<-sub("1",1,cleanTestData$Electrical)
cleanTestData$Electrical<-sub("FuseA",0,cleanTestData$Electrical)
cleanTestData$Electrical<-sub("FuseF",0,cleanTestData$Electrical)
cleanTestData$Electrical<-sub("FuseP",0,cleanTestData$Electrical)
cleanTestData$Electrical<-sub("Mix",0,cleanTestData$Electrical)
cleanTestData$Electrical<-as.factor(cleanTestData$Electrical)

#replace na values
cleanTestData$Electrical[is.na(cleanTestData$Electrical)]<-0
#send back to factor
cleanTestData$Electrical<-as.factor(cleanTestData$Electrical)

#replace other na values by making the variable an integer, sending NA values to 0, then sending variable back to a factor
cleanTestData$BsmtQual<-as.integer(cleanTestData$BsmtQual)
cleanTestData$BsmtQual[is.na(cleanTestData$BsmtQual)]<-0
cleanTestData$BsmtQual<-as.factor(cleanTestData$BsmtQual)

cleanTestData$BsmtFinType1<-as.integer(cleanTestData$BsmtFinType1)
cleanTestData$BsmtFinType1[is.na(cleanTestData$BsmtFinType1)]<-0 
cleanTestData$BsmtFinType1<-as.factor(cleanTestData$BsmtFinType1)

cleanTestData$GarageFinish<-as.integer(cleanTestData$GarageFinish)
cleanTestData$GarageFinish[is.na(cleanTestData$GarageFinish)]<-0 
cleanTestData$GarageFinish<-as.factor(cleanTestData$GarageFinish)

#change fireplaces to either do or dont have one

cleanTestData$Fireplaces<-sub(2,1,cleanTestData$Fireplaces)
cleanTestData$Fireplaces<-sub(3,1,cleanTestData$Fireplaces)
cleanTestData$Fireplaces<-sub(4,1,cleanTestData$Fireplaces)
cleanTestData$Fireplaces<-as.factor(cleanTestData$Fireplaces)
#change heating to gas or non gas

cleanTestData$Heating<-sub("GasA","Gas", cleanTestData$Heating)
cleanTestData$Heating<-sub("GasW","Gas", cleanTestData$Heating)
cleanTestData$Heating<-sub("Wall","NonGas", cleanTestData$Heating)
cleanTestData$Heating<-sub("Floor","NonGas", cleanTestData$Heating)
cleanTestData$Heating<-sub("OthW","NonGas", cleanTestData$Heating)
cleanTestData$Heating<-sub("Grav","NonGas", cleanTestData$Heating)
cleanTestData$Heating<-as.factor(cleanTestData$Heating)
#change paveddrive to paved and not paved

cleanTestData$PavedDrive<-sub("P","N", cleanTestData$PavedDrive)
cleanTestData$PavedDrive<-as.factor(cleanTestData$PavedDrive)