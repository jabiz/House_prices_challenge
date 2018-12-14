install.packages("rpart")
install.packages("randomForest")
install.packages("rattle")
library(rpart)
library(randomForest)
library(rattle)
library(class)
library(ggplot2)
library(e1071)
#import data
testdata2<-read.csv("C:/Users/Admin/Documents/house_price/test.csv")
trainingdata2<-read.csv("C:/Users/Admin/Documents/house_price/train.csv")
testdata2$SalePrice<-0
#join tables to get rid of all NAs 
combineddata<-rbind(trainingdata2,testdata2)
#replace NA values with average values for each variable, or 0 in eg the case of not having a basement

combineddata$Exterior1st<-as.integer(combineddata$Exterior1st)
combineddata$Exterior1st[is.na(combineddata$Exterior1st)]<-13
combineddata$Exterior1st<-as.factor(combineddata$Exterior1st)

combineddata$Exterior2nd<-as.integer(combineddata$Exterior2nd)
combineddata$Exterior2nd[is.na(combineddata$Exterior2nd)]<-14
combineddata$Exterior2nd<-as.factor(combineddata$Exterior2nd)

combineddata$Functional<-as.integer(combineddata$Functional)
combineddata$Functional[is.na(combineddata$Functional)]<-2
combineddata$Functional<-as.factor(combineddata$Functional)

combineddata$BsmtQual<-as.integer(combineddata$BsmtQual)
combineddata$BsmtQual[is.na(combineddata$BsmtQual)]<-0
combineddata$BsmtQual<-as.factor(combineddata$BsmtQual)

combineddata$BsmtFinType1<-as.integer(combineddata$BsmtFinType1)
combineddata$BsmtFinType1[is.na(combineddata$BsmtFinType1)]<-0 
combineddata$BsmtFinType1<-as.factor(combineddata$BsmtFinType1)

combineddata$GarageFinish<-as.integer(combineddata$GarageFinish)
combineddata$GarageFinish[is.na(combineddata$GarageFinish)]<-0 
combineddata$GarageFinish<-as.factor(combineddata$GarageFinish)

combineddata$Electrical<-as.integer(combineddata$GarageFinish)
combineddata$Electrical[is.na(combineddata$Electrical)]<-0
combineddata$Electrical<-as.factor(combineddata$GarageFinish)

combineddata$GarageFinish<-as.integer(combineddata$GarageFinish)
combineddata$GarageFinish[is.na(combineddata$GarageFinish)]<-0
combineddata$GarageFinish<-as.factor(combineddata$GarageFinish)

combineddata$MSZoning<-as.integer(combineddata$MSZoning)
combineddata$MSZoning[is.na(combineddata$MSZoning)]<-4
combineddata$MSZoning<-as.factor(combineddata$MSZoning)

combineddata$KitchenQual<-as.integer(combineddata$KitchenQual)
combineddata$KitchenQual[is.na(combineddata$KitchenQual)]<-3
combineddata$KitchenQual<-as.factor(combineddata$KitchenQual)

combineddata$GarageCars<-as.integer(combineddata$GarageCars)
combineddata$GarageCars[is.na(combineddata$GarageCars)]<-0

combineddata$GarageArea<-as.integer(combineddata$GarageArea)
combineddata$GarageArea[is.na(combineddata$GarageArea)]<-500
combineddata$GarageArea<-as.integer(combineddata$GarageArea)

combineddata$Fence<-as.integer(combineddata$Fence)
combineddata$Fence[is.na(combineddata$Fence)]<-0
combineddata$Fence<-as.factor(combineddata$Fence)

combineddata$FireplaceQu<-as.integer(combineddata$FireplaceQu)
combineddata$FireplaceQu[is.na(combineddata$FireplaceQu)]<- 0
combineddata$FireplaceQu<-as.factor(combineddata$FireplaceQu)

combineddata$Alley<-as.integer(combineddata$Alley)
combineddata$Alley[is.na(combineddata$Alley)]<- 0
combineddata$Alley<-as.factor(combineddata$Alley)

#drop variables we deemed unsuitable
combineddata$Id<-NULL
combineddata$LotFrontage<-NULL
combineddata$Street<-NULL
combineddata$LotShape<-NULL
combineddata$Utilities<-NULL
combineddata$LotConfig<-NULL
combineddata$Condition1<-NULL
combineddata$Condition2<-NULL
combineddata$YearBuilt<-NULL
combineddata$YearRemodAdd<-NULL
combineddata$RoofMatl<-NULL
combineddata$MasVnrType<-NULL
combineddata$MasVnrArea<-NULL
combineddata$RoofMatl<-NULL
combineddata$BsmtCond<-NULL
combineddata$BsmtExposure<-NULL
combineddata$RoofMatl<-NULL
combineddata$BsmtFinSF1<-NULL
combineddata$BsmtFinSF2<-NULL
combineddata$BsmtFinType2<-NULL
combineddata$BsmtUnfSF<-NULL
combineddata$TotalBsmtSF<-NULL
combineddata$LowQualFinSF<-NULL
combineddata$RoofMatl<-NULL
combineddata$BsmtFullBath<-NULL
combineddata$BsmtHalfBath<-NULL
combineddata$BedroomAbvGr<-NULL
combineddata$KitchenAbvGr<-NULL
combineddata$GarageType<-NULL
combineddata$GarageQual<-NULL
combineddata$GarageCond<-NULL
combineddata$GarageYrBlt<-NULL
combineddata$WoodDeckSF<-NULL
combineddata$OpenPorchSF<-NULL
combineddata$EnclosedPorch<-NULL
combineddata$X3SsnPorch<-NULL
combineddata$ScreenPorch<-NULL
combineddata$PoolArea<-NULL
combineddata$PoolQC<-NULL
combineddata$MiscFeature<-NULL
combineddata$MiscVal<-NULL
combineddata$MoSold<-NULL
combineddata$YrSold<-NULL
combineddata$SaleType<-NULL
combineddata$LotArea<-NULL




trainingdata<-combineddata[1:1460,]
testdata<-combineddata[1461:2919,]
str(combineddata)

cleaneddata<-as.data.frame(trainingdata)
  

  
cleanedtestdata<-as.data.frame(testdata)
  
cleanedtestdata$SalePrice<-NULL
  
  
 
#make decision forest and write to csv file after applying it to the cleaned test data set

  str(cleaneddata)
  str(cleanedtestdata)
  
  cleaneddata$SalePrice <- as.numeric(cleaneddata$SalePrice)
  
  tree <- rpart(SalePrice ~ . , data=cleaneddata)
  
  print(tree$cptable)
  
  forest <- randomForest(SalePrice ~ . , data=cleaneddata)
  
  print(forest)
  
  predTest <- predict(forest, cleanedtestdata, type = "class")
  
  #some NA files, cant find a reason, replaced with avg price in csv file
  write.csv(predTest, "predictionNumericForest10.csv")


