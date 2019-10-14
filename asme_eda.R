#### HOUSE PRICES ADVANCED REGRESSION TECHNIQIES ####
#### AMME Data set ####
#### https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data ####
library(knitr)


library(DataExplorer) #For data exploration 
library(psych)
library(corrplot) # For graphical correlations
#### Set Path ####
getwd()
setwd("/Users/chetanak/Box Sync/Projects/DataScience/R/house-prices-advanced-regression-techniques/")
getwd()
########

#List the files and read the data files
#print(list.files("../input"))
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
head(train)
head(test)
ncol(train)
nrow(train)
ncol(test)
nrow(test)

setdiff(names(train), names(test))

test_ids <- test$Id
train$Id <- NULL
test$Id <- NULL
test$SalePrice <- NA

df <- rbind(train, test)
df_NA <- df[rowSums(is.na(df)) > 0,]

##Based on the Description of the data set, change the 'NA' to 'None' where applicable
df$Alley[is.na(df$Alley)] <- 'None'
df$BsmtQual[is.na(df$BsmtQual)] <- 'None'
df$BsmtCond[is.na(df$BsmtCond)] <- 'None'
df$BsmtExposure[is.na(df$BsmtExposure)] <- 'None'
df$BsmtFinType1[is.na(df$BsmtFinType1)] <- 'None'
df$BsmtFinSF1[is.na(df$BsmtFinSF1)] <- 0
df$BsmtFinType2[is.na(df$BsmtFinType2)] <- 'None'
df$BsmtFinSF2[is.na(df$BsmtFinSF2)] <- 0
df$FireplaceQu[is.na(df$FireplaceQu)] <- 'None'
df$GarageType[is.na(df$GarageType)] <- 'None'
df$GarageFinish[is.na(df$GarageFinish)] <- 'None'
df$GarageQual[is.na(df$GarageQual)] <- 'None'
df$GarageCond[is.na(df$GarageCond)] <- 'None'
df$GarageCars[is.na(df$GarageCars)] <- 0
df$GarageArea[is.na(df$GarageArea)] <- 0
df$GarageYrBlt[is.na(df$GarageYrBlt)] <- 'None'
df$PoolQC[is.na(df$PoolQC)] <- 'None'
df$Fence[is.na(df$Fence)] <- 'None'
df$MiscFeature[is.na(df$MiscFeature)] <- 'None'
df$BsmtUnfSF[is.na(df$BmstUnfSF)] <- 0
df$TotalBsmtSF[is.na(df$TotalBsmtSF)] <- 0
df$BsmtFullBath[is.na(df$BsmtFullBath)] <- 0
df$BsmtHalfBath[is.na(df$BsmtHalfBath)] <- 0
df$BsmtUnfSF[is.na(df$BsmtUnfSF)] <- 0

df_NA <- df[rowSums(is.na(df)) > 0,]

dim(df)
#structure of train data set
as.data.frame(cbind(lapply(df, class)))
str(df)
describe(df)


#Show the  missing value counts with their column names
NAcol <- which(colSums(is.na(df)) > 0)
df_NA <- sort(colSums(sapply(df[NAcol], is.na)), decreasing = TRUE)

numeric_cols <- unlist(sapply(df, is.numeric))

#subset by numeric and charater type and get a count of missing values for each variable
describe(dfNum)
dfNum <- df[, numeric_cols]
dfNum_NA <- sort(sapply(dfNum, function(x) sum(is.na(x))), decreasing=TRUE)
dfNum_NA <- dfNum_NA[dfNum_NA>0]
dfNum_NA

str(dfChar)
dfChar <- df[,!numeric_cols]
dfChar_NA <- sort(sapply(dfChar, function(x) sum(is.na(x))), decreasing=TRUE)
dfChar_NA <- dfChar_NA[dfChar_NA>0]
dfChar_NA

###Plot the missing values #####

DataExplorer::plot_missing(dfNum)
#DataExplorer::plot_boxplot(dfNum)
DataExplorer::plot_histogram(dfNum)
DataExplorer::create_report(dfNum)

DataExplorer::plot_missing(dfChar)
#DataExplorer::plot_boxplot(dfChar)
DataExplorer::plot_histogram(dfChar)
DataExplorer::create_report(dfChar)

#### Plot the correlations of all numeric variables ####
cor_numVar <- cor(dfNum, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations with Sales Price
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", upper="circle", lower="number")

##### Basement variables names ######
##
### BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF,
### TotalBsmtSF, BsmtFullBath, BsmtHalfBath
bsmtVarNames <- c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "BsmtFinSF2", "BsmtUnfSF",
 "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath")
bsmtVarDF <- df[bsmtVarNames]
bsmtVarDF_NA <- bsmtVarDF[rowSums(is.na(bsmtVarDF)) > 0,]

DataExplorer::plot_histogram(bsmtVarDF)


table(df$BsmtQual)
table(df$BsmtCond)
table(df$BsmtExposure)
table(df$BsmtFinType1)
table(df$BsmtFinSF1)
table(df$BsmtFinType2)
table(df$BsmtFinSF2)
table(df$BsmtUnfSF)
table(df$TotalBsmtSF)
table(df$BsmtFullBath)
table(df$BsmtHalfBath)

df$GarageType[is.na(df$GarageType)] <- 'None'
df$GarageFinish[is.na(df$GarageFinish)] <- 'None'
df$GarageQual[is.na(df$GarageQual)] <- 'None'
df$GarageCond[is.na(df$GarageCond)] <- 'None'
df$GarageCars[is.na(df$GarageCars)] <- 0
df$GarageArea[is.na(df$GarageArea)] <- 0

length(which(is.na(df$GarageFinish) & is.na(df$GarageType) & is.na(df$GarageQual) & is.na(df$GarageCond) ))

df$GarageFinish[is.na(df$GarageType)] <- 'None'
df$GarageQual[is.na(df$GarageType)] <- 'None'
df$GarageCond[is.na(df$GarageType)] <- 'None'
df$GarageYrBlt[is.na(df$GarageType)] <- 0
df$GarageType <- 'None'

