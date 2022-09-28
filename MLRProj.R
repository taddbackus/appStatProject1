library(ggplot2)
library(GGally)
library(tidyverse)
library(lindia)
library(olsrr)
library(caret)
library(ISLR)
theme_set(theme_minimal())

################################################################################
# Reading data
################################################################################
carDataRaw = read.csv('/Users/taddbackus/School/fall22/appliedStat/Project1/data1.csv',
                      header=TRUE)
carData = carDataRaw

################################################################################
# Renaming variables
################################################################################
carData = carData %>%
  rename(
    fuelType = Engine.Fuel.Type,
    horsepower = Engine.HP,
    cylinders = Engine.Cylinders,
    transmission = Transmission.Type,
    drive = Driven_Wheels,
    doors = Number.of.Doors,
    category = Market.Category,
    size = Vehicle.Size,
    style = Vehicle.Style,
    highwayMPG = highway.MPG,
    cityMPG = city.mpg
  )

################################################################################
# Making categorical variables factors
################################################################################
carData$Make = as.factor(carData$Make)
carData$Model = as.factor(carData$Model)
carData$fuelType = as.factor(carData$fuelType)
carData$transmission = as.factor(carData$transmission)
carData$drive = as.factor(carData$drive)
carData$category = as.factor(carData$category)
carData$size = as.factor(carData$size)
carData$style = as.factor(carData$style)

################################################################################
# Missing variables
################################################################################
summary(carData)
sapply(carData, function(x) sum(is.na(x)))
# Cylinders
# All missing values were electric or rotary
carData$cylinders[is.na(carData$cylinders)] = 0

# Doors
# Used similar vehicles
carData$doors[is.na(carData$doors) & carData$Make == 'Tesla'] = 4
carData$doors[is.na(carData$doors) & carData$Make == 'Ferrari'] = 2

# Horsepower
# Used mean of same model if there was data otherwise dropped
carData$horsepower[is.na(carData$horsepower) & 
                    carData$Make == 'Lincoln' &
                    carData$Model == 'Continental'] = mean(
                      carData$horsepower[!is.na(carData$horsepower) &
                                           carData$Make == 'Lincoln' &
                                           carData$Model == 'Continental']
                    )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Ford' &
                     carData$Model == 'Escape'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Ford' &
                                            carData$Model == 'Escape']
                     )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Ford' &
                     carData$Model == 'Freestar'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Ford' &
                                            carData$Model == 'Freestar']
                     )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Mitsubishi' &
                     carData$Model == 'i-MiEV'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Mitsubishi' &
                                            carData$Model == 'i-MiEV']
                     )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Chevrolet' &
                     carData$Model == 'Impala'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Chevrolet' &
                                            carData$Model == 'Impala']
                     )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Mercedes-Benz' &
                     carData$Model == 'M-Class' &
                     carData$fuelType == 'diesel'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Mercedes-Benz' &
                                            carData$Model == 'M-Class' &
                                            carData$fuelType == 'diesel']
                     )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Lincoln' &
                     carData$Model == 'MKZ'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Lincoln' &
                                            carData$Model == 'MKZ']
                     )
carData$horsepower[is.na(carData$horsepower) &
                     carData$Make == 'Toyota' &
                     carData$Model == 'RAV4 EV'] = mean(
                       carData$horsepower[!is.na(carData$horsepower) &
                                            carData$Make == 'Toyota' &
                                            carData$Model == 'RAV4 EV']
                     )

# Fuel Type
# Fixing Suzuki Verona
carData$fuelType[carData$Make == 'Suzuki' &
                   carData$fuelType == ''] = 'regular unleaded'

# Category
# Using only the first value in the string for simplicity
# Replacing the N/A factor with Other because it seems important
carData$category = sub(',.*','',carData$category)
carData$category[carData$category == 'N/A'] = 'Other'
carData$category = as.factor(carData$category)


carDataClean = na.omit(carData)
sapply(carDataClean, function(x) sum(is.na(x)))

################################################################################
# Creating variables
################################################################################
# Combined MPG
# For gasoline vehicles, the label shows City, Highway, and Combined MPG (miles per gallon) values. 
#The Combined MPG value is the most prominent for the purpose of quick and easy comparison across vehicles. 
#Combined fuel economy is a weighted average of City and Highway MPG values that is calculated by weighting the City value by 55% and the Highway value by 45%.
# -- epa.gov
cor(carDataClean$highwayMPG,carDataClean$cityMPG)
ggplot(carData,aes(x=highwayMPG,y=cityMPG))+
  geom_point()
carDataClean$combMPG = carDataClean$highwayMPG*.45 + carDataClean$cityMPG*.55
carDataClean = subset(carDataClean, select=-c(highwayMPG,cityMPG))

# Before and After 2001
carDataClean$yearCat = ifelse(carDataClean$Year<2001,'Before','After')

################################################################################
# Initial EDA and transforming data
################################################################################
# Un-transformed scatter matrix
num_cols = unlist(lapply(carDataClean,is.numeric))
num_cols
carNum = carDataClean[,num_cols]
ggpairs(carNum)

# Transformed scatter matrix
carDataTrans = carDataClean
carDataTrans$MSRP = log(carDataTrans$MSRP)
carDataTrans$combMPG = log(carDataTrans$combMPG)
carNumTrans = carDataTrans[,num_cols]
ggpairs(carNumTrans)

ggplot(carDataTrans, aes(y=category))+
  geom_bar()
################################################################################
# Splitting data
################################################################################
ss = sample(1:3, size=nrow(carDataTrans),replace=TRUE,prob=c(0.8,0.1,0.1))
train = carDataTrans[ss==1,]
test = carDataTrans[ss==2,]
val = carDataTrans[ss==3,]

################################################################################
# Initial EDA and transforming data
################################################################################
# Popularity
cor(train$MSRP,train$Popularity)
ggplot(train, aes(x=Popularity,y=Make))+
  geom_boxplot()

ggplot(train, aes(x=MSRP,y=Make,fill=yearCat))+
  geom_boxplot(show.legend=TRUE)

ggplot(train, aes(x=MSRP,y=fuelType,fill=fuelType))+
  geom_boxplot(show.legend=FALSE)

ggplot(train, aes(x=MSRP,y=transmission,fill=transmission))+
  geom_boxplot(show.legend=FALSE)

ggplot(train, aes(x=MSRP,y=drive,fill=drive))+
  geom_boxplot(show.legend=FALSE)

ggplot(train, aes(x=MSRP,y=category,fill=category))+
  geom_boxplot(show.legend=FALSE)

ggplot(train, aes(x=MSRP,y=size,fill=yearCat))+
  geom_boxplot()

ggplot(train, aes(x=MSRP,y=style,fill=yearCat))+
  geom_boxplot()

################################################################################
# Objective 1
################################################################################
# Playing with LASSO
fitControl<-trainControl(method="repeatedcv",number=5,repeats=1) 


#Fitting glmnet
set.seed(1234)
glmnet.fit<-train(MSRP~combMPG+cylinders+horsepower+Year+doors+Make+fuelType+transmission+drive+Year:yearCat,
                  data=train,
                  method="glmnet",
                  trControl=fitControl,
                  tuneGrid=expand.grid(data.frame(alpha=1,lambda=seq(0,.05,.001)))
)

glmnet.fit
plot(glmnet.fit)


#Investigating coefficients
opt.pen<-glmnet.fit$finalModel$lambdaOpt #penalty term
coef(glmnet.fit$finalModel,opt.pen)
AIC(glmnet.fit)

################################################################################
# Objective 2
# Complex MLR
################################################################################
complexModel1 = lm(MSRP~combMPG+cylinders+horsepower+Year+doors+Make+fuelType+transmission+
                     drive+category+horsepower:category+Year:yearCat+yearCat+combMPG:fuelType,
                     data=train)
summary(complexModel1)
AIC(complexModel1)

glmnet.fit2 = train(MSRP~combMPG+cylinders+horsepower+Year+doors+Make+fuelType+transmission+
                      drive+category+yearCat+horsepower:category+Year:yearCat+combMPG:fuelType,
                    data=train,
                    method='glmnet',
                    trControl=fitControl,
                    tuneGrid=expand.grid(data.frame(alpha=1,lambda=seq(0,.05,.0001)))
                    )
glmnet.fit2
plot(glmnet.fit2)
opt.pen<-glmnet.fit2$finalModel$lambdaOpt #penalty term
coef(glmnet.fit2$finalModel,opt.pen)

################################################################################
# Objective 2
# kNN Regression
################################################################################
testModel = train(MSRP~.,data=train,method='knn')
testModel

plot(testModel)

testModel = train(MSRP~combMPG+cylinders+horsepower+yearCat+doors,
                  data=train,
                  method='knn')
testModel
plot(testModel)



























