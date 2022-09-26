library(ggplot2)
library(GGally)
library(tidyverse)
library(lindia)
library(olsrr)
theme_set(theme_minimal())

carDataRaw = read.csv('/Users/taddbackus/School/fall22/appliedStat/Project1/data1.csv',header=TRUE)
carData = carDataRaw
# Making better variable names because im picky
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
    cityMPG = city.mpg,
  )

# Dealing with correlation between MPGS
# For gasoline vehicles, the label shows City, Highway, and Combined MPG (miles per gallon) values. 
#The Combined MPG value is the most prominent for the purpose of quick and easy comparison across vehicles. 
#Combined fuel economy is a weighted average of City and Highway MPG values that is calculated by weighting the City value by 55% and the Highway value by 45%.
# -- epa.gov
cor(carData$highwayMPG,carData$cityMPG)
carData$combMPG = carData$highwayMPG*.45 + carData$cityMPG*.55

# Chaning characters to factors
carData$Make = as.factor(carData$Make)
carData$Model = as.factor(carData$Model)
carData$fuelType = as.factor(carData$fuelType)
carData$transmission = as.factor(carData$transmission)
carData$drive = as.factor(carData$drive)
carData$category = as.factor(carData$category)
carData$size = as.factor(carData$size)
carData$style = as.factor(carData$style)

# Summary of data and count of missing variables
summary(carData)
sapply(carData, function(x) sum(is.na(x)))

# Missing values in cylinders column
# All the missing values were electric or rotary engines
carData$cylinders[is.na(carData$cylinders)] = 0

# Missing values in doors
carData$doors[is.na(carData$doors) & carData$Make == 'Tesla'] = 4
carData$doors[is.na(carData$doors) & carData$Make == 'Ferrari'] = 2

# Missing values in horsepower
sapply(carData, function(x) sum(is.na(x)))


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
carDataNA = carData[is.na(carData$horsepower),]
carDataClean = na.omit(carData)
carDataClean = subset(carDataClean,select=-c(highwayMPG,cityMPG))
sapply(carDataClean, function(x) sum(is.na(x)))


# Plotting scatter matrix
num_cols = unlist(lapply(carDataClean,is.numeric))
num_cols
carNum = carDataClean[,num_cols]
carNum$MSRP = log(carNum$MSRP)
ggpairs(carNum)

# Transforming MSRP and MPG
carDataTrans = carDataClean
carDataTrans$MSRP = log(carDataTrans$MSRP)
carDataTrans$combMPG = log(carDataTrans$combMPG)
carNumTrans = carDataTrans[,num_cols]
ggpairs(carNumTrans)

# Splitting into 80 10 10
ss = sample(1:3, size=nrow(carDataClean),replace=TRUE,prob=c(0.8,0.1,0.1))
trainC = carDataClean[ss==1,]
trainT = carDataTrans[ss==1,]
testT = carDataTrans[ss==2,]
valT = carDataTrans[ss==3,]

# Objective 1 model
# Variables i think are important/simple
partialData = subset(trainT,
                     select=c(
                       combMPG,
                       cylinders,
                       horsepower,
                       Year,
                       doors,
                       Make,
                       fuelType,
                       transmission,
                       drive,
                       MSRP
                     ))
partFit1 = lm(MSRP~.,data=partialData)
summary(partFit1)
gg_diagnose(partFit1)

partFitSelect = ols_step_both_aic(partFit1,details=TRUE)

# All of the variables
fullFit1 = lm(MSRP~.,data=trainT)
summary(fullFit1)
fullFitSelect = ols_step_both_aic(fullFit1,details=TRUE)
summary(fullFitSelect)

# Final? objective 1 fit
obj1Fit = lm(MSRP~combMPG+cylinders+horsepower+Year+doors+Make+fuelType+transmission+drive+Popularity,data=trainT)
summary(obj1Fit)
plot(obj1Fit$residuals)

# Playing with iteraction terms
obj1Fit2 = lm(MSRP~combMPG+cylinders+horsepower+Year+doors+Make+fuelType+transmission+
                drive+Popularity+category+horsepower:category,data=trainT)
summary(obj1Fit2)
AIC(obj1Fit2)
summary(lm(MSRP~combMPG+fuelType+combMPG:fuelType,data=trainT))
AIC(lm(MSRP~horsepower+category+horsepower:category,data=trainT))
AIC(obj1Fit)


# EDA
ggplot(carNumTrans,aes(x=MSRP,y=as.factor(doors)))+
  geom_boxplot()

ggplot(carDataTrans,aes(x=MSRP,y=style))+
  geom_boxplot()

ggplot(carDataTrans,aes(x=Popularity))+
  geom_histogram()
ggplot(trainC, aes(x=MSRP))+
  geom_histogram(bins=100)+
  xlim(0,25000)
ggplot(trainT, aes(x=MSRP))+
  geom_histogram(bins=100)

ggplot(trainT, aes(x=Popularity,y=MSRP))+
  geom_point(position='jitter')+
  geom_smooth(method='lm')

popMake = trainT %>%
  group_by(Make) %>%
  summarize(avgCost = mean(MSRP))
popMake$avgPop = trainT %>%
  group_by(Make) %>%
  summarize(avgPop = mean(Popularity))
ggplot(popMake,aes(x=avgPop$avgPop,y=avgCost,color=Make))+
  geom_point()

ggplot(trainT,aes(x=horsepower,y=MSRP,color=drive))+
  geom_point()

ggplot(trainC,aes(x=MSRP,y=category))+
  geom_boxplot()
summary(trainC$category)

ggplot(trainT,aes(x=MSRP,y=style))+
  geom_boxplot()

ggplot(trainT,aes(x=horsepower,y=MSRP,color=category))+
  geom_smooth(method='lm',show.legend = FALSE)

ggplot(trainT,aes(x=combMPG,y=MSRP,color=category))+
  geom_smooth(method='lm',show.legend=FALSE)

ggplot(trainT,aes(x=combMPG,y=MSRP,color=fuelType))+
  geom_smooth(method='lm',show.legend=FALSE)
