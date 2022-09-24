library(ggplot2)
library(GGally)
library(tidyverse)



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
sapply(carDataClean, function(x) sum(is.na(x)))


# Plotting scatter matrix
num_cols = unlist(lapply(carDataClean,is.numeric))
num_cols
carNum = carDataClean[,num_cols]
carNum$MSRP = log(carNum$MSRP)
ggpairs(carNum)

# Transforming MSRP and MPG
carDataTrans = carDataClean
carDataTrans$highwayMPG = log(carDataTrans$highwayMPG)
carDataTrans$cityMPG = log(carDataTrans$cityMPG)
carDataTrans$MSRP = log(carDataTrans$MSRP)
carNumTrans = carDataTrans[,num_cols]
ggpairs(carNumTrans)

# Splitting into 80 10 10
ss = sample(1:3, size=nrow(carDataRaw),replace=TRUE,prob=c(0.8,0.1,0.1))
train = carDataRaw[ss==1,]
test = carDataRaw[ss==2,]
val = carDataRaw[ss==3,]



