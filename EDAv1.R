library(ggplot2)
library(GGally)


carDataRaw = read.csv('/Users/taddbackus/School/fall22/appliedStat/Project1/data1.csv',header=TRUE)
carData = carDataRaw
carData$Make = as.factor(carData$Make)
carData$Engine.Fuel.Type = as.factor(carData$Engine.Fuel.Type)
carData$Transmission.Type = as.factor(carData$Transmission.Type)
carData$Driven_Wheels = as.factor(carData$Driven_Wheels)
carData$Market.Category = as.factor(carData$Market.Category)
carData$Vehicle.Size = as.factor(carData$Vehicle.Size)
carData$Vehicle.Style = as.factor(carData$Vehicle.Style)
summary(carData)
sapply(carData, function(x) sum(is.na(x)))

hpNA = carData[is.na(carData$Engine.HP),]
summary(hpNA)
cylNA = carData[is.na(carData$Engine.Cylinders),]
cylNA$Engine.Cylinders[is.na(cylNA$Engine.Cylinders)] = 0
carData$Engine.Cylinders[is.na(cylNA$Engine.Cylinders)] = 0
doorNA = carData[is.na(carData$Number.of.Doors),]
summary(doorNA)


num_cols = unlist(lapply(carData,is.numeric))
num_cols
carNum = carData[,num_cols]
carNum$MSRP = log(carNum$MSRP)
ggpairs(carNum)

ss = sample(1:3, size=nrow(carDataRaw),replace=TRUE,prob=c(0.8,0.1,0.1))
train = carDataRaw[ss==1,]
test = carDataRaw[ss==2,]
val = carDataRaw[ss==3,]



plot(carDataRaw,pch=20)
p = ggpairs(train, title='correlation')
ggpairs(train,columns=3,5,6,9,10:13)