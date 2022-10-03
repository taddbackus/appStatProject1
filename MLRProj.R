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
set.seed(1234)
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
# # Playing with LASSO
# fitControl<-trainControl(method="repeatedcv",number=5,repeats=1) 
# 
# 
# #Fitting glmnet
# set.seed(1234)
# glmnet.fit<-train(MSRP~combMPG+cylinders+horsepower+Year+doors+Make+fuelType+transmission+drive+Year:yearCat,
#                   data=train,
#                   method="glmnet",
#                   trControl=fitControl,
#                   tuneGrid=expand.grid(data.frame(alpha=1,lambda=seq(0,.05,.001)))
# )
# 
# glmnet.fit
# plot(glmnet.fit)
# 
# 
# #Investigating coefficients
# opt.pen<-glmnet.fit$finalModel$lambdaOpt #penalty term
# coef(glmnet.fit$finalModel,opt.pen)
# AIC(glmnet.fit)


##### my attempt

MSRPaov = aov(MSRP ~ .,data = train)
summary(MSRPaov)

Popularty_VS_All = aov(Popularity ~., data = train)
summary(Popularty_VS_All)

# TukeyHSD(MSRPaov)

# popularity of brand vs year 

train %>% group_by(Make) %>% 
  summarise_at(c("Popularity", "Year","MSRP"), mean, na.rm = TRUE) %>%
  ggplot(aes(x = Year, y = Popularity, color = MSRP)) + geom_point() +
  geom_smooth(method=glm,linetype="dashed",color="black") + 
  scale_color_gradient(low = "red", high = "black") + 
  labs(title = "Year vs Popularity, Color Scaled with MSRP")


# cylinders vs msrp

ggplot(train, aes(x=as.factor(cylinders),y=MSRP, fill = as.factor(cylinders))) + 
  geom_boxplot() + labs(title = "Cylinders vs MSRP", x = "Cylinders") + 
  theme(legend.position="none")

MSRPaovPred <- predict(MSRPaov, newdata = test, na.action = na.pass)
RMSE(pred = MSRPaovPred, obs = test$MSRP)


par(mfrow=c(2,2))
plot(MSRPaov)
par(mfrow=c(1,1))

qqnorm(train$Popularity)
qqline(train$Popularity)

qqnorm(train$MSRP)
qqline(train$MSRP)


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

       
testingset = rbind(testT,valT)
Predictions = predict(glmnet.fit2,testingset)
trainingPred = cbind(glmnet.fit2$results,trainT)

FullSet = cbind(Predictions,testingset)
MSRP = exp(FullSet$MSRP)
Predictions = exp(FullSet$Predictions)
TestASEError = mean((Predictions-MSRP)/MSRP)
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


library(ranger)

net = seq(1,500,5)


OOB = vector("numeric", length(net))
rs = vector("numeric", length(net))
ntree = vector("numeric", length(net))

for (i in 1:length(net)){
rf = ranger(exp(MSRP) ~., trainT, num.trees = net[i],mtry = 4, importance = 'impurity', write.forest = TRUE)
OOB[i] = rf$prediction.error
rs[i] = rf$r.squared
ntree[i] = rf$num.trees


}
alldata = data.frame(cbind(OOB,rs,ntree))
colnames(alldata) = c("OutofBag", "RSquared", "NumberofTrees")


datatorun = alldata[which.min(alldata$OutofBag),]


rerun = ranger(exp(MSRP)~., trainT, num.trees = datatorun$NumberofTrees, importance = 'impurity', write.forest = TRUE)


# Creating Gini Score Data Frame
GiniScores = data.frame(GiniScore = c(rerun$variable.importance),"Variable" = c(t(colnames(subset(trainT,select = -c(MSRP))))))


##Creating Indexed Gini Scores
GiniScores$med = median(GiniScores$GiniScore)
GiniScores$Index = GiniScores$GiniScore / GiniScores$med

### Final Predictors

Predictors = (GiniScores %>% filter(Index >=1)) %>% select(Variable)
Predictors2 = t(Predictors)
slimmeddata = cbind(data.frame(trainT[,colnames(trainT) %in% c(Predictors2)]),data.frame("MSRP" =trainT$MSRP))


###Final MLR

complexModel2 = lm(exp(MSRP)~.,data=slimmeddata)
summary(complexModel2)
AIC(complexModel2)

Predictors3 = data.frame("MSRP"= c("MSRP"))
Predictors3 = cbind(Predictors2,Predictors3)
val = valT[,colnames(trainT) %in% c(Predictors2)]
valMSRP = valT[,colnames(trainT) %in% c(Predictors3)]
test = testT[,colnames(trainT) %in% c(Predictors2)]
testMSRP = testT[,colnames(trainT) %in% c(Predictors3)]
test = rbind(test,val)
testMSRP = rbind(testMSRP,valMSRP)
levels = complexModel2$xlevels
Models = data.frame("Model" =levels$Model)

test = test %>% filter(Model %in% Models$Model)
testMSRP = testMSRP %>% filter(Model %in% Models$Model)
predictions_final = predict(complexModel2,newdata=test)
fitControl<-trainControl(method="repeatedcv",number=5,repeats=1) 
finalset = cbind(predictions_final,test)
finalset = cbind(finalset,testMSRP$MSRP)
finalset$`testMSRP$MSRP` = exp(finalset$`testMSRP$MSRP`)

colnames(finalset)[colnames(finalset) == "testMSRP$MSRP"] ="BeginMSRP"

Col1 = finalset$MSRP
col2 = finalset$BeginMSRP
AverageError =mean((finalset$predictions_final-finalset$BeginMSRP)/finalset$BeginMSRP)
TotalErrors = data.frame("Complex Model Error" = AverageError,"Initial Model Error" = TestASEError)
























