rm(list=ls())

#require(knitr)
require(dplyr)
require(tidyr)
require(matrixStats)
require(ggplot2)
require(lubridate)
require(Metrics)
require(caret)

#Set working directories
my_dir1 <- "C:/Docs/Dropbox/PhD/classes/STAT695/BikeShare"
#my_dir2 <- ""

if(dir.exists(my_dir1)){
  setwd(my_dir1)
}#else if(dir.exists(my_dir2)){
#  setwd(my_dir2)

train <- read.csv("data/train.csv", header = TRUE)%>%
  mutate(datetime = as.character(datetime))
trainData <- select(train, -casual, -registered, -datetime)%>%
  mutate(count = log(count))

#train$datetime  <- strptime(train$datetime, "%Y-%m-%d %H:%M:%S")
#train$datetime <- ymd_hms(train$datetime)

test <- read.csv("data/test.csv", header = TRUE)%>%
  mutate(datetime = as.character(datetime))
testData <- select(test, -datetime)

#test$datetime  <- strptime(test$datetime, "%Y-%m-%d %H:%M:%S")
#test$datetime <- ymd_hms(test$datetime)

######################################################################
######################################################################

lm.fitAll <- lm(count~., data = trainData)
summary(lm.fitAll)

#fitAllPredTrain <- round(predict(lm.fitAll, trainData), digits = 0)
fitAllPredTrain <- predict(lm.fitAll, trainData)
plot(fitAllPredTrain)
qplot(fitAllPredTrain)
head(fitAllPredTrain)
min(fitAllPredTrain) #-62 when count isn't log transformed 2.385558 when log transformed
max(fitAllPredTrain) #479 when count isn't log transformed 6.934912 when log transformed
trainPredCount <- round(exp(fitAllPredTrain), digits = 0)
min(trainPredCount) #11
max(trainPredCount) #1028
min(train$count) #1
max(train$count) #977

sum(trainPredCount == train$count) / length(trainPredCount) #0.003490722
rmsle(trainData$count,fitAllPredTrain) #0.319916
rmsle(train$count, trainPredCount) #1.218684

lmfitAllPredTest <- round(exp(predict(lm.fitAll, newdata = testData)), digits = 0)
min(lmfitAllPredTest) #13
max(lmfitAllPredTest) #905

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmfitAllPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmlmfitAllPredTest.csv", row.names = FALSE)

########################################################################################
########################################################################################

lm.fit2 <- lm(count~temp+atemp+humidity, data = trainData)
summary(lm.fit2)

#fitAllPredTrain <- round(predict(lm.fitAll, trainData), digits = 0)
fit2PredTrain <- predict(lm.fit2, trainData)
plot(fit2PredTrain)
qplot(fit2PredTrain)
head(fit2PredTrain)
min(fit2PredTrain) #2.732911
max(fit2PredTrain) #6.863719
trainPred2Count <- round(exp(fit2PredTrain), digits = 0)
min(trainPred2Count) #15
max(trainPred2Count) #957
min(train$count) #1
max(train$count) #977

sum(trainPred2Count == train$count) / length(trainPred2Count) #0.003582583
rmsle(trainData$count,fit2PredTrain) #0.3243039
rmsle(train$count, trainPred2Count) #1.238186

lmfit2PredTest <- round(exp(predict(lm.fit2, newdata = testData)), digits = 0)
min(lmfit2PredTest) #17
max(lmfit2PredTest) #787

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmfit2PredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmfit2PredTest.csv", row.names = FALSE)

########################################################################################
########################################################################################
#### How to use RMSLE rather than RMSE for training?
set.seed(825)
#Linear Regression with Backwards Selection
lmBackFit <- train(count ~ ., data = trainData,
                 method = "leapBackward")
#Linear Regression with Forward Selection
lmForwFit <- train(count ~ ., data = trainData,
                   method = "leapForward")
#Linear Regression with Stepwise Selection
lmSeqFit <- train(count ~ ., data = trainData,
                   method = "leapSeq")
#Linear Regression with Stepwise Selection
lmStepAICFit <- train(count ~ ., data = trainData,
                  method = "leapStepAIC")



