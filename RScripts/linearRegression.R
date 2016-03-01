rm(list=ls())

#require(knitr)
library(dplyr)
library(tidyr)
library(matrixStats)
library(ggplot2)
library(lubridate)
library(Metrics)
library(caret)


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
  mutate(count = log(count), hour = hour(ymd_hms(train$datetime)))
qplot(train$count)
qplot(trainData$count)
plot(train$count)
plot(trainData$count)
qplot(ymd_hms(train$datetime), train$count)
qplot(ymd_hms(train$datetime), trainData$count)
#train$datetime  <- strptime(train$datetime, "%Y-%m-%d %H:%M:%S")
#train$datetime <- ymd_hms(train$datetime)

test <- read.csv("data/test.csv", header = TRUE)%>%
  mutate(datetime = as.character(datetime))
testData <- select(test, -datetime)%>%
  mutate(hour = hour(ymd_hms(test$datetime)))

#test$datetime  <- strptime(test$datetime, "%Y-%m-%d %H:%M:%S")
#test$datetime <- ymd_hms(test$datetime)

######################################################################
######################################################################

lm.fitAll <- lm(count~.-hour, data = trainData)
summary(lm.fitAll)

#fitAllPredTrain <- round(predict(lm.fitAll, trainData), digits = 0)
fitAllPredTrain <- predict(lm.fitAll, trainData)
plot(fitAllPredTrain)
qplot(fitAllPredTrain)
head(fitAllPredTrain)
min(fitAllPredTrain) #-62 when count isn't log transformed 2.385558 when log transformed
max(fitAllPredTrain) #479 when count isn't log transformed 6.934912 when log transformed
trainAllPredCount <- round(exp(fitAllPredTrain), digits = 0)
min(trainAllPredCount) #11
max(trainAllPredCount) #1028
min(train$count) #1
max(train$count) #977

sum(trainAllPredCount == train$count) / length(trainAllPredCount) #0.003490722
rmsle(trainData$count,fitAllPredTrain) #0.319916
rmsle(train$count, trainAllPredCount) #1.218684

lmfitAllPredTest <- round(exp(predict(lm.fitAll, newdata = testData)), digits = 0)
min(lmfitAllPredTest) #13
max(lmfitAllPredTest) #905

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmfitAllPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmfitAllPredTest.csv", row.names = FALSE)

######################################################################
######################################################################

lm.fitAllhour <- lm(count~., data = trainData)
summary(lm.fitAllhour)

#fitAllPredTrain <- round(predict(lm.fitAll, trainData), digits = 0)
fitAllhourPredTrain <- predict(lm.fitAllhour, trainData)
plot(fitAllhourPredTrain)
qplot(fitAllhourPredTrain)
head(fitAllhourPredTrain)
min(fitAllhourPredTrain) #1.854392
max(fitAllhourPredTrain) #6.805171
trainAllhourPredCount <- round(exp(fitAllhourPredTrain), digits = 0)
min(trainAllhourPredCount) #6
max(trainAllhourPredCount) #903
min(train$count) #1
max(train$count) #977

sum(trainAllhourPredCount == train$count) / length(trainAllhourPredCount) #0.004593055
rmsle(trainData$count,fitAllhourPredTrain) #0.2759522
rmsle(train$count, trainAllhourPredCount) #1.042438

lmfitAllhourPredTest <- round(exp(predict(lm.fitAllhour, newdata = testData)), digits = 0)
min(lmfitAllhourPredTest) #8
max(lmfitAllhourPredTest) #852

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmfitAllhourPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmfitAllhourPredTest.csv", row.names = FALSE)



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

lm.fit3 <- lm(count~temp+atemp+humidity+hour, data = trainData)
summary(lm.fit3)

#fitAllPredTrain <- round(predict(lm.fitAll, trainData), digits = 0)
fit3PredTrain <- predict(lm.fit3, trainData)
plot(fit3PredTrain)
qplot(fit3PredTrain)
head(fit3PredTrain)
min(fit3PredTrain) #2.098511
max(fit3PredTrain) #6.712053
trainPred3Count <- round(exp(fit3PredTrain), digits = 0)
min(trainPred3Count) #8
max(trainPred3Count) #822
min(train$count) #1
max(train$count) #977

sum(trainPred3Count == train$count) / length(trainPred3Count) #0.004225611
rmsle(trainData$count,fit3PredTrain) #0.2800687
rmsle(train$count, trainPred3Count) #1.059169

lmfit3PredTest <- round(exp(predict(lm.fit3, newdata = testData)), digits = 0)
min(lmfit3PredTest) #17
max(lmfit3PredTest) #866

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmfit3PredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmfit3PredTest.csv", row.names = FALSE)

########################################################################################
########################################################################################


set.seed(825)
#Linear Regression with Backwards Selection
lmBackFit <- train(count ~ ., data = trainData,
                 method = "leapBackward")
summary(lmBackFit)
lmBackFitPredTrain <- predict(lmBackFit, trainData)

min(lmBackFitPredTrain) #1.81754
max(lmBackFitPredTrain) #6.728031
lmBackFitPredCount <- round(exp(lmBackFitPredTrain), digits = 0)
min(lmBackFitPredCount) #6
max(lmBackFitPredCount) #836
min(train$count) #1
max(train$count) #977

sum(lmBackFitPredCount == train$count) / length(lmBackFitPredCount) #0.004593055
rmsle(trainData$count,lmBackFitPredTrain) #0.2764287
rmsle(train$count, lmBackFitPredCount) #1.0435

lmBackFitPredTest <- round(exp(predict(lmBackFit, newdata = testData)), digits = 0)
min(lmBackFitPredTest) # 8
max(lmBackFitPredTest) #884

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmBackFitPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmBackFitPredTest.csv", row.names = FALSE)

########################################################################################
########################################################################################


#Linear Regression with Forward Selection
lmForwFit <- train(count ~ ., data = trainData,
                   method = "leapForward")
summary(lmForwFit)

lmForwFitPredTrain <- predict(lmForwFit, trainData)
plot(lmForwFitPredTrain)
qplot(lmForwFitPredTrain)
head(lmForwFitPredTrain)
min(lmForwFitPredTrain) #1.901205
max(lmForwFitPredTrain) #6.796463
trainPredlmForwFitCount <- round(exp(lmForwFitPredTrain), digits = 0)
min(trainPredlmForwFitCount) #7
max(trainPredlmForwFitCount) #895
min(train$count) #1
max(train$count) #977

sum(trainPredlmForwFitCount == train$count) / length(trainPredlmForwFitCount) #0.004317472
rmsle(trainData$count,lmForwFitPredTrain) #0.2765883
rmsle(train$count, trainPredlmForwFitCount) #1.044539

lmForwFitPredTest <- round(exp(predict(lmForwFit, newdata = testData)), digits = 0)
min(lmForwFitPredTest) #8
max(lmForwFitPredTest) #885

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmForwFitPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmForwFitPredTest.csv", row.names = FALSE)

########################################################################################
########################################################################################

#Linear Regression with Stepwise Selection
lmSeqFit <- train(count ~ ., data = trainData,
                   method = "leapSeq")
summary(lmSeqFit)

lmSeqFitPredTrain <- predict(lmSeqFit, trainData)
plot(lmSeqFitPredTrain)
qplot(lmSeqFitPredTrain)
head(lmSeqFitPredTrain)
min(lmSeqFitPredTrain) #1.81754
max(lmSeqFitPredTrain) #6.728031
trainPredlmSeqFitCount <- round(exp(lmSeqFitPredTrain), digits = 0)
min(trainPredlmSeqFitCount) #6
max(trainPredlmSeqFitCount) #836
min(train$count) #1
max(train$count) #977

sum(trainPredlmSeqFitCount == train$count) / length(trainPredlmSeqFitCount) #0.004593055
rmsle(trainData$count,lmSeqFitPredTrain) #0.2764287
rmsle(train$count, trainPredlmSeqFitCount) #1.0435

lmSeqFitPredTest <- round(exp(predict(lmSeqFit, newdata = testData)), digits = 0)
min(lmSeqFitPredTest) #8
max(lmSeqFitPredTest) #884

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmSeqFitPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmSeqFitPredTest.csv", row.names = FALSE)

########################################################################################
########################################################################################


#Linear Regression with Stepwise Selection
lmStepAICFit <- train(count ~ ., data = trainData,
                  method = "lmStepAIC")
summary(lmStepAICFit)

lmStepAICFitPredTrain <- predict(lmStepAICFit, trainData)
plot(lmStepAICFitPredTrain)
qplot(lmStepAICFitPredTrain)
head(lmStepAICFitPredTrain)
min(lmStepAICFitPredTrain) #1.838903
max(lmStepAICFitPredTrain) #6.779881
trainPredlmStepAICFitCount <- round(exp(lmStepAICFitPredTrain), digits = 0)
min(trainPredlmStepAICFitCount) #6
max(trainPredlmStepAICFitCount) #880
min(train$count) #1
max(train$count) #977

sum(trainPredlmStepAICFitCount == train$count) / length(trainPredlmStepAICFitCount) #0.004776778
rmsle(trainData$count,lmStepAICFitPredTrain) #0.2760146
rmsle(train$count, trainPredlmStepAICFitCount) #1.042635

lmStepAICFitPredTest <- round(exp(predict(lmStepAICFit, newdata = testData)), digits = 0)
min(lmStepAICFitPredTest) #8
max(lmStepAICFitPredTest) #870

# Create a data frame with two columns: datetime & count. 
my_solution <- data.frame(datetime = test$datetime, count = lmStepAICFitPredTest)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "Predictions/lmStepAICFitPredTest.csv", row.names = FALSE)

