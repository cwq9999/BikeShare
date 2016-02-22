rm(list=ls())

#require(knitr)
require(dplyr)
require(tidyr)
require(matrixStats)
require(ggplot2)
require(lubridate)
require(Metrics)

#Set working directories
my_dir1 <- "C:/Docs/Dropbox/PhD/classes/STAT695/BikeShare"
#my_dir2 <- ""

if(dir.exists(my_dir1)){
  setwd(my_dir1)
}#else if(dir.exists(my_dir2)){
#  setwd(my_dir2)

train <- read.csv("data/train.csv", header = TRUE)%>%
  mutate(datetime = as.character(datetime))
trainData <- select(train, -casual, -registered, -datetime)
 
#train$datetime  <- strptime(train$datetime, "%Y-%m-%d %H:%M:%S")
#train$datetime <- ymd_hms(train$datetime)

test <- read.csv("data/test.csv", header = TRUE)%>%
  mutate(datetime = as.character(datetime))
testData <- select(test, -datetime)

#test$datetime  <- strptime(test$datetime, "%Y-%m-%d %H:%M:%S")
#test$datetime <- ymd_hms(test$datetime)

lm.fitAll <- lm(count~., data = trainData)
summary(lm.fitAll)

fitAllPredTrain <- round(predict(lm.fitAll, trainData), digits = 0)
plot(fitAllPredTrain)
qplot(fitAllPredTrain)
head(fitAllPredTrain)
min(fitAllPredTrain) #-62
max(fitAllPredTrain) #479
sum(fitAllPredTrain == train$count) / length(fitAllPredTrain)
rmsle(train$count, fitAllPredTrain) #Warning message: In log(1 + predicted) : NaNs produced

fitAllPred <- predict(lm.fitAll, newdata = testData)
