#Loading libraries
library(ggplot2)

#Reading files from dir
train <- read.csv("E:/DataScience/Git/TechGigMachineLearning/train_data.csv", header= T, stringsAsFactors= F)
test <- read.csv("E:/DataScience/Git/TechGigMachineLearning/test_data.csv",header = T, stringsAsFactors = F)

test$Loan_Status <- "NA"
allData <- rbind(train,test)

allData$Gender <- as.factor(allData$Gender)


str(train)

