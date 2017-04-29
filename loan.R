#Loading libraries
library(ggplot2)

#Reading files from dir
train <- read.csv("E:/DataScience/Git/TechGigMachineLearning/train_data.csv", header= T, stringsAsFactors= F)
test <- read.csv("E:/DataScience/Git/TechGigMachineLearning/test_data.csv",header = T, stringsAsFactors = F)

str(train)
summary(train)
str(test)
summary(test)

test$Loan_Status <- "NA"      #adding column "Loan_Status" into "test" dataset
allData <- rbind(train,test)   #Combining test and train dataset

#converting features into factors
allData$Gender <- as.factor(allData$Gender)
allData$Married <- as.factor(allData$Married)
allData$Dependents <- as.factor(allData$Dependents)
allData$Education <- as.factor(allData$Education)
allData$Self_Employed <- as.factor(allData$Self_Employed)
allData$Credit_History <- as.factor(allData$Credit_History)
allData$Property_Area <- as.factor(allData$Property_Area)
allData$Loan_Status <- as.factor(allData$Loan_Status)
allData$Loan_Amount_Term <- as.factor(allData$Loan_Amount_Term)


