#Loading libraries
library(ggplot2)
library(nnet)
library(randomForest)

#Reading files from dir
train <- read.csv("E:/DataScience/Git/TechGigMachineLearning/train_data.csv", header= T, stringsAsFactors= F)
test <- read.csv("E:/DataScience/Git/TechGigMachineLearning/test_data.csv",header = T, stringsAsFactors = F)

str(train)
summary(train)
str(test)
summary(test)

test$Loan_Status <- NA     #adding column "Loan_Status" into "test" dataset
allData <- rbind(train,test)   #Combining test and train dataset

#converting features into factors
allData$Gender <- factor(allData$Gender, levels = c("M","F"))
allData$Married <- factor(allData$Married, levels = c("Yes","No"))
allData$Dependents <- factor(allData$Dependents, levels = c("0","1","2","3+"))
allData$Education <- factor(allData$Education)
allData$Self_Employed <- factor(allData$Self_Employed, levels =c("Yes","No"))
allData$Credit_History <- as.factor(allData$Credit_History)
allData$Property_Area <- as.factor(allData$Property_Area)
allData$Loan_Status <- as.factor(allData$Loan_Status)
allData$Loan_Amount_Term <- as.factor(allData$Loan_Amount_Term)

str(allData)
summary(allData)

#Handling Married NA
data_marry <- allData[is.na(allData$Married),]

data_marry_not <- allData[!is.na(allData$Married),]

data_marry$Married <- "No"

fillDependents <- function(x,y){
  
  for(i in 1:3){
    
    if(x[i]>0)
      y[i]=1        
    else
      y[i]=0
    
  }
  return(y)
}

d <- fillDependents(data_marry$CoapplicantIncome,data_marry$Dependents)
data_marry$Dependents <- d

allData <- rbind(data_marry,data_marry_not)
allData <- allData[order(allData$Application_ID),]

allData$Married <- factor(allData$Married, levels = c("Yes","No"))

#Handling Gender NA
allData$Gender<- as.numeric(allData$Gender)
data_gender <- allData[is.na(allData$Gender),]
data_gender1 <- allData[!is.na(allData$Gender),]



data_gender2 <- subset(data_gender1,!is.na(data_gender1$Dependents) & !is.na(data_gender1$Self_Employed) & !is.na(data_gender1$LoanAmount))


m1<- glm(Gender ~ Married + Dependents +ApplicantIncome + CoapplicantIncome + LoanAmount, data = data_gender2)

p1<- predict(m1, data_gender)
p1 <- round(p1, digits = 0)
data_gender$Gender <- p1

allData <- rbind(data_gender, data_gender1)

allData$Gender <- as.factor(allData$Gender)

#handling dependents NA
data_dep <- allData[is.na(allData$Dependents),]
data_dep1 <- allData[!is.na(allData$Dependents),]

m2 <- multinom(Dependents ~ Married + Gender + Education + Self_Employed +ApplicantIncome + CoapplicantIncome, data = data_dep1)
p2 <- predict(m2,data_dep)
data_dep$Dependents <- p2

allData <- rbind(data_dep1, data_dep)


#handling self-employed NA
data_se <- allData[is.na(allData$Self_Employed),]
data_se1 <- allData[!is.na(allData$Self_Employed),]

m3 <- multinom(Self_Employed ~ Gender + Married + Education + ApplicantIncome + CoapplicantIncome + Property_Area, data = data_se1)

p3 <- predict(m3,data_se)

data_se$Self_Employed <- p3

allData <- rbind(data_se, data_se1)

#Applying Linear Regression to fill missing values of LoanAmount
Data_LoanAmount_train <- allData[!is.na(allData$LoanAmount),]
Data_LoanAmount_test <-  allData[is.na(allData$LoanAmount),]

m4 <- lm(LoanAmount ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome, data = Data_LoanAmount_train)
p4 <- predict(m4,Data_LoanAmount_test)
p4 <-round(p4, digits = 0)
Data_LoanAmount_test$LoanAmount <-p4

allData <- rbind(Data_LoanAmount_train, Data_LoanAmount_test)


#Applying logistic Regression to fill Loan Amount Term
Data_LAT_train <- allData[!is.na(allData$Loan_Amount_Term),]
Data_LAT_test <-  allData[is.na(allData$Loan_Amount_Term),]

m5 <- multinom(Loan_Amount_Term ~ Gender + Married + Dependents + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Property_Area, data = Data_LAT_train)
p5<- predict(m5,Data_LAT_test)
Data_LAT_test$Loan_Amount_Term <- p5

allData <- rbind(Data_LAT_train,Data_LAT_test)

# Credit History
data_credit_train <- allData[!is.na(allData$Credit_History),]
data_credit_test <- allData[is.na(allData$Credit_History),]

m6 <- multinom(Credit_History ~ Gender+Married+ Dependents +Self_Employed + Education + ApplicantIncome + CoapplicantIncome+ LoanAmount + Loan_Amount_Term + Property_Area, data = data_credit_train)
p6 <- predict(m6,data_credit_test)
 
data_credit_test$Credit_History <- p6

allData <- rbind(data_credit_train, data_credit_test)

allData <- allData[order(allData$Application_ID),]

train <- allData[!is.na(allData$Loan_Status),]
test <- allData[is.na(allData$Loan_Status),]


#Final Model 
model <- randomForest(Loan_Status~.,data = train, ntree=500)
pF <- predict(model, test)
test$Loan_Status <- pF

Application_ID <- test$Application_ID
output <- as.data.frame(Application_ID)
output$Loan_Staus <- test$Loan_Status
write.csv(output,"Submission.csv", row.names = F)


