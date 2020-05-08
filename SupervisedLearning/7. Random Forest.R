# # # # # # # # # # # # # # #
#  Algorithm : Random Forest#
# # # # # # # # # # # # # # #
library('randomForest')

#Clear all previous variables
rm(list = ls())

#Set working directory 
setwd("C:/Users/Dyuti/Downloads")

#Import breastcancer dataset
dataset<-read.csv(file = 'attrition_data.csv',stringsAsFactors = FALSE)

# Remove the first column of the dataset
dataset<-dataset[ -c(22,1,2,4)]
#Replace all NA with zeroes

dataset$STATUS <- factor(dataset$STATUS, levels = c("T","A"),labels = c("0", "1"))
dataset$ETHNICITY <- factor(dataset$ETHNICITY, levels = c("BLACK","ASIAN","WHITE","HISPA","PACIF","TWO","AMIND","Unknown"),labels = c("1", "2","3","4","5","6","7","0"))
dataset$SEX <- factor(dataset$SEX, levels = c("M","F"),labels = c("0", "1"))
dataset$MARITAL_STATUS <- factor(dataset$MARITAL_STATUS, levels = c("Single","Divorced","Married"),labels = c("0", "1","2"))
dataset$REFERRAL_SOURCE <- sub("^$", "Unknown", dataset$REFERRAL_SOURCE)
dataset$TERMINATION_YEAR[is.na(dataset$TERMINATION_YEAR)]= "2030"
dataset$IS_FIRST_JOB <- factor(dataset$IS_FIRST_JOB, levels = c("Y","N"),labels = c("0", "1"))
dataset$TRAVELLED_REQUIRED <- factor(dataset$TRAVELLED_REQUIRED, levels = c("Y","N"),labels = c("0", "1"))
dataset$REHIRE <- factor(dataset$REHIRE, levels = c("TRUE","FALSE"),labels = c("0", "1"))
dataset$DISABLED_EMP <- factor(dataset$DISABLED_EMP, levels = c("Y","N"),labels = c("0", "1"))
dataset$DISABLED_VET <- factor(dataset$DISABLED_VET, levels = c("Y","N"),labels = c("0", "1"))
dataset$EDUCATION_LEVEL <- factor(dataset$EDUCATION_LEVEL, levels = c("LEVEL 1","LEVEL 2","LEVEL 3","LEVEL 4","LEVEL 5"),labels = c("1", "2","3","4","5"))


dataset$AGE<-ifelse(dataset$AGE >= 0&dataset$AGE <35, "Less than 35 yr old", (ifelse(dataset$AGE  >= 35&dataset$AGE <=50, "More than 35 yr old but less than 50","Older than 50 yr")))
dataset$HRLY_RATE<-ifelse(dataset$HRLY_RATE >= 0&dataset$HRLY_RATE <60, "Less than 60 per hr", (ifelse(dataset$HRLY_RATE  >= 60&dataset$HRLY_RATE <=120, "More than 60 per hr but less than 120","Greater than 120 yr")))
#dataset[dataset == '?']<-0
dataset$AGE <- as.factor(dataset$AGE)
dataset$REFERRAL_SOURCE <- as.factor(dataset$REFERRAL_SOURCE)
dataset$TERMINATION_YEAR <- as.factor(dataset$TERMINATION_YEAR)
dataset$HRLY_RATE <- as.factor(dataset$HRLY_RATE)
dataset$HIRE_MONTH <- as.factor(dataset$HIRE_MONTH)
dataset$NUMBER_OF_TEAM_CHANGED <- as.factor(dataset$NUMBER_OF_TEAM_CHANGED)
dataset$JOB_SATISFACTION <- as.factor(dataset$JOB_SATISFACTION)
dataset$PERFORMANCE_RATING <- as.factor(dataset$PERFORMANCE_RATING)
dataset$PREVYR_1 <- as.factor(dataset$PREVYR_1)
dataset$PREVYR_2 <- as.factor(dataset$PREVYR_2)
dataset$PREVYR_3 <- as.factor(dataset$PREVYR_3)
dataset$PREVYR_4 <- as.factor(dataset$PREVYR_4)
dataset$PREVYR_5 <- as.factor(dataset$PREVYR_5)

dataset$STATUS <- factor(dataset$STATUS, levels = c("0","1"),labels = c("T", "A"))

dataset<-na.omit(dataset)
#Split training and testing(training = 75% , testing = 25%)
set.seed(135)
#dataset <- data.matrix(dataset)
index<-sort(sample(nrow(dataset),round(0.25*nrow(dataset))))
training<-dataset[-index,]
testing<-dataset[index,]


#training <- data.matrix(training)
#testing <- data.matrix(testing)
# Implement Random Forest alogrithm
randomForest_class<-randomForest(STATUS~.,data = training, importance=TRUE, ntree=5000)
summary(randomForest_class)
plot(randomForest_class)
randomForest_predict<-predict( randomForest_class ,testing , type="class" )

randomForest_predict
#View(testing)
#Confusin Matrix

#######################

fit_importance <- importance(randomForest_class)
fit_importance

# Get top 15 variables from importance - these will be used in future algoritms
top_features <- sort(fit_importance[,3], decreasing=TRUE)[1:10]
top_features

# Check Results
varImpPlot(randomForest_class)
Prediction <- predict(randomForest_class, testing)
library(caret)
confusionMatrix(testing$STATUS, Prediction)

#accuracy 99%
