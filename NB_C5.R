
rm(list=ls())
library(randomForest)
library(dbplyr)
dataSet<-read.csv("C:/Users/Dyuti/Downloads/attrition_data.csv",na.strings = '?')
View(dataSet)
set.seed(80)
#To factor the data set
library(C50)
dataSet$STATUS <- factor(dataSet$STATUS, levels = c("T","A"),labels = c("0", "1"))
dataSet$ETHNICITY <- factor(dataSet$ETHNICITY, levels = c("BLACK","ASIAN","WHITE","HISPA","PACIF","TWO","AMIND","Unknown"),labels = c("1", "2","3","4","5","6","7","0"))
dataSet$SEX <- factor(dataSet$SEX, levels = c("M","F"),labels = c("0", "1"))
dataSet$MARITAL_STATUS <- factor(dataSet$MARITAL_STATUS, levels = c("Single","Divorced","Married"),labels = c("0", "1","2"))
dataSet$REFERRAL_SOURCE <- sub("^$", "Unknown", dataSet$REFERRAL_SOURCE)
dataSet$TERMINATION_YEAR[is.na(dataSet$TERMINATION_YEAR)]= "2030"
dataSet$IS_FIRST_JOB <- factor(dataSet$IS_FIRST_JOB, levels = c("Y","N"),labels = c("0", "1"))
dataSet$TRAVELLED_REQUIRED <- factor(dataSet$TRAVELLED_REQUIRED, levels = c("Y","N"),labels = c("0", "1"))
dataSet$REHIRE <- factor(dataSet$REHIRE, levels = c("TRUE","FALSE"),labels = c("0", "1"))
dataSet$DISABLED_EMP <- factor(dataSet$DISABLED_EMP, levels = c("Y","N"),labels = c("0", "1"))
dataSet$DISABLED_VET <- factor(dataSet$DISABLED_VET, levels = c("Y","N"),labels = c("0", "1"))
dataSet$EDUCATION_LEVEL <- factor(dataSet$EDUCATION_LEVEL, levels = c("LEVEL 1","LEVEL 2","LEVEL 3","LEVEL 4","LEVEL 5"),labels = c("1", "2","3","4","5"))


dataSet$AGE<-ifelse(dataSet$AGE >= 0&dataSet$AGE <35, "Less than 35 yr old", (ifelse(dataSet$AGE  >= 35&dataSet$AGE <=50, "More than 35 yr old but less than 50","Older than 50 yr")))
dataSet$HRLY_RATE<-ifelse(dataSet$HRLY_RATE >= 0&dataSet$HRLY_RATE <60, "Less than 60 per hr", (ifelse(dataSet$HRLY_RATE  >= 60&dataSet$HRLY_RATE <=120, "More than 60 per hr but less than 120","Greater than 120 yr")))

dataSet<-na.omit(dataSet)
#Identifying missing values
dataSet[dataSet == "?"] <- NA
any(is.na(dataSet))

colSums(is.na(dataSet))
unique(dataSet$JOB_GROUP)
summary(dataSet)

#Drop columns JOB_group,emp id, job code, annual rate
dataSet <- dataSet[ -c(22,1,2,4) ]

#conert char to factor for matrix formation
dataSet$AGE <- as.factor(dataSet$AGE)
dataSet$REFERRAL_SOURCE <- as.factor(dataSet$REFERRAL_SOURCE)
dataSet$TERMINATION_YEAR <- as.factor(dataSet$TERMINATION_YEAR)
dataSet$HRLY_RATE <- as.factor(dataSet$HRLY_RATE)
# To split the data set into test and testing 
idx<-sort(sample(nrow(dataSet),as.integer(.75*nrow(dataSet))))
training<-dataSet[idx,]
test<-dataSet[-idx,]

###################
#Implement C 5.0
model<-C5.0(STATUS~.,training[,-1])
summary(model)
plot(model)
#Prediction using test 
prediction<-predict(model,test[,-1],type="class") 
#Forming the confusin matrix
conf_matrix<-table(test[,23],prediction)
conf_matrix
str(prediction)
#Showing the error rate 
wrong<-sum(test[,23]!=prediction)
error_rate<-wrong/length(test[,23])
error_rate

acc<- function(x){
  sum(diag(x))/sum(rowSums(x))*100
}
acc(conf_matrix)
###########################
##try this! 
library(neuralnet)

dataSet<-read.csv("C:/Users/Dyuti/Downloads/attrition_data.csv",na.strings = '?')
View(dataSet)
table(dataSet$diagnosis)

#To factor the data set
dataSet<-data.frame(lapply(na.omit(dataSet),as.numeric))

# To split the data set into test and testing 
idx<-sort(sample(nrow(dataSet),as.integer(.70*nrow(dataSet))))
training<-dataSet[idx,]
test<-dataSet[-idx,]
?neuralnet()
model<- neuralnet(STATUS~.,training[-1], hidden=5, threshold=0.01)

#Plot the neural network
plot(model)

## test should have only the input colum
ann <-compute(model,test)
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
length(test$STATUS)
table(ann_cat,test$STATUS)

wrong<- (test$STATUS!=ann_cat)
errorRate<-sum(wrong)/length(wrong)
errorRate
#####################################
library(klaR)
library(e1071)
?naiveBayes

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(STATUS ~., data=training)
#What does the model say? Print the model summary
Naive_Bayes_Model
#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,test)
#Confusion matrix to check accuracy
table(NB_Predictions,test$STATUS)
wrong<- (test$STATUS!=NB_Predictions)
errorRate<-sum(wrong)/length(wrong)
errorRate
################################ 
library(e1071)
#Fit a model. The function syntax is very similar to lm function
model_svm <- svm(STATUS~. , data= training)
#Use the predictions on the data
pred <- predict(model_svm, training)

