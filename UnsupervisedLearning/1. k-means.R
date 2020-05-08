# # # # # # # # # # # # # # # # # #
#  Algorithm : k-means clustering #
# # # # # # # # # # # # # # # # # #

rm(list=ls())

library(clue)
dataSet<-read.csv("C:/Users/Dyuti/Downloads/attrition_data.csv",na.strings = '?')

set.seed(50)
#To factor the data set
#dataSet$ETHNICITY[is.na(dataSet$ETHNICITY)]= "Unknown"

#dataSet$ETHNICITY <- sub("", "0", dataSet$ETHNICITY)
dataSet$STATUS <- factor(dataSet$STATUS, levels = c("T","A"),labels = c("0", "1"))
dataSet$ETHNICITY <- factor(dataSet$ETHNICITY, levels = c("BLACK","ASIAN","WHITE","HISPA","PACIF","TWO","AMIND","Unknown"),labels = c("1", "2","3","4","5","6","7","0"))
dataSet$SEX <- factor(dataSet$SEX, levels = c("M","F"),labels = c("0", "1"))
dataSet$MARITAL_STATUS <- factor(dataSet$MARITAL_STATUS, levels = c("Single","Divorced","Married"),labels = c("0", "1","2"))
#dataSet$REFERRAL_SOURCE[is.na(dataSet$REFERRAL_SOURCE)]= "Unknown"
dataSet$REFERRAL_SOURCE <- sub("^$", "Unknown", dataSet$REFERRAL_SOURCE)

dataSet$TERMINATION_YEAR[is.na(dataSet$TERMINATION_YEAR)]= "2030"
dataSet$IS_FIRST_JOB <- factor(dataSet$IS_FIRST_JOB, levels = c("Y","N"),labels = c("0", "1"))
dataSet$TRAVELLED_REQUIRED <- factor(dataSet$TRAVELLED_REQUIRED, levels = c("Y","N"),labels = c("0", "1"))
dataSet$REHIRE <- factor(dataSet$REHIRE, levels = c("TRUE","FALSE"),labels = c("0", "1"))
dataSet$DISABLED_EMP <- factor(dataSet$DISABLED_EMP, levels = c("Y","N"),labels = c("0", "1"))
dataSet$DISABLED_VET <- factor(dataSet$DISABLED_VET, levels = c("Y","N"),labels = c("0", "1"))
dataSet$EDUCATION_LEVEL <- factor(dataSet$EDUCATION_LEVEL, levels = c("LEVEL 1","LEVEL 2","LEVEL 3","LEVEL 4","LEVEL 5"),labels = c("1", "2","3","4","5"))
#

dataSet$AGE<-ifelse(dataSet$AGE >= 0&dataSet$AGE <35, "Less than 35 yr old", (ifelse(dataSet$AGE  >= 35&dataSet$AGE <=50, "More than 35 yr old but less than 50","Older than 50 yr")))
dataSet<-na.omit(dataSet)
#Identifying missing values
dataSet$HRLY_RATE<-ifelse(dataSet$HRLY_RATE >= 0&dataSet$HRLY_RATE <60, "Less than 60 per hr", (ifelse(dataSet$HRLY_RATE  >= 60&dataSet$HRLY_RATE <=120, "More than 60 per hr but less than 120","Greater than 120 yr")))


dataSet[dataSet == "?"] <- NA
any(is.na(dataSet))

colSums(is.na(dataSet))
#unique(dataSet$JOB_GROUP)
summary(dataSet)
#f=data.frame(dataSet$JOB_GROUP)
#model.matrix(f)
#Drop columns JOB_group,emp id, job code, annual rate, hrly rate,
dataSet<-dataSet[ -c(22,1,2,4)]

#conert char to factor for matrix formation
dataSet$AGE <- as.factor(dataSet$AGE)
dataSet$REFERRAL_SOURCE <- as.factor(dataSet$REFERRAL_SOURCE)
dataSet$TERMINATION_YEAR <- as.factor(dataSet$TERMINATION_YEAR)
dataSet$HRLY_RATE <- as.factor(dataSet$HRLY_RATE)
#feature selection

#custom<-c('JOB_SATISFACTION','PERFORMANCE_RATING')
## Normalize the data led to NA when converting to matrix
#for (i in custom) {
# dataSet[i] <- (dataSet[,i] - mean(dataSet[,i])) / sd(dataSet[,i])
#} 
# To split the data set into test and testing 
features<-c('HRLY_RATE','ETHNICITY','SEX','MARITAL_STATUS','JOB_SATISFACTION','AGE','NUMBER_OF_TEAM_CHANGED','REFERRAL_SOURCE','HIRE_MONTH','REHIRE','TERMINATION_YEAR','IS_FIRST_JOB','TRAVELLED_REQUIRED','PERFORMANCE_RATING','DISABLED_EMP','DISABLED_VET','EDUCATION_LEVEL','PREVYR_1','PREVYR_2','PREVYR_3','PREVYR_4','PREVYR_5')
idx<-sort(sample(nrow(dataSet),as.integer(.70*nrow(dataSet))))
training<-dataSet[idx,]
test<-dataSet[-idx,]

dataSet <- data.matrix(dataSet)
training <- data.matrix(training)
test <- data.matrix(test)


##--------------kmeans
set.seed(60)
kmeans_2<- kmeans(training[,features],3,nstart = 10)
#kmeans_2$cluster
kmeans <- kmeans(training[,features], centers=7, nstart=10)
o=order(kmeans$cluster)
library(cluster)
clusplot(training[,-1], kmeans$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

library(fpc)
plotcluster(training, kmeans$cluster)
table(kmeans_2$cluster,training[,'STATUS'])


tests <- data.matrix(test)
#df<-test[STATUS]
#----------predict on test set
kmeans_predict<-cl_predict( kmeans_2, tests[,features], type='class' )
x<-table(kmeans=kmeans_predict,actual=tests[,'STATUS'])
acc<- function(x){
  sum(diag(x))/sum(rowSums(x))*100
}
acc(x)
#ACCURACY 48%
