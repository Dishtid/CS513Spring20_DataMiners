# # # # # # # # # # # # # # #
#  Algorithm : h-clustering #
# # # # # # # # # # # # # # #
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

m_dist<-dist( training[,features])
hcresults<-hclust(m_dist)
library(dendextend)
dend <- as.dendrogram(hcresults)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) 

dend <- hang.dendrogram(dend,hang_height=0.1)
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
#plot(dend, 
 #    main = "Clustered Iris data set
  #   (the labels give the true flower species)", 
   #  horiz =  TRUE,  nodePar = list(cex = .007))
#legend("topleft", legend = iris_species, fill = rainbow_hcl(3))
par(mar = rep(0,4))
library(ggplot2)
# Rectangle dendrogram using ggplot2
ggd1 <- as.ggdend(dend)
ggplot(ggd1)

#library(circlize)
#circlize_dendrogram(dend)
hc2<-cutree(hcresults,3)
t<-table(hclust=hc2,actual=training[,'STATUS'])

acc<- function(x){
  sum(diag(x))/sum(rowSums(x))*100
  
}
acc(t)
#ACCURACY: 56.08% 

