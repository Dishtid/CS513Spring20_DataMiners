# # # # # # # # # # # # # # # # # # # 
#  Algorithm : Nearest Neighbor/kNN #
# # # # # # # # # # # # # # # # # # #

#  purpose         : 1-Applying KNN alogrithm to Predcit 'STATUS'.
#                  : 2- Apply data visulazation for the STATUS vlaues from the originial dataset and STATUS values from KNN predicition
#                  :  3- caculate the accuracey 

# remove all object
remove(list=ls())

# Called the required library 
library(kknn)
library(class)
 

# read the CSV file 
file <- file.choose()
df <- read.csv(file)
View(df)

# summary 
summary(df)

# replace the null values in termination year by '00
df$TERMINATION_YEAR[is.na(df$TERMINATION_YEAR)]<-00

# Find the most appropraite feature for prediction the status of emplpoyss 
library(corrplot)
correlation <- cor(df[,c(2,3,4,8,9,14,17)])
corrplot(correlation,method = "circle")

# we found that column 2,3,8,and9  which are ANNUAL_RATE, HRLY_RATE, JOB_STATISFACTION, and AGE  are the most correlated features that can help us in better prediction the status of employees 
# normalizing function 
normalize <- function(x) {
        return ((x - min(x)) / (max(x) - min(x))) }


# normalize the correlated features
df_n <- as.data.frame(lapply(df[,c(2,3,8,9)], normalize)) 

View(df[,c(2,3,8,9)])
idx<-sort(sample(nrow(df),as.integer(.70*nrow(df)))) # train 70% of the data 
#idx<-seq(1, nrow(df), by=10)
training <- df_n[idx, ] # train 70% of the data 
testing <- df_n[-idx, ] # test 30% of the data 

train_label <- df[idx, 21] # The target lable for  training dataset is 'STATUS' which is in column #21
test_label <- df[-idx, 21] # The target lable for  testing dataset is 'STATUS' which is in column #21
??knn

# Apply the 'knn'for the train_label which is 'STATUS'
STATUS_test_pred <- knn(train = training,test = testing, cl= train_label,k= 98) 

# Visualization 
# Purpose :Comparing the actual STATUS values from originial dataset to the STATUS values from KNN predicition

library(ggplot2)
install.packages('breakDown')
install.packages("dplyr")
install.packages("magrittr")
install.packages("tidyquant")
library(breakDown)
library(dplyr)
library(tidyquant)


# barplot for STATUS values from originial dataset 

barplot(xtabs(~df$STATUS),  ylab = "Number of employees", xlab = "Employees' Status" , main = "Employee STATUS resulting from Originial dataset"
        ,col = c("#69b3a2","#a6a6a6"), border = "white",
        legend.text = c("A:Active", "T:Terminated"),
        args.legend=list(cex=0.75,x="topright"))

table(df$STATUS) # to show the exact number of Active and terminated employees
# barplot for STATUS prediction from KNN prediction 

barplot(xtabs(~STATUS_test_pred),  ylab = "Number of employees", ylim = c(0,2500), xlab = "Employees' Status" , main ="Employee STATUS resulting from KNN",
        col = c("#69b3a2","#a6a6a6"), border = "white",
        legend.text = c("A:Active", "T:Terminated"),
        args.legend=list(cex=0.75,x="topright"))

table(STATUS_test_pred) # to show the exact number of Active and terminated employees.

#Thus,as both graph are shown Active employees is always higher than terminated once. 
#So, we can assume that our model  is right in predciting the employees status.


# Accuracy 

# Evalute the accuracey by using 'gmodels'library that has a 'CrossTable' function that helps finding out the accuracy.
# 'CrossTable' return cross tabulation of predicted and observed classifications.
# That helps in using the number of the cells for calculating true posititve (TP), true negative (TN), false negative (FN) and false positive (FP), and using these values to caculate  the 'accuracy'.

install.packages("gmodels")
require("gmodels")
library("gmodels")
table <- CrossTable(x = test_label, y = STATUS_test_pred,
                    prop.chisq = FALSE) 

# Caculate the accuracy by  posititve (TP), true negative (TN), false negative (FN) and false positive (FP) for each category or level 1 'A' or 2 'T'

tp <- table$t[1,1]

tn <- table$t[2,2]

fp <- table$t[1,2]

fn <- table$t[2,1]


accuracey <- (tp+tn)/(tp+tn+fn+fp)

accuracey # The result of Accuracey 

