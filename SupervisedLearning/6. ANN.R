# # # # # # # # # # # # # # # # #
#  Algorithm : Neural Networks  #
# # # # # # # # # # # # # # # # #

# Clearing object environment
rm(list=ls())
dev.off()

# For debugging
#NA_preproc

# Load data file CSV
csvInput <- file.choose()
dataset <- read.csv(csvInput)

View(dataset)
str(dataset)
head(dataset) 

any(is.na(dataset))
data <- dataset

# Install required packages
#install.packages("caTools")
library(dplyr)
library(caret)
library(neuralnet)

# Choose columns
data<-select(data, ANNUAL_RATE, HRLY_RATE, JOB_SATISFACTION, AGE, STATUS)

# factorize the status as A=1 and T=0
data$STATUS <- factor(data$STATUS, labels = c(1,0))
#data$STATUS <- as.numeric(data$STATUS)
View(data)
str(data)

# Normalize
data_norm <- select(data, ANNUAL_RATE, HRLY_RATE, JOB_SATISFACTION, AGE)

data_norm<-data.frame(lapply(data_norm,as.numeric))
str(data_norm)

maxs<-apply(data, 2, max)
maxs

mins<-apply(data, 2, min)
mins

scaled.data<-as.data.frame(apply(data_norm[,1:ncol(data_norm)],2,function(x) (x - min(x))/(max(x)-min(x))))
str(scaled.data)
head(scaled.data)

#combine data
final<-data.frame(data, scaled.data)
View(final)

final<-select(final, ANNUAL_RATE.1, HRLY_RATE.1, JOB_SATISFACTION.1, AGE.1, STATUS)
View(final)

#Categorize data
index<- seq (1,nrow(data),by=5)
train<- data[-index,]
test<- data[index,]

cols <- colnames(data)
cols

# Train the data
nn <- neuralnet(STATUS ~ ., data=train, hidden=4, exclude = NULL, threshold=0.04)
plot(nn)

dev.off();

nn1 <- neuralnet(STATUS ~ ., data=test, hidden=4, exclude = NULL, threshold=0.04)
plot(nn1)

# Predict
pred <- predict(nn, test)
View(pred)

# Fatorize the prediction
pred_cat <- ifelse(pred<0.5,0,1)
View(pred_cat)
View(test$STATUS)

# Error rate for test data
wrong<- (test$STATUS!=pred_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Accuracy for test data
accuracy <- 1-error_rate
accuracy