# # # # # # # # # # # # # # # # # #
#  Algorithm : Linear Regression  #
# # # # # # # # # # # # # # # # # #

# Clearing object environment
rm(list=ls())

dev.off()
cat("\014") #ctrl+l

# Load required package
library(reshape2)
library(ggplot2)

# Load data file CSV
csvInput <- file.choose()
data <- read.csv(csvInput)
View(data)

#clean data
any(is.na(data))

#check datatypes
str(data)

# Replace NA
data$TERMINATION_YEAR[is.na(data$TERMINATION_YEAR)] <- 0000
View(data)

#Check the type of data
str(data)

# factorize the status as A=1 and T=0
data$STATUS <- factor(data$STATUS, labels = c(1,0))
data$STATUS <- as.numeric(data$STATUS)
summary(data)
View(data)

# Choosing only correlated fields
mydatas <- data[, c(2,3,8,9,21)]
str(mydatas)
View(mydatas)

#clean data
any(is.na(mydatas))

#check datatypes
str(mydatas)

#linear regression
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(dplyr)

#grab only numeric columns
num_cols <- sapply(data, is.numeric)
num_cols

#filter using correlation
install.packages("corrgram")
install.packages("corrplot")
library(corrgram)
library(corrplot)

cor_data <- cor(data[,num_cols])
View(cor_data)

#only numeric data
print(corrplot(cor_data, method = 'color'))

#any type of data
corrgram(data, order=TRUE, lower.panel=panel.shade, upper.panel = panel.pie,
         text.panel = panel.txt)

#Plots
ggplot(data, aes(x=mydatas$STATUS)) + geom_histogram( alpha=0.4, fill='blue', stat = 'count')

#Split data
library(caTools)
set.seed(101)
sample <- sample.split(mydatas$STATUS, SplitRatio = 0.7)
train <- subset(mydatas, sample == TRUE)
View(train)
train$STATUS<-as.numeric(train$STATUS)
test <- subset(mydatas, sample == FALSE)
test$STATUS<-as.numeric(test$STATUS)
View(mydatas)

#linear regression
#model <- lm(y~x1+x2, data)
#model <- lm(y ~., data)
model <- lm(STATUS ~ ANNUAL_RATE+HRLY_RATE+JOB_SATISFACTION+AGE, data=train)
print(summary(model))

# Visualize
res <- residuals(model)

res <- as.data.frame(res)
head(res)

ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

# Predict
predict <- predict(model,test)

results <- cbind(predict,test$STATUS) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
View(results)

to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Mean Squared Error
results$pred <- sapply(results$pred,to_zero)
mse <- mean((results$real-results$pred)^2)
print(mse)

# Root mean squared error
mse^0.5

# Error rate
SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(mydatas$STATUS) - results$real)^2)

R2 = 1 - SSE/SST
R2

# Accuracy of the model
Accuracy= 1 - R2
Accuracy
