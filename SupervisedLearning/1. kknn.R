# # # # # # # # # # # # # # # # #
#  Algorithm : Nearest Neighbor #
# # # # # # # # # # # # # # # # #

# remove all object
remove(list=ls())

# Called the required library 
library(kknn)
library(class)
library(caret)

dev.off()

# read the CSV file 
file <- file.choose()
df <- read.csv(file)
View(df)

# summary 
summary(df)

# replace the null values in termination year by '00
df$TERMINATION_YEAR[is.na(df$TERMINATION_YEAR)]<-00

# Split data
idx<-sort(sample(nrow(df),as.integer(.70*nrow(df)))) # train 70% of the data 
#idx<-seq(1, nrow(df), by=10)
training <- df[idx, ] # train 70% of the data 
testing <- df[-idx, ] # test 30% of the data

# The target lable for  testing dataset is 'STATUS' which is in column #21
target <- df[idx, 21]

# Train & Predict for testing - unweighted
STATUS_test_pred_k <- kknn(formula=target~., 
                           training, testing, 
                           k=98, kernel ="rectangular")
fit <- fitted(STATUS_test_pred_k)

predict_k <- table(testing$STATUS,fit)
View(predict_k)

# Find error rate
wrong <- sum(testing[,21]!=fit)
error_rate1 <- wrong/length(testing$STATUS)
error_rate1

# Accuracy for test
Accuracy1 <- 1-error_rate1
Accuracy1

# Train & Predict for testing - weighted
STATUS_test_pred_kw <- kknn(formula=target~., 
                            training, testing, 
                            k=98, kernel ="triangular")
fitw <- fitted(STATUS_test_pred_kw)

predict_kw <- table(testing$STATUS,fitw)
View(predict_kw)

# Find error rate
wrong <- sum(testing[,21]!=fitw)
error_rate2 <- wrong/length(testing$STATUS)
error_rate2

# Accuracy for test
Accuracy2 <- 1-error_rate2
Accuracy2
