# # # # # # # # # # # # # # # # # # # # # # # # # #
#  Algorithm : Classification And Regression Tree #
# # # # # # # # # # # # # # # # # # # # # # # # # #

# remove all objects 
rm(list = ls())

# install packages
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer) 

# read file 
file <- file.choose()
df <- read.csv(file)
View(df)

# summary 
summary(df)

# Change nulls into '00'
df$TERMINATION_YEAR[is.na(df$TERMINATION_YEAR)]<-00

# Find the most appropraite feature for prediction the status of emplpoyss 
#library(corrplot)
#correlation <- cor(df[,c(2,3,4,8,9,14,17)])
#corrplot(correlation,method = "circle")

# Include all features that that is important from  correlation matrix
df<-df[,c(2,3,8,9,21)]

#splitting data into test and training
indx<-sample(nrow(df),0.70*nrow(df))
training<-df[indx,] #70% training
test<-df[-indx,] #30% testing

#CART
CART_class<-rpart(STATUS~.,data = training)


#CART PLOT
rpart.plot(CART_class)

# much fancier graph
fancyRpartPlot(CART_class, main = "Decision_Tree", palettes=c("Greys", "Oranges"))

# Predication for training 
CART_predict<-predict(CART_class, training, type="class") 
table(Actual=training[,5],CART=CART_predict)


# error rate for traning 
table(Actual=training[,5],CART=CART_predict)
CARTWrong<-sum(training[,5]!=CART_predict)
error_rate<-CARTWrong/length(training$STATUS)
error_rate

# accuracy 
accuracy <- 1-error_rate
accuracy

# Predication for test 
CART_predict1<-predict(CART_class, test, type="class") 
table(Actual=test[,5],CART=CART_predict1)


# error rate 
table(Actual=test[,5],CART=CART_predict1)
CARTWrong1<-sum(test[,5]!=CART_predict1)
error_rate1<-CARTWrong1/length(test$STATUS)
error_rate1

# accuracy for testing 

accuracy <- 1- error_rate1
accuracy

