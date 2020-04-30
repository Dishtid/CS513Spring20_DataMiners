##  Step 0: Clear the environment
rm(list=ls())

##  Step 1: Load the relavent packages
#installed.packages()

# CART standard package
#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle

filename<-file.choose()
dsn<-  read.csv(filename)

View(dsn) 
attach(dsn)

set.seed(111)

index<-sort(sample(nrow(dsn),round(.25*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

#Grow the tree 
dev.off()
CART_class<-rpart(STATUS~.,data=training)

CART_predict<-predict(CART_class,test)
str(CART_predict)

CART_predict_cat<-ifelse(CART_predict[,1]<=.5,'Yes','No')
table(Actual=test[,4],CART=CART_predict_cat)
CART_wrong<-sum(test[,4]!=CART_predict_cat)
CART_error_rate<-CART_wrong/length(test[,4])
CART_error_rate

CART_predict2<-predict(CART_class,test, type="class")
table(Actual=test[,4],CART=CART_predict2)
CART_wrong2<-sum(test[,4]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test[,4])
CART_error_rate2

#important features
library(rpart)
library(rpart.plot) 
library(rattle)           
library(RColorBrewer) 

rpart.plot(CART_class)
prp(CART_class)

# much fancier graph
fancyRpartPlot(CART_class, main = "Decision_Tree", palettes=c("Greys", "Oranges"))

detach(dsn)
