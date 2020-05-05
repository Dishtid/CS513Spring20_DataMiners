# # # # # # # # # # # # # # # # # #
#  Algorithm : k-means clustering #
# # # # # # # # # # # # # # # # # #

# Clearing object environment
rm(list=ls())

dev.off()
cat("\014") #ctrl+l

# Load required package
install.packages()
library()

# Load Wisconsin Breast cancer data file CSV
csvInput <- file.choose()
data <- read.csv(csvInput)
View(data)

#Omit nulls
data <- na.omit(data)
View(data)

#splitting every 5th data as test and remaining as training
idx<-seq(1, nrow(data), by=5)
training<-data[-idx,-1]
test<-data[idx,-1]

#remove first column
data <- data[,-1] 
View(data)