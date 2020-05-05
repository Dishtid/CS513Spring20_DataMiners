# # # # # # # # # # # # # # # # #
#  Algorithm : Neural Networks  #
# # # # # # # # # # # # # # # # #

# Clearing object environment
rm(list=ls())

#for debugging
NA_preproc

# Load Wisconsin Breast cancer data file CSV
csvInput <- file.choose()
dataset <- read.csv(csvInput)
View(dataset)

str(dataset)
head(dataset) 

any(is.na(dataset))
data <-dataset

data<-na.omit(data)
any(is.na(data))
View(data)

library(dplyr)

data_norm<-select(data, ANNUAL_RATE, HRLY_RATE, JOB_SATISFACTION, AGE, PREVYR_1, PREVYR_2, PREVYR_3, PREVYR_4, PREVYR_5)
View(data_norm)

maxs<-apply(data, 2, max)
maxs

mins<-apply(data, 2, min)
mins

#normalize
#scaled.data<-scale(data, center=mins, scale=maxs-mins)
scaled.data<-as.data.frame(apply(data_norm[,1:ncol(data_norm)],2,function(x) (x - min(x))/(max(x)-min(x))))
head(scaled.data)

#combine data
final<-data.frame(data, scaled.data)
View(final)

final<-select(final, EMP_ID,ANNUAL_RATE.1, HRLY_RATE.1,JOBCODE,ETHNICITY,SEX,MARITAL_STATUS, JOB_SATISFACTION.1, AGE.1, NUMBER_OF_TEAM_CHANGED,REFERRAL_SOURCE,HIRE_MONTH,REHIRE,TERMINATION_YEAR,IS_FIRST_JOB,TRAVELLED_REQUIRED, PERFORMANCE_RATING, DISABLED_EMP, DISABLED_VET, EDUCATION_LEVEL, STATUS,JOB_GROUP,PREVYR_1.1, PREVYR_2.1, PREVYR_3.1, PREVYR_4.1, PREVYR_5.1)
View(final)

#Categorize data
install.packages("caTools")
library(caTools)

index<-sample.split(final$STATUS, SplitRatio = 0.7)
#train<-subset(final, split==T)
#test<-subset(final, split==F)

#index <- seq (1,nrow(final),by=5)
test<- final[index,]
train<-final[-index,]

library(neuralnet)
cols<-names(train)
cols

f<-as.formula(paste("STATUS~",paste(cols[!cols %in% "STATUS"], collapse = "+")))
f

#choose only valid columns with unique values
validcols <- sapply(final, function(f)length(unique(final[!is.na(final)])) > 1)
View(final)

final <- final[,validcols]
str(final)

final2<-final
final2<-final[,-(5:7), drop=FALSE]
final3<-final2[,-(8:10), drop=FALSE]
final4<-final3[,-(9:10), drop=FALSE]
final5<-final4[,-(10:14), drop=FALSE]
final6<-final5[,-(7), drop=FALSE]
str(final6)


m <- model.matrix( 
  ~ EMP_ID + ANNUAL_RATE.1 + HRLY_RATE.1 + JOBCODE + JOB_SATISFACTION.1 + AGE.1 + TERMINATION_YEAR + PERFORMANCE_RATING + PREVYR_1.1 + PREVYR_2.1 + PREVYR_3.1 + PREVYR_4.1 + PREVYR_5.1, data = final)
m<-m[,-1 ,drop=FALSE]
head(m)


nn<-neuralnet(final6, data=m, hidden=15, exclude = NULL, threshold=0.04, stepmax=1e7)
plot(nn)

pred <-predict(model , test)
print(pred)
