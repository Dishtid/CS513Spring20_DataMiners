# # # # # # # # # # # #
# Data Visualization  #
# # # # # # # # # # # #

# remove all object
remove(list=ls())
dev.off();

# Called the required library 
library(dplyr)
library(ggplot2)

# read the CSV file 
file <- file.choose()
df <- read.csv(file)
View(df)

# summary 
summary(df)

# Split data
df_new <- df[, c(2,3,8,9,21)]
View(df_new)

active <- filter(df_new, STATUS=='A')
nrow(active)
terminated <- filter(df_new, STATUS == 'T')
nrow(terminated)

# Bar plot for Active & Terminated Vs AGE
#g <- ggplot(active, aes(AGE))
#g + geom_bar()

barplot(xtabs(~active$AGE),  
        ylab = "Number of employees", ylim = c(0,180), 
        xlab = "Employees' AGE" , 
        main = "Active employees' AGE",
        col = c("#69b3a2"), border = "white")

barplot(xtabs(~terminated$AGE),  
        ylab = "Number of employees", ylim = c(0,180), 
        xlab = "Employees' AGE" , 
        main = "Terminated employees' AGE",
        col = c("#69b3a2"), border = "white")

# Bar plot for A & T Vs Hourly_Rate
g <- ggplot(active, aes(HRLY_RATE))
g + geom_bar()
g <- ggplot(terminated, aes(HRLY_RATE))
g + geom_bar()


barplot(xtabs(~active$HRLY_RATE),  
        ylab = "Number of employees", 
        xlab = "Employees' Rate" , 
        main = "Active employees' Hourly Rate",
        col = c("#69b3a2"), border = "white")


barplot(xtabs(~terminated$HRLY_RATE),  
        ylab = "Number of employees",  
        xlab = "Employees' Rate" , 
        main = "Terminated employees' Hourly Rate",
        col = c("#69b3a2"), border = "white")

# Bar plot for A & T Vs Job Satisfaction
g <- ggplot(active, aes(JOB_SATISFACTION))
g + geom_bar()

g <- ggplot(terminated, aes(JOB_SATISFACTION))
g + geom_bar()


barplot(xtabs(~active$JOB_SATISFACTION),  
        ylab = "Number of employees", 
        xlab = "Employees' Rate" , 
        main = "Active employees' Job Satisfaction",
        col = c("#69b3a2"), border = "white")


barplot(xtabs(~terminated$JOB_SATISFACTION),  
        ylab = "Number of employees",  
        xlab = "Employees' Rate" , 
        main = "Terminated employees' Job Satisfaction",
        col = c("#69b3a2"), border = "white")
