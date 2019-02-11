##Loading packages
library(tidyverse)
install.packages("reshape")
library(reshape)
install.packages("arules", dependencies=TRUE)
library(arules)
install.packages("arulesViz")
library(arulesViz)

##Loading the data
setwd("/Users/MortenAndersen/Desktop/Master Thesis/Bodylab Data/Test Data")
test_data <- read.csv("orderlines-4.csv",row.names = NULL,sep = ";")

##Wrangling the data
names <- c("OrderID","ProductID","ProductName","Variant","Quantity","UnitPrice")
colnames(test_data) <- names

require(reshape2)
test_data1 <- dcast(test_data, OrderID ~ ProductID, value.var="Quantity",fill=0)
test_data1$OrderID <- as.character(test_data1$OrderID)
test_data1[2:ncol(test_data1)] <- ifelse(test_data1>0,"Yes","No")
test_data1 <- test_data1[3:ncol(test_data1)]

##Summary - Remove grams + ambassador from the data
summary(test_data1)
transaction_rules <- apriori(test_data1,parameter=list(sup=0.01,conf=0.05,target="rules"))
summary(myrules)
