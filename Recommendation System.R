##Loading packages
library(tidyverse)
install.packages("reshape")
library(reshape)
##Loading the data
setwd("/Users/MortenAndersen/Desktop/Master Thesis/Bodylab Data/Test Data")
test_data <- read.csv("orderlines-4.csv",row.names = NULL,sep = ";")

##Wrangling
names <- c("OrderID","ProductID","ProductName","Variant","Quantity","UnitPrice")
colnames(test_data) <- names

require(reshape2)
test_data1 <- dcast(test_data, OrderID ~ ProductID, value.var="Quantity",fill=0)

