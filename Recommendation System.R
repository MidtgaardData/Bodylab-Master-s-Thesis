##The following script is based on this guide: https://www.datacamp.com/community/tutorials/market-basket-analysis-r 
##For visualization: https://www.r-bloggers.com/implementing-apriori-algorithm-in-r/

##Packaging
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyr")
install.packages("reshape")
install.packages("reshape2")
install.packages("tidyverse")
install.packages("dplyr")

library(tidyverse)
library(reshape)
library(reshape2)
library(plyr)
library(tidyr)
library(arulesViz)
library(arules)
library(dplyr)

##Loading the dataset
setwd("C:/Users/morte/OneDrive/Skrivebord")
raw_data <- read.table("analyse_orderlines_full (5).txt",sep=",")

##Loading lookupliste
lookup_data <- read.table("Lookupliste.csv",sep=",",header=TRUE)
lookup_names <- c("V2","V3")
colnames(lookup_data) <- lookup_names
raw_data <- left_join(raw_data,lookup_data,by="V2")

##Wrangling the data
raw_data[,2:6] <- NULL
names <- c("OrderID","ProductID")
colnames(raw_data) <- names
rawt_data <- raw_data

raws_data <- subset(rawt_data, rawt_data$ProductID!="Not Grouped")
raw2_data <- subset(raws_data, raws_data$ProductID!="FreeBee")
raw3_data <- subset(raws_data, raw2_data$ProductID!="Freebee")

##Batching the orders
df_itemList <- ddply(raw3_data,"OrderID", 
                     function(raw3_data)paste(raw3_data$ProductID, 
                                              collapse = ","))
df_itemList$OrderID <- NULL
colnames(df_itemList) <- c("itemList")

write.csv(df_itemList,"ItemList.csv",quote=FALSE, row.names = FALSE)

##Performing association rule through algorithm
txn <- read.transactions(file="ItemList.csv", rm.duplicates = TRUE,format="basket",sep=",")

txn
summary(txn)

txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)

basket_rules <- apriori(txn,parameter = list(sup = 0.001, conf = 0.8,target="rules"))

##Detaching potential conflict and inspecting
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}

inspect(basket_rules)

df_basket <- as(basket_rules,"data.frame")
View(df_basket)

##Visualizing the rules
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)

##Summary
summary(test_data1)
write.csv(df_itemList,"ItemList.csv", row.names = TRUE)
transaction_rules <- apriori(test_data1,parameter=list(sup=0.01,conf=0.05,target="rules"))
summary(myrules)


##Theory for Arules - der er 2 artikler (https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/)

##Wrangling to fit algorithm (ALTERNATIVE POSSIBILITY. Output is transposed 0/1)
##require(reshape2)
##test_data1 <- dcast(rawt_data, OrderID ~ ProductID, value.var="Quantity",fill=0)
##test_data1$OrderID <- as.character(test_data1$OrderID)
##test_data1[2:ncol(test_data1)] <- ifelse(test_data1>0,"Yes","No")
