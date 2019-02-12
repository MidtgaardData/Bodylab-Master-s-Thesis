##Loading packages
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("GGally")
library(GGally)
install.packages("scatterplot3d")
library(scatterplot3d)


##Loading the dataset
Bodylab_Data <- read.csv()

##Data-wrangling

##Performing linear regression
fit1 <- lm(Sales~.,data=Bodylab_Data,na.action = na.omit)
summary(forecastLM)

##Visualization - variable relationships
pairs(~Sales)

##Regression & visualization: https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/