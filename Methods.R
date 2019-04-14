# data and libraries 

library(mboost)
library(ggplot2)
library(randomForest)
set.seed(12345)

spam <- read.csv2('spambase.csv')
spam$Spam <- as.factor(spam$Spam)
# Adaboost 

