# data and libraries 

library(mboost)
library(ggplot2)
library(randomForest)
set.seed(1234)

spam <- read.csv2('spambase.csv')
spam$Spam <- as.factor(spam$Spam)
# Adaboost 

n <- dim(spam)[1]
id <- sample(1:n, floor(n*(2/3)))
train <- spam[id,]
test <- spam[-id,]

# diffrent number of trees 
ntrees <- seq(10,100,by=10)
output <- data.frame(number_of_trees = 0, Error = 0)
row <- 1
# a loop to get the result for difrent number of trees 
for(i in ntrees){
  RF <- randomForest(formula=Spam ~.,data=train,ntree= i)
  yhat <- predict(RF,newdata=test)
  # confusion matrix
  cm <- table(test$Spam, yhat)
  Error <- (cm[2,1]+cm[1,2])/sum(cm)
  output[row,] <- c(i,Error)
  row <- row + 1
}
# PLOTing the results 
ggplot() +
  geom_line(data = output, mapping = aes(x = number_of_trees
                                         , y = Error)
            , size = 1.5, color = 'forestgreen' ) +
  theme_light() + 
  scale_x_discrete(limits = seq(from = 10, to = 100, by = 10)) +
  ggtitle( 'Misclassification rate by random forest algorithim with
           different number of trees' ) +
  theme(plot.title = element_text(hjust = 0.5))
