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
output <- data.frame(number_of_trees = 0, rfError = 0)
row <- 1
# a loop to get the result for difrent number of trees 
for(i in ntrees){
  RF <- randomForest(formula=Spam ~.,data=train,ntree= i)
  yhat <- predict(RF,newdata=test)
  # confusion matrix
  cm <- table(test$Spam, yhat)
  rfError <- (cm[2,1]+cm[1,2])/sum(cm)
  output[row,] <- c(i,rfError)
  row <- row + 1
}
# PLOTing the results 
ggplot() +
  geom_line(data = output, mapping = aes(x = number_of_trees
                                         , y = rfError)
            , size = 1.5, color = 'forestgreen' ) +
  theme_light() + 
  scale_x_discrete(limits = seq(from = 10, to = 100, by = 10)) +
  ggtitle( 'Misclassification rate by random forest algorithim with
           different number of trees' ) +
  theme(plot.title = element_text(hjust = 0.5))

# Adaboost 
row <- 1
output2 <- data.frame(number_of_iterations = 0, Error = 0)

for (i in ntrees){
  ada <- blackboost(formula = Spam~., data = train,family =Binomial(type = c("adaboost")), control = boost_control(mstop = i))
                    Y_hat <- predict(object = ada
                                     , newdata = test
                                     , type = 'class')
                    cm <- table(test$Spam, Y_hat)
                    Error <- (cm[1,2]+cm[2,1])/sum(cm)
                    output2[row,] <- c(i,Error)
                    row <- row + 1
}
#----- Plot





Finaldf <- data.frame(number_of_iterations=seq(10,100,by=10),Error=output2$Error,rfError=output$rfError)
ggplot(data = Finaldf) +
  geom_line(mapping = aes(x = number_of_iterations
                                          , y = Error)
            , size = 1.5, color = 'deepskyblue1' ) +
 geom_line(mapping = aes(x = number_of_iterations
                                         , y = rfError)
            , size = 1.5, color = 'forestgreen' ) 
theme_light()+ 




