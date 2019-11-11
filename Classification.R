#install.packages("ggplot2")
#install.packages("C50")
library('ggplot2')
library(C50)
library(gmodels)
library(class)

csv = read.csv("data/tic-tac-toe.data")
head(csv)
tail(csv)
shuffle_index <- sample(1:nrow(csv))
head(shuffle_index)
csv= csv[shuffle_index,]
head(csv)

prop.table((table(csv_train$positive)))

#install.packages("rpart.plot")	
library(rpart)
library(rpart.plot)
fit <- rpart(positive~., data = csv_train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, csv_test, type = 'class')
table_mat <- table(csv_test$positive, predict_unseen)
table_mat

#--------------------------

set.seed(1)
csv_rand =  csv[order(runif(957)),]
csv_train  =csv_rand[1:857,]
csv_test = csv_rand[858:975,]


#Train
csv_model = C5.0(positive ~ ., data = csv_train)

#Evaluation
csv_predictions = predict(csv_model, csv_test)

CrossTable(csv_predictions, csv_test$positive,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))



#decision tree
shuffle_index <- sample(1:nrow(csv))
head(shuffle_index)

