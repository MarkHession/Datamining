
#The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution. 
#The classification goal is to predict if the client will subscribe a term deposit (variable y).


bank <- read.csv("data/bank/bank.csv")
# breaks plot(model)
str(bank)

summary(bank)

bank$month= NULL
bank$job = NULL
bank$contact = NULL
bank$poutcome = NULL
bank$pdays = NULL

set.seed(1)
bank_rand <- bank[order(runif(4521)), ]

# split the data frames
bank_train <- bank_rand[1:3617, ]
bank_test <- bank_rand[3618:4521, ]

# check the proportion of class variable
prop.table(table(bank_train$y))
prop.table(table(bank_test$y))

#train and predict
library(C50)
model <- C5.0(y ~ ., data = bank_train)
# display simple facts about the tree
model
# display detailed information about the tree
summary(model)
# Evaluating model performance
# Create a factor vector of predictions on test data
predictions <- predict(model, bank_test, type= 'class')


# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(predictions, bank_test$y,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted y', 'actual y'))


#tree
plot(model, compress=TRUE)

library(rpart)
library(rpart.plot)
fit <- rpart(y~., data = bank_train, method = 'class')
rpart.plot(fit, extra = 106)

#prediction
predict_unseen <-predict(fit, bank_test, type = 'class')
table_mat <- table(bank_test$y, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
