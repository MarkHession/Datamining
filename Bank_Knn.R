bank <- read.csv("data/bank/bank.csv")

bank$job=NULL
bank$marital=NULL
bank$education=NULL
bank$housing=NULL
bank$loan=NULL
bank$contact=NULL
bank$month=NULL
bank$poutcome=NULL
bank$default=NULL

bank=bank[,c(8,1,2,3,4,5,6,7)]

# table of success/failure
table(bank$y)


# recode y as a factor
bank$y <- factor(bank$y,
                         levels = c("yes", "no"),
                         labels = c("Success", "Fail"))

# table or proportions with more informative labels
prop.table(table(bank$y))


# summarize three numeric features
summary(bank[c("age", "balance",
               "duration")])


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))


# note doesn’t include the labels
bank_n <- as.data.frame(lapply(bank[2:8], normalize))
summary(bank_n$age)
hist(bank_n$age)


# create training and test data (no labels)
bank_train <- bank_n[1:4421, ]
bank_test <- bank_n[4422:4521, ]


# create labels for training and test data
bank_train_labels <- bank[1:4421, 1]
bank_test_labels <- bank[4422:4521, 1]


## Step 3: Training a model on the data ----
library(class)
predictions <- knn(train = bank_train, test =
                     bank_test, cl = bank_train_labels, k=21)

## Step 4: Evaluating model performance ----
# load the "gmodels" library
#install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(predictions, bank_test_labels,
           prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE)

##################
# try several different values of k


# k=1
predictions <- knn(train = bank_train, test = bank_test, 
                   cl = bank_train_labels, k=1)
CrossTable(predictions, bank_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=5
predictions <- knn(train = bank_train, test = bank_test, 
                   cl = bank_train_labels, k=5)
CrossTable(predictions, bank_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=11
predictions <- knn(train = bank_train, test = bank_test, 
                   cl = bank_train_labels, k=11)
CrossTable(predictions, bank_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=15
predictions <- knn(train = bank_train, test = bank_test, 
                   cl = bank_train_labels, k=15)
CrossTable(predictions, bank_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=21
predictions <- knn(train = bank_train, test = bank_test, 
                   cl = bank_train_labels, k=21)
CrossTable(predictions, bank_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=27
predictions <- knn(train = bank_train, test = bank_test, 
                   cl = bank_train_labels, k=27)
CrossTable(predictions, bank_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
