## This is a markdown file

## Read the Data

pml_train <- read.csv("C:/Users/chbasu/Downloads/pml-training.csv")
pml_test <- read.csv("C:/Users/chbasu/Downloads/pml-testing.csv")

## Exploratory Analysis

summary(pml_train)
summary(pml_test)
dim(pml_train)
dim(pml_test)

## There are 160 variables, I will have to reduce this count as there may not be so many factors for prediction and also the model will run out of memory. After looking at the data from above commands it is very evident that there are many missing values and various  variables with 0 variance.Removing the blank values and reducing the variables count which are of no use.

pml_train <- pml_train[, colSums(is.na(pml_train)) == 0]
pml_test <- pml_test[, colSums(is.na(pml_test)) == 0]

## Removing columns with timestamp, window etc.

pml_trainR <- grepl("^X|timestamp|window", names(pml_train)) 

## Getting classe variable for temp
classe<- pml_train$classe
pml_train <- pml_train[, !pml_trainR]

## Removing non numeric columns
pml_trainM <- pml_train[, sapply(pml_train, is.numeric)]

## Attach classe column back so we can train the model.
pml_trainM$classe <- classe

## Training the model
library(caret)

RF <- train(classe ~ ., data=pml_trainM, method="rf")

## Model is fit, we can predict now with test after cleaning the test data.

pml_testR <- grepl("^X|timestamp|window", names(pml_test))
pml_test <- pml_test[, !pml_testR]
pml_testM <- pml_test[, sapply(pml_test, is.numeric)]

## Lets start our prediction
predict_RF <- predict(RF, pml_testM)

