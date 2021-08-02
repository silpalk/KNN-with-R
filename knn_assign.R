##for glass data
library(readr)
glass <-read.csv(file.choose())
# Exploratory Data Analysis
str(glass)
# table of diagnosis
table(glass$Type)

str(glass$Type)
summary(glass)
sum(is.na(glass))

# table or proportions with more informative labels
round(prop.table(table(glass$Type)) * 100, digits = 2)
# summarize any three numeric features
summary(glass[c("RI", "Na", "Si")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
normalize(c(10, 20, 30, 40, 50))

# normalize the glass data
glass_n <- as.data.frame(lapply(glass[1:9], normalize))


# confirm that normalization worked
summary(glass_n$RI)

# create training and test data
glass_train <- glass_n1[1:150, ]
glass_test <- glass_n1[150:214,]
# create labels for training and test data

glass_train_labels <- glass[1:150, 10]
glass_train_labels <- glass_train_labels["Type"]

glass_test_labels <- glass[150:214, 10]
glass_test_labels <- glass_test_labels["Type"]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

glass_test_pred <- knn(train = glass_train, test = glass_test,cl = glass_train_labels, k = 7)


## ---- Evaluating model performance ---- ##
confusion_test <- table(x = glass_test_labels, y = glass_test_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy 

# Training Accuracy to compare against test accuracy
glass_train_pred <- knn(train = glass_train, test = glass_train, cl = glass_train_labels, k=21)

confusion_train <- table(x = glass_train_labels, y = glass_train_pred)
confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

### for zoo data set
library(readr)
##loading of Zoo data set
zoo <-read.csv(file.choose())
str(zoo)
# table of diagnosis
table(zoo$type)
zoo <-zoo[-1]

str(glass$type)
summary(zoo)
sum(is.na(zoo))

# table or proportions with more informative labels
round(prop.table(table(zoo$type)) * 100, digits = 2)



# create training and test data
zoo_train <- zoo[1:76, ]
zoo_test <- zoo[76:101,]
# create labels for training and test data

zoo_train_labels <- zoo[1:76, 17]
zoo_train_labels <- zoo_train_labels["Type"]

zoo_test_labels <- zoo[76:101, 17]
zoo_test_labels <- zoo_test_labels["Type"]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

zoo_test_pred <- knn(train = zoo_train,test = zoo_test,cl, k = 3)


## ---- Evaluating model performance ---- ##
confusion_test <- table(x = zoo_test_labels, y = zoo_test_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy 

# Training Accuracy to compare against test accuracy
zoo_train_pred <- knn(train = zoo_train, test = zoo_train, cl = zoo_train_labels, k=3)

confusion_train <- table(x = zoo_train_labels, y = zoo_train_pred)
confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

