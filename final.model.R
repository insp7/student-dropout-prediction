#This model is the final version.

#load packages --------

library(e1071)
library(caret)
library(here)

#read in data --------

fullData <- read.csv(here("data", "AcademicSuccess.csv"))

#filter Enrolled out --------

filterFullData <- fullData %>%
  filter(Target != "Enrolled")

#remove features --------

filterFullData$Educational.special.needs <- NULL
filterFullData$International <- NULL
filterFullData$Nacionality <- NULL
filterFullData$Curricular.units.2nd.sem..credited.<- NULL

#split the data --------

set.seed(123)

trainIndex <- createDataPartition(filterFullData$Target, p = 0.8, list = FALSE)

trainData <- filterFullData[trainIndex, ]
testData <- filterFullData[-trainIndex, ]

#convert Target to factor for SVM model --------

trainData$Target <- as.factor(trainData$Target)
testData$Target <- as.factor(testData$Target)

#SVM model --------

svm_model <- svm(Target ~ ., data = trainData, kernel = "linear", 
                 type = "C-classification", cost = 0.4)

#evaluate the model with testData--------

predictions <- predict(svm_model, testData)

confMat <- table(testData$Target, predictions)
print(confMat)

TP <- confMat[1, 1]
TN <- confMat[2, 2]
FP <- confMat[1, 2]
FN <- confMat[2, 1]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))