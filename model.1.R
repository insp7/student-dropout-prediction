#This model is used to compare accuracy after removing different features.

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
#filterFullData$Daytime.evening.attendance <- NULL
#filterFullData$Marital.status <- NULL

filterFullData$Curricular.units.2nd.sem..credited.<- NULL
#filterFullData$Curricular.units.1st.sem..credited.<- NULL
#filterFullData$Curricular.units.2nd.sem..enrolled.<- NULL
#filterFullData$Curricular.units.1st.sem..enrolled.<- NULL
#filterFullData$Curricular.units.2nd.sem..approved.<- NULL
#filterFullData$Curricular.units.1st.sem..approved.<- NULL

#split the data --------

set.seed(123)

trainIndex <- createDataPartition(filterFullData$Target, p = 0.8, list = FALSE)

trainData <- filterFullData[trainIndex, ]
testData <- filterFullData[-trainIndex, ]

#convert Target to factor for SVM model --------

trainData$Target <- as.factor(trainData$Target)
testData$Target <- as.factor(testData$Target)

#SVM model --------

svm_model <- svm(Target ~ ., data = trainData, kernel = "linear", type = "C-classification")

#evaluate the model with testData--------

predictions <- predict(svm_model, testData)

confusion_matrix <- table(testData$Target, predictions)
print(confusion_matrix)

correct_predictions <- sum(diag(confusion_matrix))

total_observations <- sum(confusion_matrix)

accuracy <- correct_predictions / total_observations
print(accuracy)
