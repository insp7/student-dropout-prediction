#This model predicts enrolled students' performance.

#load packages --------

library(e1071)
library(caret)
library(here)

#read in data --------

fullData <- read.csv(here("data", "AcademicSuccess.csv"))

#filter Enrolled out --------

filterFullData <- fullData %>%
  filter(Target != "Enrolled")

#filter Dropout and Graduate out --------

EnrolledData <- fullData %>%
  filter(!(Target %in% c("Dropout", "Graduate")))

#remove features --------

filterFullData <- filterFullData %>%
  select(
    -Educational.special.needs,
    -International,
    -Nacionality,
    -Curricular.units.2nd.sem..credited.
  )

EnrolledData <- EnrolledData %>%
  select(
    -Educational.special.needs,
    -International,
    -Nacionality,
    -Curricular.units.2nd.sem..credited.
  )

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

#prediction for Enrolled students --------

predictions <- predict(svm_model, EnrolledData)

resultData <- EnrolledData
resultData$Prediction <- predictions

#summarize resultData --------

table(resultData$Prediction)

