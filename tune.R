#This script is used to tune the SVM Model.

#load packages --------

library(e1071)
library(here)

#read in data --------

fullData <- read.csv(here("data", "AcademicSuccess.csv"))

#filter Enrolled out --------

filterFullData <- fullData %>%
  filter(Target != "Enrolled")

#remove features --------

filterFullData$Nacionality <- NULL
filterFullData$Educational.special.needs <- NULL
filterFullData$International <- NULL
filterFullData$Curricular.units.2nd.sem..credited.<- NULL

#split the data --------

set.seed(123)

trainIndex <- createDataPartition(filterFullData$Target, p = 0.8, list = FALSE)

trainData <- filterFullData[trainIndex, ]
testData <- filterFullData[-trainIndex, ]

#convert Target to factor for SVM model --------
trainData$Target <- as.factor(trainData$Target)
testData$Target <- as.factor(testData$Target)

#tune the SVM Model --------

set.seed (1)
tune.model <- tune(svm, Target ~ ., data = trainData, kernel = "linear",
                   range = list(cost = seq(0.1, 2, by = 0.1)))

#summary and plot --------
summary(tune.model)

plot(tune.model, main = "SVM Hyperparameter Tuning Results", xlab = "Cost", ylab = "Cross-Validation Error")
abline(v = tune.model$best.parameters$cost, col = "red", lwd = 1)
grid()
