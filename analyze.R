#The script did correlation analysis and RFE

#load packages --------
library(tidyverse)
library(caret)
library(here)
library(randomForest)

#read in data --------

fullData <- read.csv(here("data", "AcademicSuccess.csv"))

#filter Enrolled out --------

filterFullData <- fullData %>%
  filter(Target != "Enrolled")

#correlation analysis --------

cor_matrix <- cor(filterFullData[, sapply(filterFullData, is.numeric)])
high_cor <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)

high_cor_pairs <- data.frame(
  Feature1 = rownames(cor_matrix)[high_cor[, 1]],
  Feature2 = colnames(cor_matrix)[high_cor[, 2]],
  Correlation = cor_matrix[high_cor]
)

high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs, 1, sort))), ]

#Recursive Feature Elimination (RFE) with Random Forest --------

filterFullData$Target <- as.factor(filterFullData$Target)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
predictors <- filterFullData[, names(filterFullData) != "Target"]
outcome <- filterFullData$Target

set.seed(123) 
rfe_results <- rfe(predictors, outcome, sizes = c(1:10), rfeControl = control, method = "rf")
print(rfe_results)

model <- randomForest(Target ~ ., data = filterFullData)
importanceScores <- varImp(model)


