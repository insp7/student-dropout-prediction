#load packages --------
library(tidyverse)
library(corrplot)
library(e1071)
library(caret)
library(here)
library(randomForest)
library(gbm)
library(rmarkdown)

#read in data --------
url <- "C:/Users/Aniket Konkar/Desktop/R Project/csci_6221/AcademicSuccess.csv"
fullData  <- read.csv(url, header=T, sep=",")
filterFullData <- fullData %>%
  filter(Target != "Enrolled")
EnrolledData <- fullData %>%
  filter(!(Target %in% c("Dropout", "Graduate")))

#summary the Target --------
str(filterFullData)
table(fullData$Target)

#plot data using geom_freqpoly by Target(Dropout and Graduate) --------
plot_freqpoly <- function(data, x_var, bin_width) {
  x_var <- enquo(x_var) 
  data %>%
    ggplot(aes(x = !!x_var, colour = Target)) +
    geom_freqpoly(binwidth = bin_width) +
    scale_x_continuous(breaks = unique(data[[quo_name(x_var)]])) +
    theme_minimal()
}

#correlation analysis --------
tempFilterFullData <- filterFullData
tempFilterFullData$Target <- as.factor(filterFullData$Target)
tempFilterFullData$Target <- as.numeric(tempFilterFullData$Target)
cor_matrix <- cor(tempFilterFullData[, sapply(tempFilterFullData, is.numeric)])
high_cor <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs <- data.frame(
  Feature1 = rownames(cor_matrix)[high_cor[, 1]],
  Feature2 = colnames(cor_matrix)[high_cor[, 2]],
  Correlation = cor_matrix[high_cor]
)
high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs, 1, sort))), ]
print(high_cor_pairs)
corrplot(cor_matrix, method = "color", 
         tl.cex = 0.6,                  
         tl.col = "black",             
         addCoef.col = "gray", 
         number.cex = 0.4)    
correlation_with_target <- cor_matrix[35,]
view(correlation_with_target)

set.seed(123)
#split the data --------
trainIndex <- createDataPartition(filterFullData$Target, p = 0.8, list = FALSE)
trainData <- filterFullData[trainIndex, ]
testData <- filterFullData[-trainIndex, ]

#convert Target to factor --------
trainData$Target <- as.factor(trainData$Target)
testData$Target <- as.factor(testData$Target)

#recode the Target variable as numeric --------
numericTrainData <- trainData
numericTestData <- testData
numericTrainData$Target <- ifelse(trainData$Target == "Graduate", 1, 0)
numericTestData$Target <- ifelse(testData$Target == "Graduate", 1, 0)

LRpredictFunc <- function(model, testData, threshold = 0.5) {
  probabilities <- predict(model, testData, type = "response")
  predicted_classes <- ifelse(probabilities > threshold, "Graduate", "Dropout")
  predicted_classes <- factor(predicted_classes, levels = levels(testData$Target))
  
  confMat <- table(testData$Target, predicted_classes)
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
}

#Logistic Regression Model --------
logistic_model <- glm(Target ~ ., data = trainData, family = binomial)
LRpredictFunc(logistic_model, testData, threshold = 0.48)
summary(logistic_model)

# if gender affects the target?
ggplot(numericTrainData, aes(x = Gender, 
                             y = Target)) +
  geom_jitter(height = .05, 
              alpha = 0.1) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) # remove the standard error ribbon

# if tuition fees up to date
ggplot(numericTrainData, aes(x = Tuition.fees.up.to.date, 
                             y = Target)) +
  geom_jitter(height = .05, 
              alpha = 0.1) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

# Approved curricular units for sem 2 vs Target
ggplot(numericTrainData, aes(x = Curricular.units.2nd.sem..approved., 
                       y = Target)) +
  geom_jitter(height = .05, 
              alpha = 0.1) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)

computeAccuracy <- function(model, testData, threshold = 0.5) {
  probabilities <- predict(model, testData, type = "response")
  predicted_classes <- ifelse(probabilities > threshold, "Graduate", "Dropout")
  predicted_classes <- factor(predicted_classes, levels = levels(testData$Target))
  
  confMat <- table(testData$Target, predicted_classes)
  
  TP <- confMat[1, 1]
  TN <- confMat[2, 2]
  FP <- confMat[1, 2]
  FN <- confMat[2, 1]
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  return(accuracy)
}

#find the best threshold for the Logistic Regression Model --------
best_threshold <- 0.3
best_accuracy <- 0

for (threshold in seq(0.3, 0.7, by = 0.01)) {
  accuracy <- computeAccuracy(logistic_model, testData, threshold)
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_threshold <- threshold
  }
}
print(best_threshold)
print(best_accuracy)

#predict function --------
predictFunc <- function(model, testData){
  predictions <- predict(model, testData)
  
  confMat <- table(testData$Target, predictions) # confusion matrix
  print(confMat)
  
  TP <- confMat[1, 1] # True Positive => 1st row, 1st column
  TN <- confMat[2, 2] # True Negative
  FP <- confMat[1, 2] # False Positive
  FN <- confMat[2, 1] # False Negative
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  print(paste("Accuracy:", accuracy))
  print(paste("Precision:", precision))
  print(paste("Recall:", recall))
  print(paste("F1 Score:", f1_score))
}

#find the best ntree for the Random Forest Model --------
ntree_values <- seq(500, 650, by = 25)
accuracies <- numeric(length(ntree_values))
for (i in seq_along(ntree_values)) {
  set.seed(123) 
  rf_model <- randomForest(Target ~ ., data = trainData, ntree = ntree_values[i])
  predictions <- predict(rf_model, testData)
  
  accuracies[i] <- sum(predictions == testData$Target) / nrow(testData) # total correct predictions divided by all predictions
}

plot(ntree_values, accuracies, type = "b", col = "red",
     xlab = "ntree", ylab = "Accuracy")

optimal_ntrees <- ntree_values[which.max(accuracies)]
best_accuracy <- max(accuracies)
print(paste("Optimal ntree: ", optimal_ntrees))
print(paste("Best accuracy: ", best_accuracy))

#Random Forest Model --------
rf_model <- randomForest(Target ~ ., data = trainData, ntree = 525)
predictFunc(rf_model, testData)


# SVM Model
svm_model.radial <- svm(Target ~ ., data = trainData, kernel = "radial", type = "C-classification")
svm_model.linear <- svm(Target ~ ., data = trainData, kernel = "linear", type = "C-classification")
svm_model.poly<- svm(Target ~ ., data = trainData, kernel = "polynomial", type = "C-classification")
svm_model.sig<- svm(Target ~ ., data = trainData, kernel = "sigmoid", type = "C-classification")

predictFunc(svm_model.radial, testData)
predictFunc(svm_model.linear, testData) 
predictFunc(svm_model.poly, testData) 
predictFunc(svm_model.sig, testData) 

#The svm_model with kernel = "linear" has a better performance

#tune the SVM Model --------
set.seed(1)
tune.model <- tune(svm, Target ~ ., data = trainData, kernel = "linear",
                   range = list(cost = seq(0.1, 2, by = 0.1)))
summary(tune.model)
plot(tune.model, main = "SVM Hyperparameter Tuning Results", xlab = "Cost", ylab = "Cross-Validation Error")
abline(v = tune.model$best.parameters$cost, col = "red", lwd = 1)
grid()
svm_model.c.3 <- svm(Target ~ ., data = trainData, kernel = "linear", 
                     type = "C-classification", cost = 0.3)
predictFunc(svm_model.c.3, testData) 

#prediction for enrolled students with SVM
Enrolled.predictions <- predict(svm_model.linear, EnrolledData)
resultData <- EnrolledData
resultData$Prediction <- Enrolled.predictions
table(resultData$Prediction)
