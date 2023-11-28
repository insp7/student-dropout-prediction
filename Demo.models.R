#load packages --------

library(tidyverse)
library(corrplot)
library(e1071)
library(caret)
library(here)
library(randomForest)
library(gbm)

#read in data --------

fullData <- read.csv(here("data", "AcademicSuccess.csv"))

#filter categories out in Target--------

filterFullData <- fullData %>%
  filter(Target != "Enrolled")

EnrolledData <- fullData %>%
  filter(!(Target %in% c("Dropout", "Graduate")))

#summary the Target --------

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

plot_freqpoly(filterFullData, Nacionality, 0.5)
plot_freqpoly(filterFullData, Curricular.units.1st.sem..grade., 0.3)

#split the data --------

set.seed(123)

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

#predict function --------

predictFunc <- function(model, testData){
  predictions <- predict(model, testData)
  
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
}

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

#Gradient Boosting Model --------
gbm_model <- gbm(Target ~ ., data = numericTrainData, distribution = "bernoulli",  
                 n.trees = 150, interaction.depth = 7, n.minobsinnode = 40, shrinkage = 0.1)    

gbm_predictions <- predict(gbm_model, testData, n.trees = 150, type = "response")
gbm_predicted_classes <- ifelse(gbm_predictions > 0.5, "Graduate", "Dropout")
gbm_predicted_classes <- factor(gbm_predicted_classes, levels = levels(testData$Target))

gbm_confMat <- confusionMatrix(gbm_predicted_classes, testData$Target)
print(gbm_confMat)

#find the best Gradient Boosting Model --------
grid <- expand.grid(
  interaction.depth = c(6, 7, 8, 9),     
  n.trees = seq(150, 300, by = 50), 
  shrinkage = c(0.1, 0.2, 0.5),    
  n.minobsinnode = c(20, 30, 40)         
)

control <- trainControl(
  method = "cv",    
  number = 5         
)

gbm_grid_model <- train(
  Target ~ ., 
  data = trainData,
  method = "gbm",
  trControl = control,
  verbose = FALSE,
  tuneGrid = grid,
  metric = "Accuracy"
)

print(gbm_grid_model$bestTune)

#Logistic Regression Model --------
logistic_model <- glm(Target ~ ., data = trainData, family = binomial)
LRpredictFunc(logistic_model, testData, threshold = 0.48)

#find the best threshold for the Logistic Regression Model --------

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

#Random Forest Model --------
rf_model <- randomForest(Target ~ ., data = trainData, ntree = 600)
predictFunc(rf_model, testData)

#find the best ntree for the Random Forest Model --------

ntree_values <- seq(500, 650, by = 25)
accuracies <- numeric(length(ntree_values))

for (i in seq_along(ntree_values)) {
  set.seed(123) 
  rf_model <- randomForest(Target ~ ., data = trainData, ntree = ntree_values[i])
  predictions <- predict(rf_model, testData)
  
  accuracies[i] <- sum(predictions == testData$Target) / nrow(testData)
}

plot(ntree_values, accuracies, type = "b", col = "red",
     xlab = "ntree", ylab = "Accuracy")

optimal_ntrees <- ntree_values[which.max(accuracies)]
best_accuracy <- max(accuracies)
print(paste("Optimal ntree: ", optimal_ntrees))
print(paste("Best accuracy: ", best_accuracy))

#choose an appropriate kernel for the SVM model --------

svm_model.radial <- svm(Target ~ ., data = trainData, kernel = "radial", type = "C-classification")
svm_model.linear <- svm(Target ~ ., data = trainData, kernel = "linear", type = "C-classification")

predictFunc(svm_model.radial, testData)
predictFunc(svm_model.linear, testData) 

#The svm_model with kernel = "linear" has a better performance

#tune the SVM Model --------

set.seed (1)
tune.model <- tune(svm, Target ~ ., data = trainData, kernel = "linear",
                   range = list(cost = seq(0.1, 2, by = 0.1)))

summary(tune.model)

plot(tune.model, main = "SVM Hyperparameter Tuning Results", xlab = "Cost", ylab = "Cross-Validation Error")
abline(v = tune.model$best.parameters$cost, col = "red", lwd = 1)
grid()

svm_model.c.3 <- svm(Target ~ ., data = trainData, kernel = "linear", 
                     type = "C-classification", cost = 0.3)
predictFunc(svm_model.c.3, testData) 

#test SVM Model with different costs --------

calculate_metrics <- function(cost, trainData, testData) {
  model <- svm(Target ~ ., data = trainData, kernel = "linear", type = "C-classification", cost = cost)
  predictions <- predict(model, testData)
  predictions <- factor(predictions, levels = levels(testData$Target))
  confMat <- confusionMatrix(predictions, testData$Target)
  
  return(c(Accuracy = confMat$overall['Accuracy'], 
           Precision = confMat$byClass['Pos Pred Value'],
           Recall = confMat$byClass['Sensitivity'],
           F1 = 2 * (confMat$byClass['Pos Pred Value'] * confMat$byClass['Sensitivity']) / (confMat$byClass['Pos Pred Value'] + confMat$byClass['Sensitivity'])
  ))
}

cost_values <- seq(0.1, 2, by = 0.01)
metrics_list <- lapply(cost_values, function(c) calculate_metrics(c, trainData, testData))
metrics_df <- do.call(rbind, metrics_list)
metrics_df <- data.frame(Cost = cost_values, metrics_df)
names(metrics_df) <- c("Cost", "Accuracy", "Precision", "Recall", "F1")

metrics_long <- melt(metrics_df, id.vars = "Cost", variable.name = "Metric", value.name = "Value")

ggplot(metrics_long, aes(x = Cost, y = Value, colour = Metric)) +
  geom_line() +
  labs(title = 'SVM Performance Across Cost Values',
       x = 'Cost', y = 'Metric Value') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("Accuracy" = "blue",
                                "Precision" = "red",
                                "Recall" = "green",
                                "F1" = "orange"))+
  geom_vline(xintercept = 0.17, linetype = "dashed", color = "black")

svm_model.c.17 <- svm(Target ~ ., data = trainData, kernel = "linear", 
                      type = "C-classification", cost = 0.17)
predictFunc(svm_model.c.17, testData) 

#correlation analysis --------

cor_matrix <- cor(filterFullData[, sapply(filterFullData, is.numeric)])
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
print(importanceScores)

#remove less important and highly correlated features --------

remove.features <- function(Data) {
  Data$Educational.special.needs <- NULL
  Data$Curricular.units.2nd.sem..credited.<- NULL
  return(Data)
}

newTrainData <- remove.features(trainData)
newTestData <- remove.features(testData)

newTrainData$Target <- as.factor(newTrainData$Target)
newTestData$Target <- as.factor(newTestData$Target) 

#recode the Target variable as numeric --------

newNumericTrainData <- newTrainData
newNumericTestData <- newTestData

newNumericTrainData$Target <- ifelse(newNumericTrainData$Target == "Graduate", 1, 0)
newNumericTestData$Target <- ifelse(newNumericTestData$Target == "Graduate", 1, 0)

#SVM Model with new Data--------

newSvm_model<- svm(Target ~ ., data = newTrainData, kernel = "linear", type = "C-classification", cost = 0.17)
predictFunc(newSvm_model, newTestData) 

#Logistic Regression Model with new Data--------

newLr_model <- glm(Target ~ ., data = newTrainData, family = binomial)
LRpredictFunc(newLr_model, newTestData, threshold = 0.48)

#Random Forest Model with new Data --------

newRf_model <- randomForest(Target ~ ., data = newTrainData, ntree = 600)
predictFunc(newRf_model, newTestData)

#GBM Model with new Data --------

newGbm_model <- gbm(Target ~ ., data = newNumericTrainData, distribution = "bernoulli",  
                 n.trees = 150, interaction.depth = 7, n.minobsinnode = 40, shrinkage = 0.1)    

newGbm_predictions <- predict(newGbm_model, newTestData, n.trees = 150, type = "response")
newGbm_predicted_classes <- ifelse(newGbm_predictions > 0.5, "Graduate", "Dropout")
newGbm_predicted_classes <- factor(newGbm_predicted_classes, levels = levels(newTestData$Target))

newGbm_confMat <- confusionMatrix(newGbm_predicted_classes, newTestData$Target)
print(newGbm_confMat)

#prediction for enrolled students with SVM

EnrolledData <- EnrolledData %>%
  select(
    -Educational.special.needs,
    -International,
    -Nacionality,
    -Curricular.units.2nd.sem..credited.
  )

Enrolled.predictions <- predict(svm_model, EnrolledData)

resultData <- EnrolledData
resultData$Prediction <- Enrolled.predictions
table(resultData$Prediction)
