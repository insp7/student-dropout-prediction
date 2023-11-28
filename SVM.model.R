#load packages --------

library(tidyverse)
library(e1071)
library(caret)
library(here)
library(randomForest)

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

#convert Target to factor for SVM model --------

trainData$Target <- as.factor(trainData$Target)
testData$Target <- as.factor(testData$Target)

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

#choose an appropriate kernel --------

svm_model.radial <- svm(Target ~ ., data = trainData, kernel = "radial", type = "C-classification")
svm_model.linear <- svm(Target ~ ., data = trainData, kernel = "linear", type = "C-classification")

predictFunc(svm_model.radial, testData)
predictFunc(svm_model.linear, testData) 

#The svm_model with kernel = "linear" has a better performance

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
corrplot(cor_matrix, method = "color")

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
  Data$International <- NULL
  Data$Nacionality <- NULL
  Data$Curricular.units.2nd.sem..credited.<- NULL
  return(Data)
}

newTrainData <- remove.features(trainData)
newTestData <- remove.features(testData)

newTrainData$Target <- as.factor(newTrainData$Target)
newTestData$Target <- as.factor(newTestData$Target) 

svm_model<- svm(Target ~ ., data = newTrainData, kernel = "linear", type = "C-classification")

predictFunc(svm_model, newTestData) 

#tune the SVM Model --------

set.seed (1)
tune.model <- tune(svm, Target ~ ., data = newTrainData, kernel = "linear",
                   range = list(cost = seq(0.1, 2, by = 0.1)))

summary(tune.model)

plot(tune.model, main = "SVM Hyperparameter Tuning Results", xlab = "Cost", ylab = "Cross-Validation Error")
abline(v = tune.model$best.parameters$cost, col = "red", lwd = 1)
grid()

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
metrics_list <- lapply(cost_values, function(c) calculate_metrics(c, newTrainData, newTestData))
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
  geom_vline(xintercept = 0.24, linetype = "dashed", color = "black")

#the final SVM Model --------

svm_model<- svm(Target ~ ., data = newTrainData, kernel = "linear", type = "C-classification", cost = 0.4)

predictFunc(svm_model, newTestData) 

#prediction for enrolled students

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
