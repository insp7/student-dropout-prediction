#This model is used to test different costs.

#load packages --------

library(e1071)
library(caret)
library(here)
library(reshape2)

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

#SVM model with different cost--------

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


#reshape and plot
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