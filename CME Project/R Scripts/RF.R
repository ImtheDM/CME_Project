# Load the necessary packages
library(class)
library(haven)
library(foreign)
library(randomForest)
library(caret)

my_data <- read_dta("dataset.dta")
my_data <- subset(my_data, select = -id)

my_data$nutritionalStatus <- as.character(my_data$nutritionalStatus)
my_data$nutritionalStatus <- as.factor(my_data$nutritionalStatus)

# Split the data into training and testing sets (assuming 70% for training and 30% for testing)
set.seed(123)
train_indices <- sample(nrow(my_data), round(nrow(my_data)*0.7), replace = FALSE)
train_data <- my_data[train_indices, ]
test_data <- my_data[-train_indices, ]

# Define the predictor variables (assuming that all columns except the last one are predictors)
predictors <- names(my_data)[1:(ncol(my_data)-1)]

# Fit the random forest model
rf_model <- randomForest(formula = as.formula(paste(names(my_data)[ncol(my_data)], "~", paste(predictors, collapse="+"))),
                         data = train_data,
                         importance = TRUE,
                         ntree = 100,
                         mtry = sqrt(length(predictors)),
                         max_depth = 10)

# Make predictions on the testing set
predictions <- predict(rf_model, newdata = test_data)

# Plot variable importance scores
varImpPlot(rf_model, type = 1, pch = 19, col = "blue", main = "Variable Importance Plot")

# Evaluate the model
acc <- mean(predictions == test_data$nutritionalStatus)
cm <- confusionMatrix(factor(predictions), factor(test_data$nutritionalStatus))
precision <- cm$byClass[1]
recall <- cm$byClass[2]
F1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(acc, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(F1_score, 4)))