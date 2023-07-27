# Load the necessary packages
library(class)
library(xgboost)
library(pROC)

mydata <- read_dta("dataset.dta")
mydata <- subset(mydata, select = -id)

# split your data into training and testing sets
train_index <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

set.seed(0)
# define your XGBoost model parameters
params <- list(
  objective = "binary:logistic",
  nthread = 3,
  min_child_weight = 1,
  max_depth = 15,
  colsample_bytree = 0.7,
  learning_rate = 0.3
)

# train the XGBoost model
xgb_model <- xgboost(
  data = as.matrix(train_data[, -16]),
  label = train_data$nutritionalStatus,
  params = params,
  n_estimators = 100,
  verbose = 1,
  nrounds=20
)

test_features <- as.matrix(test_data[, -16])

# make predictions on the testing set
preds <- predict(xgb_model, test_features)

# evaluate the performance of the model
preds <- ifelse(preds > 0.5, 1, 0)

importances <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix = importances)

acc <- mean(preds == test_data$nutritionalStatus)
cm <- confusionMatrix(factor(preds), factor(test_data$nutritionalStatus))
precision <- cm$byClass[1]
recall <- cm$byClass[2]
F1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(acc, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(F1_score, 4)))