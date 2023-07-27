# Load the necessary packages
library(class)
library(haven)
library(foreign)
library(randomForest)
library(caret)
library(xgboost)
library(rpart)

mydata <- read_dta("dataset.dta")
mydata <- subset(mydata, select = -id)

# split your data into training and testing sets
train_index <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

# XGBOOST

set.seed(0)
params <- list(
  objective = "binary:logistic",
  nthread = 3,
  min_child_weight = 1,
  max_depth = 15,
  colsample_bytree = 0.7,
  learning_rate = 0.3
)
xgb_model <- xgboost(
  data = as.matrix(train_data[, -16]),
  label = train_data$nutritionalStatus,
  params = params,
  n_estimators = 100,
  verbose = 1,
  nrounds=20
)
test_features <- as.matrix(test_data[, -16])
xgb_preds <- predict(xgb_model, test_features)
xgb_preds <- ifelse(xgb_preds > 0.5, 1, 0)

# DT

criterion <- "gini"
max_depth <- 2
min_samples_leaf <- 0.14
validation_type <- "k-fold"
k_folds <- 10
dt_model <- rpart(nutritionalStatus ~ ., data = train_data, method = "class",
               control = rpart.control(cp = 0, maxdepth = max_depth, minsplit = min_samples_leaf, minbucket = min_samples_leaf, xval = ifelse(validation_type == "k-fold", k_folds, 0), parms = list(split = criterion)))
dt_preds <- predict(dt_model, test_data, type = "class")

# RF

mydata$nutritionalStatus <- as.character(mydata$nutritionalStatus)
mydata$nutritionalStatus <- as.factor(mydata$nutritionalStatus)
predictors <- names(mydata)[1:(ncol(mydata)-1)]
rf_model <- randomForest(formula = as.formula(paste(names(mydata)[ncol(mydata)], "~", paste(predictors, collapse="+"))),
                         data = train_data,
                         importance = TRUE,
                         ntree = 50,
                         mtry = sqrt(length(predictors)),
                         max_depth = 10)
rf_preds <- predict(rf_model, newdata = test_data)

# KNN

train_data_feature <- mydata[train_index, -16] 
train_target <- mydata[train_index, 16]
test_data_feature <- mydata[-train_index, -16] 
test_target <- mydata[-train_index, 16]

train_data_scaled_feauture <- scale(train_data_feature)
test_data_scaled_feature <- scale(test_data_feature)
knn_preds <- knn(train_data_scaled_feauture, test_data_scaled_feature, data.matrix(train_target), k=25)




# Ensemble Prediction
ensemble_pred <- ifelse(xgb_preds == 1 | rf_preds == 1 | dt_preds == 1 | knn_preds == 1, 1, 0)

# Evaluate the model
acc <- mean(ensemble_pred == as.factor(test_data$nutritionalStatus))
cm <- confusionMatrix(factor(ensemble_pred), factor(test_data$nutritionalStatus))
precision <- cm$byClass[1]
recall <- cm$byClass[2]
F1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(acc, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(F1_score, 4)))