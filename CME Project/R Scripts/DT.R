# Load the necessary packages
library(class)
library(haven)
library(foreign)
library(rpart)
library(rpart.plot)

mydata <- read_dta("dataset.dta")
mydata <- subset(mydata, select = -id)

# split your data into training and testing sets
train_index <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

# Set your parameters
criterion <- "gini"
max_depth <- 2
min_samples_leaf <- 0.14
validation_type <- "k-fold"
k_folds <- 10

# Define the model using rpart
model <- rpart(nutritionalStatus ~ ., data = train_data, method = "class",
               control = rpart.control(cp = 0, maxdepth = max_depth, minsplit = min_samples_leaf, minbucket = min_samples_leaf, xval = ifelse(validation_type == "k-fold", k_folds, 0), parms = list(split = criterion)))

# Plot the tree
rpart.plot(model, type = 4, extra = 102, under = TRUE, box.palette = "RdBu", branch.lty = 3, cex = 0.8)

# Make predictions on the test set
predictions <- predict(model, test_data, type = "class")

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