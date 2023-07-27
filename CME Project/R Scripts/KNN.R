# Load the necessary packages
library(class)
library(haven)
library(foreign)

mydata <- read_dta("dataset.dta")
mydata <- subset(mydata, select = -id)

# Split the data into training and testing sets
x <- scale(mydata[,1:15])
y<- mydata$nutritionalStatus
table(y)

set.seed(0)
test <- sample(1:nrow(x), nrow(x)*0.3)
K25 <- knn(x[-test,], x[test,],y[-test], k=25)

res<- data.frame(y[test],K25)

# Evaluate the model
acc <- mean(K25 == y[test])
cm <- confusionMatrix(factor(K25), factor(y[test]))
precision <- cm$byClass[1]
recall <- cm$byClass[2]
F1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", round(acc, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(F1_score, 4)))