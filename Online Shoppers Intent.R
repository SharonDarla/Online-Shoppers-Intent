install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caTools")
# Load necessary libraries
library(caret)         # For model training and evaluation
library(dplyr)         # For data manipulation
library(ggplot2)       # For data visualization
library(caTools)       # For train-test split

# Load the dataset
data_url <- "C:/Users/saias/Downloads/online+shoppers+purchasing+intention+dataset (1)/online_shoppers_intention.csv"
dataset <- read.csv(data_url)

# View dataset structure
str(dataset)

# Data Preprocessing
# Convert necessary variables to factor
dataset$Revenue <- as.factor(dataset$Revenue)
dataset$Weekend <- as.factor(dataset$Weekend)
dataset$Month <- as.factor(dataset$Month)
dataset$VisitorType <- as.factor(dataset$VisitorType)

# Check for missing values and handle them
dataset <- na.omit(dataset)  # Remove rows with missing values

# Split data into training and testing sets (70-30 split)
set.seed(123)
split <- sample.split(dataset$Revenue, SplitRatio = 0.7)
train_data <- subset(dataset, split == TRUE)
test_data <- subset(dataset, split == FALSE)

# Logistic Regression Model
logistic_model <- glm(Revenue ~ ., data = train_data, family = "binomial")

# Summary of the model
summary(logistic_model)

# Predict on the test dataset
predictions <- predict(logistic_model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Convert predictions to factor
predicted_classes <- as.factor(predicted_classes)
levels(predicted_classes) <- c("FALSE", "TRUE")

# Model Evaluation
confusion_matrix <- confusionMatrix(predicted_classes, test_data$Revenue)
print(confusion_matrix)

# Accuracy, Precision, and Recall
cat("Accuracy: ", confusion_matrix$overall["Accuracy"], "\n")
cat("Precision: ", confusion_matrix$byClass["Precision"], "\n")
cat("Recall: ", confusion_matrix$byClass["Recall"], "\n")

# Visualization of Results
ggplot(data = test_data, aes(x = Revenue, fill = predicted_classes)) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs Predicted Purchase Intention", x = "Actual Revenue", y = "Count") +
  scale_fill_manual(values = c("red", "green"), name = "Predicted Revenue")
