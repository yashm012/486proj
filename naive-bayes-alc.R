# Load necessary library
library(e1071)

# Use d3 (merged dataset) and subset relevant columns
data <- d3[, c("G3", "Dalc", "Walc", "failures", "goout", "absences", 
               "studytime", "famrel", "Pstatus", "Medu", "Fedu")]

# Remove rows with missing values
data <- na.omit(data)

# Bin G3 into categories: Low (0-10), Medium (11-15), High (16-20)
data$G3 <- cut(data$G3, breaks = c(0, 10, 15, 20), labels = c("Low", "Medium", "High"))

# Convert G3 to a factor
data$G3 <- as.factor(data$G3)

# Ensure all predictors are factors (for consistency with categorical NB)
for (col in names(data)[-1]) {
  data[[col]] <- as.factor(data[[col]])
}

# Split data into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit Naive Bayes model
nb_model <- naiveBayes(G3 ~ ., data = train_data)

# Predict on test data
nb_predictions <- predict(nb_model, test_data)

# Confusion Matrix
nb_confusion_matrix <- table(Predicted = nb_predictions, Actual = test_data$G3)
print("Naive Bayes Confusion Matrix:")
print(nb_confusion_matrix)

# Accuracy
nb_accuracy <- sum(diag(nb_confusion_matrix)) / sum(nb_confusion_matrix)
print(paste("Naive Bayes Accuracy:", round(nb_accuracy, 4)))