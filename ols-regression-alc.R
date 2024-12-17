# Load datasets
alc_consumption <- read.csv('~/Downloads/archive/student-mat.csv', header = TRUE)
d2 <- read.csv('~/Downloads/archive/student-por.csv', header = TRUE)

# Merge datasets
d3 <- merge(alc_consumption, d2, by = c("Dalc", "failures", "goout", "absences", "studytime", "famrel", "Pstatus"), suffixes = c("", ".y"))

# Remove rows with missing G3
d3 <- d3[!is.na(d3$G3), ]
colnames(d3)

# Subset relevant columns
data <- d3[, c("G3", "Dalc", "Walc", "failures", "goout", "absences", "studytime", "famrel", "Pstatus", "G1", "G2", "Medu", "Fedu")]

# Convert Pstatus to factor
data$Pstatus <- as.factor(data$Pstatus)

# Split data into training and testing sets
set.seed(42)  # For reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit OLS model on training data
ols_model <- lm(G3 ~ Dalc + Walc + failures + goout + absences + studytime + famrel + Pstatus + G1 + G2 + Medu + Fedu, data = train_data)

# Summary of the model
summary(ols_model)

# Predict on test data
ols_predictions <- predict(ols_model, test_data)

# Evaluate model performance using RMSE
ols_rmse <- sqrt(mean((test_data$G3 - ols_predictions)^2))
cat("Test RMSE:", ols_rmse, "\n")

# ---- NEW: Actual vs Predicted Grades Visualization ----

# Create a dataframe for comparison
comparison_df <- data.frame(
  Actual = test_data$G3,
  OLS_Predicted = ols_predictions
)

# Scatter plot of actual vs predicted grades
library(ggplot2)
ggplot(comparison_df, aes(x = Actual, y = OLS_Predicted)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted Final Grades (OLS Regression)",
    x = "Actual Grades",
    y = "Predicted Grades"
  ) +
  theme_minimal()

# Save the plot
ggsave("grade_prediction_comparison.png", width = 10, height = 6)

# Feature Importance Calculation

# Extract t-values from OLS summary
ols_summary <- summary(ols_model)
feature_importance <- abs(ols_summary$coefficients[, 3])  # Absolute t-values
sorted_importance <- sort(feature_importance[-1], decreasing = TRUE)  # Exclude intercept

# Print top 5 most important features
cat("\nTop 5 Most Important Features (by absolute t-value):\n")
print(sorted_importance[1:5])
