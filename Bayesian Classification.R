# Load required library
library(e1071)  # Naive Bayes classifier

# Split the data into training and testing sets
set.seed(42)  # reproducibility
train_indices <- sample(1:nrow(all_num_clean), nrow(all_num_clean) * 0.8)  # 80% for training
train_data <- all_num_clean[train_indices, ]
test_data <- all_num_clean[-train_indices, ]

# Train a Naive Bayes classifier
model <- naiveBayes(severity ~ ., data = train_data)

# Make predictions on the test data
predictions <- predict(model, test_data)

# Evaluate the model's performance
accuracy <- sum(predictions == test_data$severity) / nrow(test_data)

accuracy
