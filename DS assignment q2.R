# For decision tree model
library(rpart)

# For data visualization
library(rpart.plot)


#Get Data
df = read.csv("C:/Users/hydar/Documents/UTP/UTP UG YEAR 1/UTP UG YEAR 2 SEM 2/DATA SCIENCE/class_assignment/Train.csv", header = TRUE)
df
test_data = read.csv("C:/Users/hydar/Documents/UTP/UTP UG YEAR 1/UTP UG YEAR 2 SEM 2/DATA SCIENCE/class_assignment/Test.csv", header = TRUE)

formula = df$Segmentation

summary(is.na(df))
dim(df)


fit.tree = rpart(Segmentation ~ ., data=df, method = "class", cp=0.008)
fit.tree

# Visualizing the unpruned tree
rpart.plot(fit.tree)

# Checking the order of variable importance
fit.tree$variable.importance

#Predict Using the Classification Tree
pred.tree = predict(fit.tree, df, type = "class")


# Make predictions on the test data
predictions <- predict(fit.tree, newdata=df, type="class")

# Compute accuracy
accuracy <- sum(predictions == df$Segmentation) / nrow(df)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))




# Step 1: Combine the Original Training and Testing Sets
combined_data <- rbind(df, test_data)

# Step 2 and 3: Perform Five Tests with Different Settings
set.seed(123)  # for reproducibility

# Initialize a vector to store accuracy for each iteration
accuracy_vector <- numeric(5)

for(i in 1:5) {
  # Randomly sample indices for the training set
  train_indices <- sample(1:nrow(combined_data), 0.7 * nrow(combined_data))
  
  # Create new training and testing sets
  train_data_new <- combined_data[train_indices, ]
  test_data_new <- combined_data[-train_indices, ]
  
  # Build the CART model
  fit.tree <- rpart(Segmentation ~ ., data=train_data_new, method="class", cp=0.008)
  
  # Make predictions on the test data
  predictions <- predict(fit.tree, newdata=test_data_new, type="class")
  
  # Compute accuracy
  accuracy <- sum(predictions == test_data_new$Segmentation) / nrow(test_data_new)
  accuracy_vector[i] <- round(accuracy * 100, 2)
  
  # Print accuracy for this iteration
  print(paste("Iteration ", i, " Accuracy: ", accuracy_vector[i], "%"))
}

# Print the average accuracy over the 5 iterations
print(paste("Average Accuracy: ", mean(accuracy_vector), "%"))