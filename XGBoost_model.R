# Load necessary libraries
library(tree)
library(caret)
library(ggplot2)
library(xgboost)
library(randomForest)
library(gbm)

# Read data from a file
data1 <- read.table(file.choose(), header = T, sep = ",")

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets
parts <- sample(1:nrow(data1), nrow(data1) * 0.8)
train <- data1[parts,]
test <- data1[-parts,]

# Extract predictors (features) and target variable from training and testing sets
train_x <- data.matrix(train[,c("ncore","bfreq","temp")])
train_y <- train[,"tdp"]
test_x <- data.matrix(test[, c("ncore","bfreq","temp")])
test_y <- test[, "tdp"]

# Convert data to xgboost matrix format
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Define watchlist for tracking performance during training
watchlist = list(train = xgb_train, test = xgb_test)

# Define parameters for the xgboost model
param_list <- list(booster = "gbtree", eta = 0.1, max_depth = 6)

# Train the xgboost model
final = xgboost(params = param_list, data = xgb_train, nrounds = 200, verbose = 0)

# Print final model details
print(final)

# Make predictions on test data
pred_tdp <- predict(final, xgb_test)

# Create dataframe for test_y and predicted values
accuracyTest <- as.data.frame(test_y, row.names = NULL)
accuracyTest["Predicted_tdp"] <- as.data.frame(pred_tdp, row.names = NULL)

# Measure prediction accuracy
print(paste("MSE: ", mean((test_y - pred_tdp)^2))) # Mean Squared Error (MSE)
print(paste("MAE: ", caret::MAE(test_y,pred_tdp))) # Mean Absolute Error (MAE)
print(paste("RMSE: ", caret::RMSE(test_y,pred_tdp))) # Root Mean Squared Error (RMSE)

# Calculate R squared on testing data
r2_check <- cor(accuracyTest$test_y, accuracyTest$Predicted_tdp)^2
print(r2_check)

# Calculate the residuals
residualsGB <- accuracyTest$test_y - accuracyTest$Predicted_tdp

# Create a normal probability plot of the residuals
qqnorm(residualsGB)
qqline(residualsGB)

# Plot predicted vs actual
ggplot(accuracyTest, aes(x = test_y, y = accuracyTest$Predicted_tdp)) + 
  geom_point(shape=1, color="darkblue") +
  geom_abline(mapping=aes(intercept= 0, slope=1),color="red") + 
  labs(x = "TDP", y = "TDP Predicted")