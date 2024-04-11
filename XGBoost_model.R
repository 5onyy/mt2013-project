library(tree)
library(caret)
library(ggplot2)
library(xgboost)
library(randomForest)
library(gbm)

data1 <- read.table(file.choose(), header = T, sep = ",")

set.seed(123)
#Test bằng cách sample trên github của bài mẫu
parts <- sample(1:nrow(data1), nrow(data1) * 0.8)

train <- data1[parts,]
test <- data1[-parts,]

train_x <- data.matrix(train[,c("ncore","bfreq","temp")])
train_y <- train[,"tdp"]

test_x = data.matrix(test[, c("ncore","bfreq","temp")])
test_y = test[, "tdp"]

xgb_train = xgb.DMatrix(data = train_x, label  = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)


#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)


param_list <- list(booster="gbtree",eta = 0.1, max_depth = 6)

#define final model

#final = xgb.train(params = param_list, data = xgb_train, watchlist=watchlist, nrounds = 100)
final = xgboost(params = param_list, data = xgb_train, nrounds = 200, verbose = 0)

#final <- gbm(formula = tdp ~ bfreq + ncore + temp, data = train, 
#             distribution = "gaussian", 
#             n.trees = 1000, shrinkage = 0.1,
#             interaction.depth = 6,
#             bag.fraction = 1,
#             n.minobsinnode = 1)

print(final)

#use model to make predictions on test data
pred_tdp = predict(final, xgb_test)

#pred_tdp = predict(final, newdata = test)

accuracyTest <- as.data.frame(test_y, row.names = NULL)
accuracyTest["Predicted_tdp"] <- as.data.frame(pred_tdp, row.names = NULL)

#measure prediction accuracy
mean((test_y - pred_tdp)^2) #mse
sum((test_y - pred_tdp)^2) 
MAE_ofGB <- caret::MAE(test_y, pred_tdp) #mae
caret::RMSE(test_y, pred_tdp) #rmse

r2_check <- cor(accuracyTest$test_y, accuracyTest$Predicted_tdp)^2
print(r2_check)

# Calculate the residuals by subtracting the actual values from the predicted values
residualsGB <- accuracyTest$test_y - accuracyTest$Predicted_tdp

# Create a normal probability plot of the residuals
qqnorm(residualsGB)
qqline(residualsGB)

# Plotting the predicted - actual
ggplot(accuracyTest, aes(x = test_y, y = accuracyTest$Predicted_tdp)) + 
  geom_point(shape=1, color="blue") +
  geom_abline(mapping=aes(intercept= 0, slope=1),color="darkblue") + 
  labs(x = "TDP", y = "TDP Predicted")


