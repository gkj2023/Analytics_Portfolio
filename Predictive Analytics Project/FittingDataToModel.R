#Model Fitting
filtered_df <- read.csv("C:/Users/jhaga/OneDrive/Desktop/UsedCarPricePrediction/UsedCarPricePrediction/filtered_df.csv")
summary(filtered_df)

#Making everything neumerical
filtered_df[] <- data.matrix(filtered_df)
filtered_df

filtered_df = subset(filtered_df, select = -c(X, lat, long) ) #Removed unwanted column
colnames(filtered_df)

#df_normalized <- scale(filtered_df)
#print(df_normalized)
normalize_column <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
df_normalized <- apply(filtered_df, 2, normalize_column)
df_normalized <- as.data.frame(df_normalized)
summary(df_normalized)

library(caret)

set.seed(123)  # For reproducibility
training_index <- sample(1:nrow(df_normalized), size = 0.8 * nrow(df_normalized))
training_data <- df_normalized[training_index, ]
testing_data <- df_normalized[-training_index, ]

#Linear regression Model
model <- lm(price ~ ., data = training_data)
predictions <- predict(model, newdata = testing_data)
mse <- mean((testing_data$price - predictions)^2)
print(paste("Mean Squared Error:", mse))
summary(model)

library(ggplot2)
data_for_plot <- data.frame(
  actual_price = testing_data$price,  # Replace with your actual price column name
  predicted_price = predictions
)
ggplot(data_for_plot, aes(x = actual_price, y = predicted_price)) +
  geom_point(aes(color = actual_price), size = 3) +  # Color points by actual price
  labs(title = "Actual vs. Predicted Price",
       x = "Actual Price",
       y = "Predicted Price") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")


# Fit ridge regression model
any(is.na(training_data))
training_matrix <- as.matrix(training_data)
ridge_model <- glmnet(x = training_matrix[, -1], y = training_matrix[, 1], alpha = 0, lambda = 0.1)
ridge_predictions <- predict(ridge_model, newx = as.matrix(testing_data[, -1]))
ridge_mse <- mean((testing_data$price - ridge_predictions)^2)
print(paste("Ridge Regression Mean Squared Error:", ridge_mse))
ridge_data_for_plot <- data.frame(
  actual_price = testing_data$price,
  predicted_price = ridge_predictions
)
ridge_data_for_plot$predicted_price <- ridge_predictions


# Check the structure of ridge_data_for_plot
str(ridge_data_for_plot)

ggplot(ridge_data_for_plot, aes(x = actual_price, y = predicted_price)) +
  geom_point(aes(color = actual_price), size = 3) +
  labs(title = "Actual vs. Predicted Price (Ridge Regression)",
       x = "Actual Price",
       y = "Predicted Price") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

# decision Tree Regressor
library(dplyr)  # for data manipulation
library(rpart)  # for decision tree regression
library(rattle)  # for plotting the decision tree (optional)
library(rpart.plot)

model <- rpart(price ~ ., data = training_data, method = "anova")  # Regression method
predictions <- predict(model, newdata = testing_data)
mse <- mean((testing_data$price - predictions)^2)
print(paste("Mean Squared Error:", mse))

rpart.plot(model, main = "Decision Tree for Price Prediction")

data_for_plot <- data.frame(
  actual_price = testing_data$price,  # Replace with your actual price column name
  predicted_price = predictions
)

# 3. Create the scatter plot (same code as before)
ggplot(data_for_plot, aes(x = actual_price, y = predicted_price)) +
  geom_point(aes(color = actual_price), size = 3, alpha = 0.5) +  # Adjust alpha for transparency
  labs(title = "Actual vs. Predicted Price (Decision Tree)",
       x = "Actual Price",
       y = "Predicted Price") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")