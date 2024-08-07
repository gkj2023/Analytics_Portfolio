# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)
install.packages("zoo")
library(zoo)


# Read the dataset
df <- read.csv("C:/Users/Abhinav/Desktop/sem2/stats2/prj/World-Stock-Prices-Dataset.csv")


# Data preprocessing
# Convert Date column to Date type
df$Date <- ymd_hms(df$Date)

# Handle missing values
df$Volume[is.na(df$Volume)] <- mean(df$Volume, na.rm = TRUE)


# Exploratory Data Analysis (EDA)
# Summary statistics
summary(df)

# Check missing values
sum(is.na(df))
df<-na.omit(df)

# Visualize stock prices over time for each brand
ggplot(df, aes(x = Date, y = Close, color = Brand_Name)) +
  geom_line() +
  ggtitle("Stock Prices Over Time") +
  xlab("Date") +
  ylab("Closing Price") +
  theme_minimal()

# clustering (using K-means)
# Select numerical columns for clustering
numeric_cols <- df %>% select_if(is.numeric)

# Scale the numeric columns
scaled_data <- scale(numeric_cols)

# Find optimal number of clusters using Elbow method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
# Take a random sample of the data
sample_data <- scaled_data[sample(nrow(scaled_data), 1000), ]

# Compute gap statistics for the sample data
gap_stat <- clusGap(sample_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot gap statistics
fviz_gap_stat(gap_stat)




# Based on the plot, decide the number of clusters (k)
k <- 5

# Run K-means clustering
kmeans_model <- kmeans(scaled_data, centers = k)
df$Cluster <- as.factor(kmeans_model$cluster)



ggplot(df, aes(x = Open, y = Close, color = Cluster)) +
  geom_point() +
  ggtitle("K-means Clustering of Stocks") +
  xlab("Open Price") +
  ylab("Close Price") +
  theme_minimal()

# Determining Dominant Industry for Each Cluster
dominant_industry <- df %>% 
  group_by(Cluster) %>% 
  summarise(Dominant_Industry = names(sort(table(Industry_Tag), decreasing = TRUE)[1]))

# Print dominant industry for each cluster
print(dominant_industry)


cluster_industry_freq <- df %>% 
  group_by(Cluster, Industry_Tag) %>% 
  summarise(Count = n()) %>% 
  arrange(Cluster, desc(Count))

# Visualize Cluster-Industry Association
ggplot(cluster_industry_freq, aes(x = Cluster, y = Count, fill = Industry_Tag)) +
  geom_bar(stat = "identity") +
  ggtitle("Cluster-Industry Association") +
  xlab("Cluster") +
  ylab("Frequency") +
  theme_minimal()
  theme(legend.title = element_text("Industry_Tag"))








##########Feature importance analysis

library(xgboost)
set.seed(123)
sample_indices <- sample(nrow(df), 1000)
sample_data <- df[sample_indices, ]

# Prepare data
train_data <- sample_data %>% select(Open, High, Low, Volume, Dividends, `Stock.Splits`)
train_labels <- sample_data$Close




# Convert data to matrix format for XGBoost
dtrain <- xgb.DMatrix(as.matrix(train_data), label = train_labels)

# Train XGBoost model
xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror")

# Feature importance plot
importance_matrix <- xgb.importance(feature_names = colnames(train_data), model = xgb_model)
xgb.plot.importance(importance_matrix)



# Forecasting future stock trend and values for a company from the dominant sector(technology)
install.packages("forecast")
library(forecast)

# Prepare time series data for "apple"
apple_data <- df %>% filter(Brand_Name == "apple") %>% select(Date, Close)
apple_ts <- ts(apple_data$Close, frequency = 365)

# Fit ARIMA model
arima_model <- auto.arima(apple_ts)

# Forecast next 60 days
forecast_values <- forecast(arima_model, h = 60)
plot(forecast_values, main = "ARIMA Forecast for Apple Stock Price")


