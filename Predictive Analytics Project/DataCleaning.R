library(ggplot2)

data <- read.csv("C:/Users/jhaga/OneDrive/Desktop/UsedCarPricePrediction/UsedCarPricePrediction/vehicles.csv")
summary(data)

#removing county since it does not contain any data
data <- data[, !(names(data) == "county")]
summary(data)

# We can safely remove Id as well since the values are non usable
hist(data$id, main = "Histogram of ID", xlab = "Values")
data <- data[, !(names(data) == "id")]
summary(data)

# posting_date, VIN, description, image_url, URL and region IRL are another features we can't make use of. Dropping them
data <- data[, !(names(data) %in% c("url", "region_url", "image_url", "VIN", "description", "posting_date"))]

na_count <- colSums(is.na(data))
print(na_count)

#checking for NA in data
if (any(is.na(data))) {
  df_clean <- na.omit(data)
}
summary(df_clean)

# Let's plot the DF based on lat and long
df_local_lat_long <- df_clean[df_clean$lat > 24 & df_clean$lat < 50 & df_clean$long > -125 & df_clean$long < -65, ]
ggplot(data = df_local_lat_long, aes(x = long, y = lat)) +
  geom_point() +
  labs(title = "Region Compare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Cleaning Manufacturer Column
# Get the counts of unique values in the 'manufacturer' column
manufacturer_counts <- table(df_clean$manufacturer)
barplot(manufacturer_counts, 
        main = "Manufacturer Count", 
        xlab = "Manufacturer", 
        ylab = "Count", 
        col = "skyblue", 
        las = 2)
# Get the counts of unique values in the 'manufacturer' column
manufacturer_counts <- table(df_clean$manufacturer)
# Get the top 20 manufacturers
top_manufacturers <- names(sort(manufacturer_counts, decreasing = TRUE))[1:20]
# Recode 'manufacturer' column
df_clean$manufacturer <- ifelse(df_clean$manufacturer %in% top_manufacturers, df_clean$manufacturer, "other")
manufacturer_counts <- table(df_clean$manufacturer)
barplot(manufacturer_counts, 
        main = "Manufacturer Count", 
        xlab = "Manufacturer", 
        ylab = "Count", 
        col = "skyblue", 
        las = 2)

# Cleaning Region column
# Get the counts of unique values in the 'region' column
region_counts <- table(df_clean$region)

# Get the top 50 regions
top_regions <- names(sort(region_counts, decreasing = TRUE))[1:50]

# Recode 'region' column
df_clean$region <- ifelse(df_clean$region %in% top_regions, df_clean$region, "other")
# Get the counts of unique values in the 'region' column after recoding
region_counts_after_recode <- table(df_clean$region)
# Create a bar plot
barplot(region_counts_after_recode,
        main = "Top 50 Regions Count",
        xlab = "Region",
        ylab = "Count",
        col = "skyblue",
        las = 2)  # Rotate x-axis labels vertically

# Cleaning Model column
# Get the counts of unique values in the 'model' column
model_counts <- table(df_clean$model)

# Get the top 50 models
top_models <- names(sort(model_counts, decreasing = TRUE))[1:50]

# Recode 'model' column
df_clean$model <- ifelse(df_clean$model %in% top_models, df_clean$model, "other")

# Get the counts of unique values in the 'model' column after recoding
model_counts_after_recode <- table(df_clean$model)

# Create a bar plot
barplot(model_counts_after_recode,
        main = "Top 50 Models Count",
        xlab = "Model",
        ylab = "Count",
        col = "skyblue",
        las = 2)  # Rotate x-axis labels vertically

# Plot of price distribution
# Set the figure size
options(repr.plot.width=11, repr.plot.height=5)

# Create a histogram with kernel density estimate
hist(df_clean$price, 
     main = "Car Price Distribution Plot",
     xlab = "Price",
     ylab = "Density",
     col = "skyblue",
     freq = FALSE)  # Set freq = FALSE to plot density instead of frequency

# Add a kernel density estimate
lines(density(df_clean$price), col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Density", "Kernel Density Estimate"), fill = c("skyblue", "red"))
# removing outliers
col <- "price"
q1 <- quantile(df_clean[[col]], 0.25)
q3 <- quantile(df_clean[[col]], 0.75)
iqr <- q3 - q1
lower_limit <- q1 - 1.5 * iqr
upper_limit <- q3 + 1.5 * iqr
filtered_df <- df_clean[df_clean[[col]] > lower_limit & df_clean[[col]] < upper_limit, ]
# Create a histogram with kernel density estimate
hist(filtered_df$price, 
     main = "Car Price Distribution Plot",
     xlab = "Price",
     ylab = "Density",
     col = "skyblue",
     freq = FALSE)  # Set freq = FALSE to plot density instead of frequency

# Add a kernel density estimate
lines(density(filtered_df$price), col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Density", "Kernel Density Estimate"), fill = c("skyblue", "red"))

# Same way Removing ouliers for year and odomoter columns
cols <- c("odometer", "year")

# Loop through each column
for (col in cols) {
  # Calculate quantiles
  q1 <- quantile(filtered_df[[col]], 0.25)
  q3 <- quantile(filtered_df[[col]], 0.75)
  
  # Calculate IQR
  iqr <- q3 - q1
  
  # Calculate lower and upper limits
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  
  # Filter dataframe to remove outliers
  filtered_df <- filtered_df[filtered_df[[col]] > lower_limit & filtered_df[[col]] < upper_limit, ]
}
# Plotting them
# Plot the distribution of 'odometer'
hist(filtered_df$odometer, 
     main = "Odometer Distribution Plot",
     xlab = "Odometer",
     ylab = "Frequency",
     col = "skyblue",
     freq = FALSE)  # Set freq = FALSE to plot density instead of frequency

# Add a density plot
lines(density(filtered_df$odometer), col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Density", "Kernel Density Estimate"), fill = c("skyblue", "red"))

# Plot the distribution of 'year'
hist(filtered_df$year, 
     main = "Year Distribution Plot",
     xlab = "Year",
     ylab = "Frequency",
     col = "skyblue",
     freq = FALSE)  # Set freq = FALSE to plot density instead of frequency

# Add a density plot
lines(density(filtered_df$year), col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Density", "Kernel Density Estimate"), fill = c("skyblue", "red"))

summary(filtered_df)

#write.csv(filtered_df,"C:\Users\jhaga\OneDrive\Documents\Downloads\filtered_df.csv")

#write.csv(filtered_df, "C:\\Users\\jhaga\\OneDrive\\Documents\\Downloads\\filtered_df.csv")

#write.csv(filtered_df, file = "C:/Users/jhaga/OneDrive/Documents/Downloads/filtered_df.csv", row.names = FALSE)

# Create the directory if it doesn't exist
#dir.create("C:/Users/jhaga/OneDrive/Documents/Downloads", showWarnings = FALSE)

# Write the CSV file
#write.csv(filtered_df, file = "C:/Users/jhaga/OneDrive/Documents/Downloads/filtered_df.csv", row.names = FALSE)


