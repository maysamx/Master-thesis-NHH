
library(data.table)

#remove.packages("keras")
#remove.packages("tensorflow")
#install.packages("keras")
library(keras)
#install_keras()
library(dplyr)



# Attempt to load the CSV file with a suspected delimiter
data <- read.csv("titlonmoyin.csv", header = TRUE, sep = "c")  # If "c" is suspected as part of the delimiter

# try to see if there's a complex delimiter like "c\" or similar
#data <- read.csv("titlonmoyin.csv", header = TRUE, sep = 'c')  # If 'c"' is the delimiter

# Check the structure of the data to see if columns are properly separated
str(data)


# Replace commas with dots and convert to numeric where needed
data <- data %>%
  mutate(across(c(SMB, HML, LIQ, MOM, Close), ~as.numeric(gsub(",", ".", .x))),
         Date = as.Date(Date, format = "%Y-%m-%d"))  # Convert Date to Date type


#write.csv(data, "titlonmoyinready.csv", row.names = FALSE)
data <- read.csv("titlonmoyinready.csv")

str(data)
summary(data)



# Read only the first 100,000 rows
#data <- fread("t (2).csv", sep = ",", header = TRUE, nrows = 2000000)
#data <- read.csv("titlonmoyin.csv")
#data <- fread("t (1).csv", sep = "c", header = TRUE, fill = TRUE)# , nrows = 1600000 ) ###500mb file
#data <- fread("t (2).csv", sep = "c", header = TRUE, fill = TRUE, nrows = 1000000, quote = "")  ###700mb file noe khoondan 2 file bayad motefavet bashe avali ba c joda shode dovomi ba ;
 
#data <- fread("t (2).csv",nrows = 200000)
#numeric_columns <- c("Open", "High", "Low", "Close")  # Specify your numeric columns
#data[, (numeric_columns) := lapply(.SD, function(x) gsub(",", ".", x)), .SDcols = numeric_columns]

# Specify numeric columns
numeric_columns <- c("Open", "High", "Low", "Close","OfficialVolume")

# Replace commas with periods in numeric columns
#for (col in numeric_columns) {
  data[[col]] <- gsub(",", ".", data[[col]])
}

# Write the modified data back to the CSV file
write.csv(data, "700mb_,..csv", row.names = FALSE)
data <- read.csv("700mb_,..csv")

# Check if there are any NAs or non-numeric values
if (any(is.na(data$Close))) {
  cat("There are NAs in the 'Close' column.\n")
  # Handle NAs, remove rows or replace with mean/median
  # data <- na.omit(data) # Remove rows with NAs
}



# Load necessary libraries
library(quantmod)

#  data is loaded into a data frame called 'data' and is sorted by date
# Convert your data frame to an xts object for quantmod
stock_data_xts <- xts(data[, -1], order.by = as.Date(data$Date))




# Calculate Moving Averages
data$MA5 <- SMA(stock_data_xts$Close, n=5)
data$MA10 <- SMA(stock_data_xts$Close, n=10)
data$MA20 <- SMA(stock_data_xts$Close, n=20)


# Save data frame to a CSV file
write.csv(data, "700mb.csv", row.names = FALSE)
# Load data frame from the CSV file
data <- read.csv("700mb.csv")



 #Remove rows with NAs in MA5 and MA10
data <- data[!is.na(data$MA5) & !is.na(data$MA10), ]


data$Close <- as.numeric(as.character(data$Close))
data$MA5 <- as.numeric(as.character(data$MA5))
# Check for unique values in 'Close' and 'MA5' columns
print(unique(data$Close))
print(unique(data$MA5))

# Now calculate BIAS 
data$BIAS5 <- (data$Close - data$MA5) / data$MA5 * 100
data$BIAS10 <- (data$Close - data$MA10) / data$MA10 * 100

# Save data frame to a CSV file
#write.csv(data, "700mb.csv", row.names = FALSE)


library(zoo)
library(data.table)

calculate_RSI <- function(prices, n) {
  # Calculate daily changes in price
  deltas <- diff(prices)
  
  # Separate gains and losses
  gains <- pmax(deltas, 0)
  losses <- pmax(-deltas, 0)
  
  # Calculate the average gains and losses using a rolling mean
  avg_gains <- rollapply(gains, n, mean, fill = NA, align = 'right')
  avg_losses <- rollapply(losses, n, mean, fill = NA, align = 'right')
  
  # Calculate the RS (Relative Strength)
  rs <- avg_gains / avg_losses
  
  # Calculate the RSI (Relative Strength Index)
  rsi <- 100 - (100 / (1 + rs))
  
  return(rsi)
}

#  
# Convert 'Close' column to numeric if it's not
#data[, Close := as.numeric(as.character(Close))]

# Calculate RSI for 6 periods


rsi6_values <- calculate_RSI(data$Close, 6)
# Prepend NA to the rsi6_values to match the length of the data  
rsi6_values <- c(NA, rsi6_values)

# Now the lengths match and you can assign it to the data table
data$RSI6 <- rsi6_values

# Do the same for RSI12 if needed
rsi12_values <- c(NA, calculate_RSI(data$Close, 12))
data$RSI12 <- rsi12_values


# Save data frame to a CSV file
#write.csv(data, "700mb.csv", row.names = FALSE)




calculate_stoch <- function(high, low, close, n, smoothK, smoothD) {
  # Calculate %K
  lowest_low <- rollapply(low, n, min, fill = NA, align = 'right')
  highest_high <- rollapply(high, n, max, fill = NA, align = 'right')
  fastK <- (close - lowest_low) / (highest_high - lowest_low) * 100
  
  # Smooth %K to get the slow %K
  slowK <- rollapply(fastK, smoothK, mean, fill = NA, align = 'right')
  
  # Calculate %D as a moving average of %K
  fastD <- rollapply(slowK, smoothD, mean, fill = NA, align = 'right')
  
  return(list(fastK = fastK, fastD = fastD))
}

#  
high_prices <- data$High
low_prices <- data$Low
close_prices <- data$Close

# Set the parameters
n <- 14       # The look-back period for %K
smoothK <- 3  # The smoothing period for %K
smoothD <- 3  # The smoothing period for %D

# Calculate stochastic oscillator
stoch_values <- calculate_stoch(high_prices, low_prices, close_prices, n, smoothK, smoothD)

# Add the stochastic values to your data frame
data$Stoch_K <- stoch_values$fastK
data$Stoch_D <- stoch_values$fastD

# Save data frame to a CSV file
#write.csv(data, "700mb.csv", row.names = FALSE)


#load the TTR package 
library(TTR)

# Convert the 'Close' column of your data to numeric (if it's not already)
data$Close <- as.numeric(as.character(data$Close))

# Calculate MACD
macd_results <- MACD(x = data$Close)
print(macd_results)  # This will show the output structure
print(class(macd_results))  # Check the class/type of macd_results
data$MACD <- macd_results[, 1]   
data$Signal <- macd_results[, 2]   








# Calculate Williams
calculate_WPR <- function(high, low, close, n) {
  highest_high <- rollapply(high, n, max, fill = NA, align = 'right')
  lowest_low <- rollapply(low, n, min, fill = NA, align = 'right')
  wpr <- ((highest_high - close) / (highest_high - lowest_low)) * -100
  return(wpr)
}

# 
library(zoo)  # for rollapply

data$WPR <- calculate_WPR(data$High, data$Low, data$Close, 12)


# Calculate Volatility

calculate_volatility <- function(series, n) {
  # Calculate the percentage change
  pct_change <- diff(log(series)) * 100
  
  # Calculate the rolling standard deviation (volatility)
  vol <- rollapply(pct_change, width = n, FUN = sd, fill = NA, align = 'right')
  
  return(vol)
}

#data <- data[, !names(data) %in% c("Volume", "VOL1" ,"VOL2")]

colnames(data)

library(zoo)  # for rollapply

# Specify numeric columns
numeric_columns <- c("Open", "High", "Low", "Close","OfficialVolume")

# Replace commas with periods in numeric columns
#for (col in numeric_columns) {
 # data[[col]] <- gsub(",", ".", data[[col]])
}
# Convert 'Close' and 'Volume' to numeric 
data$Close <- as.numeric(as.character(data$Close))
data$Volume <- as.numeric(as.character(data$Volume))


# Prepend NA to the volatility vectors
vol1_values <- c(NA, calculate_volatility(data$Close, 10))
vol2_values <- c(NA, calculate_volatility(data$Volume, 10))

# Now assign the values to the data table
data$VOL1 <- vol1_values
data$VOL2 <- vol2_values

# Calculate Differential Technical Indicators (ΔMA5: the change in MA5 from one period to the next)
# Add NA to the beginning to align lengths
data$Delta_MA5 <- c(NA, diff(data$MA5, lag = 1))


colnames(data)


# Corrected list of columns to check for NAs
columns_to_check <- c("MA5", "MA10", "MA20", "BIAS5", "BIAS10", "RSI6", "RSI12", "Stoch_K", "Stoch_D", "MACD", "Signal", "WPR", "VOL1", "VOL2", "Delta_MA5")

# Subset the dataframe to keep only rows that are complete cases in the specified columns
data <- data[complete.cases(data[, columns_to_check]), ]

# Check the structure of the cleaned data
str(data)

unique(data$CompanyId)
summary(data)
typeof(data$Date)



# Defining the columns to keep
# Updated list of columns to keep
columns_to_keep <- c("Date", "CompanyId", "Close","SMB", "HML", "LIQ", "MOM", "MA5", "MA10", "MA20", "BIAS5", "BIAS10", "RSI6", "RSI12", 
                     "Stoch_K", "Stoch_D", "MACD", "Signal", "WPR", "VOL1", "VOL2", "Delta_MA5" )

library(dplyr)

# Subset the dataframe to include only specified columns
data <- select(data, all_of(columns_to_keep))

# Optionally, check the structure of the new subset
str(data)


# Save data frame to a CSV file
write.csv(data, "titlonmoyinreadyfinalsubset.csv", row.names = FALSE)
#data <- read.csv("titlonmoyinreadyfinal.csv")



############################################################################ modeling feature extraction using CNN################


data <- read.csv("titlonmoyinreadyfinalsubset 17.5.2024.csv")

# Load necessary libraries
library(quantmod)
library(keras)





library(data.table)
library(keras)
library(tensorflow)

# Remove rows with any NA values from cnn_output dataframe
data <- na.omit(data)





# Select the columns to be used as features
features <- data[, c("Close","SMB", "HML", "LIQ", "MOM", "MA5", "MA10", "MA20", "BIAS5", "BIAS10", "RSI6", "RSI12", 
                     "Stoch_K", "Stoch_D", "MACD", "Signal", "WPR", "VOL1", "VOL2", "Delta_MA5")]

# Normalize the features
# We will perform min-max scaling here. You can also use other methods like Z-score standardization
min_vals <- sapply(features, min, na.rm = TRUE)
max_vals <- sapply(features, max, na.rm = TRUE)
scaled_features <- as.data.table(scale(features, center = min_vals, scale = max_vals - min_vals))

# Prepare the data for the CNN
#  use a sequence length of 'n' days for each prediction
sequence_length <- 10 #   using the past 10 days to predict the next day
n_features <- ncol(features)
n_samples <- nrow(features) - sequence_length + 1

# Initialize an array to hold the reshaped data
cnn_input <- array(NA, dim = c(n_samples, sequence_length, n_features))
dates <- data$Date[(11:(nrow(data) + 1))]




# Reshape the data into a 3D array
for(i in 1:n_samples) {
  cnn_input[i,,] <- as.matrix(scaled_features[i:(i + sequence_length - 1),])
}

# Now, cnn_input is ready to be used as an input for the CNN
# It has the shape: (number of samples, sequence length, number of features)
print(dim(cnn_input))




# Prepare the output data (target variable)
# Shift the closing price by one time step to create the target output
cnn_output <- data$Close[(sequence_length + 1):nrow(data)]

# Make sure the length of cnn_output matches the number of samples in cnn_input
# The last value of 'Close' will not have a corresponding future value to predict, so we remove it
cnn_output <- cnn_output[1:n_samples]

# If the CNN model's output layer has one neuron, ensure the target is a matrix with one column
cnn_output <- matrix(cnn_output, ncol = 1)
print(length(cnn_output))  # Should match n_samples





# Convert the list element 'Output' into a dataframe
cnn_output <- data.frame(cnn_output)

# Now add the 'dates' vector as a new column to the dataframe
cnn_output$Date <- dates



### check data 
# Apply is.na() and is.infinite() to each element in the data frame
na_or_inf <- sapply(data, function(x) is.na(x) | is.infinite(x))

# Count the number of rows that have any NA or Inf
count_rows_with_na_or_inf <- sum(rowSums(na_or_inf) > 0)

# Print the result
print(paste("Number of rows with NA or Inf:", count_rows_with_na_or_inf))


# Check for NA, NaN, or Inf in each column
columns_with_issues <- sapply(data, function(x) any(is.na(x) | is.infinite(x)))

# Get names of columns with any NA, NaN, or Inf
columns_names_with_issues <- names(data)[columns_with_issues]

# Print the result
print(columns_names_with_issues)


# Identified columns with issues
columns_with_issues <- c("CompanyId", "SMB", "HML", "MOM")

# Count NA, NaN, or Inf in each identified column
count_issues_per_column <- sapply(data[columns_with_issues], function(x) sum(is.na(x) | is.infinite(x)))

# Print the counts
print(count_issues_per_column)











# Function to remove rows with any NaN values
remove_NaN_rows <- function(input, output) {
  nan_rows_input <- apply(is.na(input), c(1), any)
  nan_rows_output <- is.na(output)
  nan_rows <- nan_rows_input | nan_rows_output
  
  input_clean <- input[!nan_rows, , ]
  output_clean <- output[!nan_rows]
  
  return(list("input_clean" = input_clean, "output_clean" = output_clean))
}

# Remove NaN values from cnn_input and cnn_output
#clean_data <- remove_NaN_rows(cnn_input, cnn_output)
#cnn_input <- clean_data$input_clean
#cnn_output <- clean_data$output_clean







# Function to check for NaN, Inf, or -Inf values in an array
check_for_bad_values <- function(arr) {
  nan_count <- sum(is.na(arr))
  inf_count <- sum(arr == Inf)
  neg_inf_count <- sum(arr == -Inf)
  
  return(list("NaN Count" = nan_count, "Inf Count" = inf_count, "-Inf Count" = neg_inf_count))
}


cnn_output <- na.omit(cnn_output)


# Check for bad values in cnn_input
input_check <- check_for_bad_values(cnn_input)
print(input_check)

# Check for bad values in cnn_output
output_check <- check_for_bad_values(cnn_output)
print(output_check)


cnn_input<- cnn_input[1:1336810, , ]



# Now cnn_output is ready to be used as the target for training the CNN

# Save cnn_input

save(cnn_input, file = "cnn_input17.5.2024.RData")
save(cnn_output, file = "cnn_output17.5.2024.RData")

# Load cnn_input
load("cnn_input17.5.2024.RData")
load("cnn_output17.5.2024.RData")

###model running testing 



# Load cnn_input
load("cnn_input17.5.2024.RData")
load("cnn_output17.5.2024.RData")
summary(cnn_input)
data <- read.csv("titlonmoyinreadyfinalsubset 17.5.2024.csv")

features <- data[, c("Close","SMB", "HML", "LIQ", "MOM", "MA5", "MA10", "MA20", "BIAS5", "BIAS10", "RSI6", "RSI12", 
                     "Stoch_K", "Stoch_D", "MACD", "Signal", "WPR", "VOL1", "VOL2", "Delta_MA5")]
# Prepare the data for the CNN
#  use a sequence length of 'n' days for each prediction
sequence_length <- 10 #   using the past 10 days to predict the next day
n_features <- ncol(features)
n_samples <- nrow(features) - sequence_length + 1

library(keras)
library(tidyverse)
# Specify TensorFlow version
#install_keras(tensorflow = "2.16.1")



# Load necessary libraries
library(quantmod)
library(keras)
library(data.table)
library(tensorflow)
#install.packages("tensorflo
library(keras)

# Adding 1D Convolutional layers with different kernel sizes as per the paper
# Note that 'input_shape' is set according to your input data's dimensions (sequence_length, n_features)


# Define the model
model <- keras_model_sequential()

# Input shape is based on your data: time steps = 20, features = 19
#input_shape <- c(20, 19)

 


# Define the model
#model1 <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(sequence_length, n_features), padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2, strides = 2) %>%
  layer_conv_1d(filters = 64, kernel_size = 3, activation = 'relu', padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2, strides = 2) %>%
  layer_flatten()



  model2 <- keras_model_sequential() %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 3,
    activation = 'relu',
    input_shape = c(sequence_length, n_features),
    padding = "same",
    kernel_regularizer = regularizer_l2(0.01),  # Adding L2 regularization
    kernel_initializer = initializer_glorot_uniform()  # Changing the initializer
  ) %>%
  layer_max_pooling_1d(pool_size = 2, strides = 2) %>%
  layer_conv_1d(
    filters = 64,
    kernel_size = 3,
    activation = 'relu',
    padding = "same",
    kernel_regularizer = regularizer_l2(0.01),  # Adding L2 regularization to another layer
    kernel_initializer = initializer_glorot_uniform()  # Applying initializer here as well
  ) %>%
  layer_max_pooling_1d(pool_size = 2, strides = 2) %>%
  layer_flatten()







model2 %>% compile(
  loss = 'mse',  # Mean Squared Error for regression tasks
  optimizer = optimizer_adam(learning_rate = 0.00005, clipnorm = 1),## learnin rate nesf kardam 0.0001 bood 
  metrics = c('mean_absolute_error', 'accuracy')  # Combining metrics into one vector
)

# Print the model summary
summary(model2)

sum(is.infinite(cnn_input))
sum(is.infinite(cnn_output))




# Train the model
history <- model2 %>% fit(
  x = cnn_input,
  y = cnn_output$cnn_output, 
  epochs = 5,
  batch_size = 32,
  validation_split = 0.2
)




# Assuming 'model2' is   fully compiled and trained model
# Create a feature extraction model that outputs from the second convolutional layer

feature_extractor <- keras_model(inputs = model2$input, outputs = get_layer(model2, index = 2)$output)

# Now, use this model to predict features
extracted_features <- predict(feature_extractor, cnn_input)




# Saving the extracted features for later use
saveRDS(extracted_features, file = "extracted_features_medel2.19.5.2024fama frenchfinalgetbetter pca.rds")


#################################################################################################new models feeding features 
# Load the RDS file
extracted_features <- readRDS("extracted_features_medel2.19.5.2024fama frenchfinalgetbetter pca.rds")




# View the dimensions of the loaded features
dim(extracted_features)

# Check the summary statistics for the features
summary(extracted_features)

# If you want to see the first few rows to understand what the features look like
head(extracted_features)





######PCA TEST ###########




# Flatten the array into a 2D matrix
flattened_data <- array(extracted_features, dim = c(dim(extracted_features)[1], dim(extracted_features)[2] * dim(extracted_features)[3]))

summary(flattened_data)## dataha too ranega an scaleled ok 


# Step 1: Identify columns that contain at least one zero
columns_with_zeros <- apply(flattened_data, 2, function(x) any(x == 0))

# Step 2: Filter out these columns from the matrix
#flattened_data <- flattened_data[, !columns_with_zeros]

# Show dimensions or summary of the filtered data
#print(dim(filtered_data))
#summary(filtered_data)



library(stats)

#
pca <- prcomp(flattened_data, scale. = TRUE, center = TRUE)

# Extracting the importance of components
explained_variance <- summary(pca)$importance[2,]
cumulative_variance <- cumsum(explained_variance)
num_components <- which(cumulative_variance >= 0.995)[1]

# Using the number of components to reduce dimensions
reduced_data <- pca$x[, 1:num_components]

summary(reduced_data)

# Combine df1 and df2 side by side
combined_df <- cbind(cnn_output, reduced_data)
data <- rename(combined_df, c("close"= "cnn_output"  ))
combined_df <- rename(combined_df, c("close"= "cnn_output"  ))

# Display the combined data frame
print(combined_df)


write.csv(combined_df, file = "final.feature.feed_to_other_models19.05.2024better pca +++.csv", row.names = FALSE, na = "NA")


save(reduced_data, file = "datatabnetwithpcafinal+19.5.2024.RData")


######################################################TABNET 


# load("datatabnetwithpcafinal+17.5.2024.RData")
data <- read.csv("final.feature.feed_to_other_models19.05.2024better pca +++.csv", header = TRUE, sep = ",")
#write.csv(data, file = "final.feature.feed_to_other_models17.05.2024+++.csv", row.names = FALSE, na = "NA")

#data <- rename(data, c("close"= "cnn_output"  ))

summary(data)## niyqazi be scale e pca ha dyded nashod oon gahdry tafavot meghdary nadaran 


# Specify the date of interest
target_date <- as.Date("2021-01-01")

# Ensure the date column is in Date format
data$Date <- as.Date(data$Date)

# Count rows before and after the target date
rows_before <- nrow(data[data$Date < target_date, ])
rows_after <- nrow(data[data$Date > target_date, ])

# Total number of rows
total_rows <- nrow(data)

# Calculate percentages
percentage_before <- (rows_before / total_rows) * 100
percentage_after <- (rows_after / total_rows) * 100

# Output the results
cat("Rows before", target_date, ":", rows_before, "\n")
cat("Rows after", target_date, ":", rows_after, "\n")
cat("Percentage before", target_date, ":", round(percentage_before, 2), "%\n")
cat("Percentage after", target_date, ":", round(percentage_after, 2), "%\n")






#data <- reduced_data
#install.packages("tabnet")
#install.packages("data.table")  # For data manipulation
#install.packages("Metrics")     # For model evaluation

# install.packages("remotes")
#remotes::install_github("mlverse/tabnet")
library(tabnet)
library(torch)
library(tabnet)
library(data.table)
library(Metrics)

# 'reduced_data' is your input features and 'cnn_output' is  target variable
#data <- data.frame(reduced_data, cnn_output)

#save(data, file = "datatabnetwithpca.RData")

# Split data into training and testing sets


library(tabnet)
library(data.table)
library(Metrics)



library(tabnet)  # Ensure the library is loaded

# Check the documentation to make sure of parameter names and expected data formats
?tabnet_fit
library(tabnet)
library(data.table)
library(Metrics)

# Load   data and ensure it's properly formatted
#cnn_output <- cnn_output
#data$cnn_output <- cnn_output
str(data)







# Load necessary packages
library(dplyr)
library(tabnet)


# Ensure the date column is in Date format
data$Date <- as.Date(data$Date)

# Specify the date of interest
target_date <- as.Date("2021-01-01")

# Split data based on the target date
train_data <- data %>% filter(Date < target_date)
test_data <- data %>% filter(Date > target_date)

# Prepare features and target
train_features <- train_data[, 3:11]
train_target <- train_data[, 1]

test_features <- test_data[, 3:11]
test_target <- test_data[, 1]

# Convert target to appropriate format   (numeric vector or matrix)
if (is.data.frame(train_target)) {
  train_target <- unlist(train_target)  # Convert data frame column to vector  
}
if (is.data.frame(test_target)) {
  test_target <- unlist(test_target)  # Convert data frame column to vector  
}

# Configuration for TabNet
config <- tabnet_config(
  epochs = 5,
  batch_size = 256,
  virtual_batch_size = 128,
  
)






# Enhanced Configuration for TabNet
#config <- tabnet_config(
  epochs = 5,  
  batch_size = 1024,  # Adjust batch size
  virtual_batch_size = 128,
  num_steps = 10,  # Increase the number of steps in the network
  gamma = 1.5,  # Adjust gamma for feature reusage
  lambda_sparse = 1e-4,  # Add sparsity regularization
  optimizer = list(
    type = "adam",
    lr = 0.01,
    weight_decay = 1e-5
  )
)




# Fit the TabNet model
tabnet_model <- tabnet_fit(
  x = train_features,
  y = train_target,
  config = config
)




# Save the trained model if needed
save(tabnet_model, file = "tabnet_model17.5.2024.RData")
# load("tabnet_model.RData")

# Making predictions on the test set
test_predictions <- predict(tabnet_model, test_features)

# Extract the numeric vector from test_predictions
pred_values <- test_predictions$.pred

# Calculate test RMSE
test_rmse <- sqrt(mean((pred_values - test_target)^2))
cat("Test RMSE:", test_rmse, "\n")

# Calculating residuals
residuals <- pred_values - test_target

# Create residual plot
plot(pred_values, residuals, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)  # Add horizontal line at y = 0 (residuals = 0)





########################################## qadimi -----------------------------

# Prepare data:    
features <- data[, 3:11]
target <- data[, 1]

# Convert target to appropriate format if necessary ( numeric vector or matrix)
if (is.data.frame(target)) {
  target <- unlist(target)  # Convert data frame column to vector if necessary
}

# Split data into training and testing sets
set.seed(123)  # for reproducibility
indexes <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_features <- features[indexes,]
train_target <- target[indexes]
test_features <- features[-indexes,]
test_target <- target[-indexes]

# Configuration for TabNet
config <- tabnet_config(
  epochs = 5,
  batch_size = 256,
  virtual_batch_size = 128,
  valid_split = 0.2  # 20% of training data used for validation
)

# Fit the TabNet model
tabnet_model <- tabnet_fit(
  x = train_features,
  y = train_target,
  config = config
)

#save(tabnet_model, file = "tabnet_model.RData")
#load("tabnet_model.RData")


# Making predictions on the test set
test_predictions <- predict(tabnet_model, test_features)
#save(test_predictions, file = "tabnettest_predictions.RData")

# Calculating RMSE for test predictions
test_rmse <- sqrt(mean((test_predictions - test_target)^2))
class(test_predictions)
class(test_features)
str(test_predictions)
str(test_target)


# loaded the dplyr package
library(dplyr)

# Extract the numeric vector from test_predictions
pred_values <- test_predictions$.pred

# Calculate test_rmse
test_rmse <- sqrt(mean((pred_values - test_target)^2))



print(paste("Test RMSE:", test_rmse))



# Calculating residuals
residuals <- test_predictions$.pred - test_target

# Create residual plot
plot(test_predictions$.pred, residuals, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)  # Add horizontal line at y = 0 (residuals = 0)








#################################  pca histogrms




# Load necessary libraries
library(ggplot2)
library(gridExtra)
#install.packages("gridExtra")
# Load your data
# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

# Load your data
#data <- read.csv("final.feature.feed_to_other_models17.05.2024+++.csv")

# List of PCA components
pca_columns <- c( "close", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
pca_columns <- c( "close")

# Create a list to hold the plots
plots <- list()

# Generate histograms using tidy evaluation
for (pca in pca_columns) {
  p <- ggplot(data, aes(x = .data[[pca]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = paste("Distribution of", pca), x = pca, y = "Density")
  plots[[pca]] <- p
}

# Arrange the plots in a grid
do.call(grid.arrange, c(plots, ncol = 3))

summary(data)

#install.packages("DescTools")
# Load necessary libraries
library(DescTools)
library(ggplot2)
library(gridExtra)

# Load your data
#data <- read.csv("final.feature.feed_to_other_models17.05.2024+++.csv")

# List of PCA components
pca_columns <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")

# Apply winsorization to each PCA component
for (pca in pca_columns) {
  data[[pca]] <- Winsorize(data[[pca]], probs = c(0.05, 0.95))
}

# Create a list to hold the plots
plots <- list()

# Generate histograms using tidy evaluation
for (pca in pca_columns) {
  p <- ggplot(data, aes(x = .data[[pca]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = paste("Distribution of", pca), x = pca, y = "Density")
  plots[[pca]] <- p
}

# Arrange the plots in a grid
do.call(grid.arrange, c(plots, ncol = 3))




# Load necessary libraries
library(DescTools)
library(dplyr)

# Load your data
#data <- read.csv("final.feature.feed_to_other_models17.05.2024+++.csv")

# Ensure the date column is in datetime format
data$Date <- as.Date(data$Date)

# List of PCA components and other features to winsorize
pca_columns <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")

# Store the number of rows before winsorization
num_rows_before <- nrow(data)

# Apply winsorization to each PCA component
for (pca in pca_columns) {
  data[[pca]] <- Winsorize(data[[pca]], probs = c(0.05, 0.95))
}

# Store the number of rows after winsorization
num_rows_after <- nrow(data)

# Print the number of rows before and after winsorization to verify they are the same
cat("Number of rows before winsorization:", num_rows_before, "\n")
cat("Number of rows after winsorization:", num_rows_after, "\n")




# Function to check for NA, NaN, and infinite values in each column
check_missing_values <- function(df) {
  sapply(df, function(x) {
    list(
      `NA` = sum(is.na(x)),
      `NaN` = sum(is.nan(x)),
      `PosInf` = sum(is.infinite(x) & x > 0),
      `NegInf` = sum(is.infinite(x) & x < 0)
    )
  })
}

# Check for missing values in the dataset
missing_values_summary <- check_missing_values(data)
print(missing_values_summary)


# Save the updated data frame if needed
write.csv(data, "winsorized_datapca final 18.5.2024.csv", row.names = FALSE)

# Check the structure of the updated data
str(data)


########### close prices assessing 

# Load necessary libraries
library(dplyr)


# Define the ranges
ranges <- list(
  "0-50" = c(0, 50),
  "50-100" = c(50, 100),
  "100-500" = c(100, 500),
  "500-1000" = c(500, 1000),
  "1000+" = c(1000, Inf)
)

# Function to calculate percentage of values in each range
calculate_percentage <- function(data, column, ranges) {
  total_count <- nrow(data)
  percentages <- sapply(ranges, function(range) {
    count <- sum(data[[column]] >= range[1] & data[[column]] < range[2])
    percentage <- (count / total_count) * 100
    return(percentage)
  })
  return(percentages)
}

# Calculate percentages for 'close' prices
percentages <- calculate_percentage(data, "close", ranges)

# Print the results
percentages



# Load necessary libraries
library(ggplot2)

# Define the ranges and percentages
ranges <- c("0-50", "50-100", "100-500", "500-1000", "1000+")
percentages <- c(62.02, 16.38, 20.46, 0.70, 0.44)

# Create a data frame for plotting
percentage_data <- data.frame(
  Range = factor(ranges, levels = ranges),
  Percentage = percentages
)

# Plot the histogram
ggplot(percentage_data, aes(x = Range, y = Percentage)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_text(aes(label = round(Percentage, 2)), vjust = -0.5, size = 4) +
  labs(title = "Percentage of Close Prices in Different Ranges",
       x = "Range",
       y = "Percentage") +
  theme_minimal()



library(dplyr)
library(DescTools)

# Winsorize the 'close' column
data$close <- Winsorize(data$close, probs = c(0.05, 0.95))

# Check the summary of the winsorized 'close' column
summary(data$close)

# Plot distribution of winsorized 'close' prices
ggplot(data, aes(x = close)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Winsorized Close Prices", x = "Winsorized Close", y = "Density")

 
 
# Save the updated data frame if needed
write.csv(data, "winsorized_datapca and clsoe prices final 18.5.2024.csv", row.names = FALSE)




#####################tabnet scaled pytorch tabnet 20.05.2024----------------






# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")
if (!require("caret")) install.packages("caret")
if (!require("tabnet")) devtools::install_github("mlverse/tabnet")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(caret)
library(tabnet)
library(ggplot2)

# Function to create lagged features with a window size
create_lagged_features <- function(data, lag_days=5) {
  lagged_features <- data %>%
    mutate(across(c("close", starts_with("PC")), 
                  list(lag_1 = ~ lag(.x, 1), lag_2 = ~ lag(.x, 2), lag_3 = ~ lag(.x, 3), 
                       lag_4 = ~ lag(.x, 4), lag_5 = ~ lag(.x, 5)), 
                  .names = "{.col}_{.fn}")) %>%
    drop_na()
  return(lagged_features)
}

# Load the CSV file
file_path <- 'final.feature.feed_to_other_models19.05.2024better pca +++.csv'
df <- read_csv(file_path)
df$Date <- as.Date(df$Date)

# Split data based on the target date
target_date <- as.Date("2021-01-01")
train_data <- df %>% filter(Date < target_date)
test_data <- df %>% filter(Date > target_date)

# Create lagged features with a 5-day window size
train_data <- create_lagged_features(train_data, lag_days = 5)
test_data <- create_lagged_features(test_data, lag_days = 5)

# Verify lagged features creation
cat("Train Data Columns After Lagging:\n", colnames(train_data), "\n")
cat("Test Data Columns After Lagging:\n", colnames(test_data), "\n")

# Prepare features and target
train_features <- train_data %>% select(-close, -Date)
train_target <- train_data$close
test_features <- test_data %>% select(-close, -Date)
test_target <- test_data$close

# Verify the columns in train_features and test_features
cat("Train Features Columns Before Scaling:\n", colnames(train_features), "\n")
cat("Test Features Columns Before Scaling:\n", colnames(test_features), "\n")

# Scale the features and targets using MinMaxScaler
scaler_features <- preProcess(train_features, method = "range")
train_features_scaled <- predict(scaler_features, train_features)
test_features_scaled <- predict(scaler_features, test_features)

scaler_target <- preProcess(as.data.frame(train_target), method = "range")
train_target_scaled <- predict(scaler_target, as.data.frame(train_target))
test_target_scaled <- predict(scaler_target, as.data.frame(test_target))

# Convert scaled targets to numeric vectors
train_target_scaled <- as.numeric(train_target_scaled$close)
test_target_scaled <- as.numeric(test_target_scaled$close)

# Convert data to data frame
train_features_scaled <- as.data.frame(train_features_scaled)
test_features_scaled <- as.data.frame(test_features_scaled)

# Verify the shapes
cat("Train Features Shape:", dim(train_features_scaled), "\n")
cat("Train Target Shape:", length(train_target_scaled), "\n")
cat("Test Features Shape:", dim(test_features_scaled), "\n")
cat("Test Target Shape:", length(test_target_scaled), "\n")

# Verify column names
cat("Train Features Columns After Scaling:\n", colnames(train_features_scaled), "\n")
cat("Test Features Columns After Scaling:\n", colnames(test_features_scaled), "\n")

# Train the TabNet model
tabnet_model <- tabnet_fit(train_features_scaled, train_target_scaled, epochs = 5, valid_split = 0.2)

# Predict using the model
predictions_scaled <- predict(tabnet_model, test_features_scaled)

# Inverse transform the predictions to original scale
predictions <- predict(scaler_target, as.data.frame(predictions_scaled))

# Calculate RMSE for real data
rmse_real <- sqrt(mean((test_target - predictions$close)^2))
print(paste("RMSE on real data:", rmse_real))

# Print the first 200 predictions along with the corresponding test target values
print("First 200 Predictions and Respective Test Target Values:")
for (i in 1:200) {
  print(paste("Predicted:", predictions$close[i], "Actual:", test_target[i]))
}

# Calculate residuals
residuals <- test_target - predictions$close

# Plot residuals
ggplot(data.frame(Predicted = predictions$close, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point() +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs Predicted Values") +
  theme_minimal()

# Extract training and validation losses
train_losses <- tabnet_model$history$train_loss
val_losses <- tabnet_model$history$valid_loss

# Plot the training and validation loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training and Validation Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red")) +
  theme_minimal()

# Extract test loss from validation loss if you want to treat it as test loss
test_losses <- val_losses

# Plot the training, validation, and test loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses, TestLoss = test_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  geom_line(aes(y = TestLoss, color = "Test Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training, Validation, and Test Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red", "Test Loss" = "green")) +
  theme_minimal()





























# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")
if (!require("caret")) install.packages("caret")
if (!require("tabnet")) devtools::install_github("mlverse/tabnet")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(caret)
library(tabnet)
library(ggplot2)

# Function to create lagged features with a window size
create_lagged_features <- function(data, lag_days=5) {
  lagged_features <- data %>%
    mutate(across(c("close", starts_with("PC")), 
                  list(lag_1 = ~ lag(.x, 1), lag_2 = ~ lag(.x, 2), lag_3 = ~ lag(.x, 3), 
                       lag_4 = ~ lag(.x, 4), lag_5 = ~ lag(.x, 5)), 
                  .names = "{.col}_{.fn}")) %>%
    drop_na()
  return(lagged_features)
}

# Load the CSV file
file_path <- 'final.feature.feed_to_other_models19.05.2024better pca +++.csv'
df <- read_csv(file_path)
df$Date <- as.Date(df$Date)

# Split data based on the target date
target_date <- as.Date("2021-01-01")
train_data <- df %>% filter(Date < target_date)
test_data <- df %>% filter(Date > target_date)

# Create lagged features with a 5-day window size
train_data <- create_lagged_features(train_data, lag_days = 5)
test_data <- create_lagged_features(test_data, lag_days = 5)

# Verify lagged features creation
cat("Train Data Columns After Lagging:\n", colnames(train_data), "\n")
cat("Test Data Columns After Lagging:\n", colnames(test_data), "\n")

# Prepare features and target
train_features <- train_data %>% select(-close, -Date)
train_target <- train_data$close
test_features <- test_data %>% select(-close, -Date)
test_target <- test_data$close

# Verify the columns in train_features and test_features
cat("Train Features Columns Before Scaling:\n", colnames(train_features), "\n")
cat("Test Features Columns Before Scaling:\n", colnames(test_features), "\n")

# Scale the features and targets using MinMaxScaler
scaler_features <- preProcess(train_features, method = "range")
train_features_scaled <- predict(scaler_features, train_features)
test_features_scaled <- predict(scaler_features, test_features)

scaler_target <- preProcess(as.data.frame(train_target), method = "range")
train_target_scaled <- predict(scaler_target, as.data.frame(train_target))
test_target_scaled <- predict(scaler_target, as.data.frame(test_target))

# Convert scaled targets to numeric vectors
train_target_scaled <- as.numeric(train_target_scaled$close)
test_target_scaled <- as.numeric(test_target_scaled$close)

# Convert data to data frame
train_features_scaled <- as.data.frame(train_features_scaled)
test_features_scaled <- as.data.frame(test_features_scaled)

# Verify the shapes
cat("Train Features Shape:", dim(train_features_scaled), "\n")
cat("Train Target Shape:", length(train_target_scaled), "\n")
cat("Test Features Shape:", dim(test_features_scaled), "\n")
cat("Test Target Shape:", length(test_target_scaled), "\n")

# Verify column names
cat("Train Features Columns After Scaling:\n", colnames(train_features_scaled), "\n")
cat("Test Features Columns After Scaling:\n", colnames(test_features_scaled), "\n")

# Train the TabNet model
tabnet_model <- tabnet_fit(train_features_scaled, train_target_scaled, epochs = 5, valid_split = 0.2)

# Predict using the model
predictions_scaled <- predict(tabnet_model, test_features_scaled)

# Inverse transform the predictions to original scale
predictions <- predict(scaler_target, as.data.frame(predictions_scaled))

# Calculate RMSE for real data
rmse_real <- sqrt(mean((test_target - predictions$close)^2))
print(paste("RMSE on real data:", rmse_real))

# Print the first 200 predictions along with the corresponding test target values
print("First 200 Predictions and Respective Test Target Values:")
for (i in 1:200) {
  print(paste("Predicted:", predictions$close[i], "Actual:", test_target[i]))
}

# Calculate residuals
residuals <- test_target - predictions$close

# Plot residuals
ggplot(data.frame(Predicted = predictions$close, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point() +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs Predicted Values") +
  theme_minimal()

# Extract training and validation losses
train_losses <- tabnet_model$history$train_loss
val_losses <- tabnet_model$history$valid_loss

# Plot the training and validation loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training and Validation Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red")) +
  theme_minimal()

# Extract test loss from validation loss if you want to treat it as test loss
test_losses <- val_losses

# Plot the training, validation, and test loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses, TestLoss = test_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  geom_line(aes(y = TestLoss, color = "Test Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training, Validation, and Test Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red", "Test Loss" = "green")) +
  theme_minimal()




























# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")
if (!require("caret")) install.packages("caret")
if (!require("tabnet")) devtools::install_github("mlverse/tabnet")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(caret)
library(tabnet)
library(ggplot2)

# Function to create lagged features with a window size
create_lagged_features <- function(data, lag_days=5) {
  lagged_features <- data %>%
    mutate(across(c("close", starts_with("PC")), 
                  list(lag_1 = ~ lag(.x, 1), lag_2 = ~ lag(.x, 2), lag_3 = ~ lag(.x, 3), 
                       lag_4 = ~ lag(.x, 4), lag_5 = ~ lag(.x, 5)), 
                  .names = "{.col}_{.fn}")) %>%
    drop_na()
  return(lagged_features)
}

# Load the CSV file
file_path <- 'final.feature.feed_to_other_models19.05.2024better pca +++.csv'
df <- read_csv(file_path)
df$Date <- as.Date(df$Date)

# Split data based on the target date
target_date <- as.Date("2021-01-01")
train_data <- df %>% filter(Date < target_date)
test_data <- df %>% filter(Date > target_date)

# Create lagged features with a 5-day window size
train_data <- create_lagged_features(train_data, lag_days = 5)
test_data <- create_lagged_features(test_data, lag_days = 5)

# Verify lagged features creation
cat("Train Data Columns After Lagging:\n", colnames(train_data), "\n")
cat("Test Data Columns After Lagging:\n", colnames(test_data), "\n")

# Prepare features and target
train_features <- train_data %>% select(-close)
train_target <- train_data$close
test_features <- test_data %>% select(-close)
test_target <- test_data$close

# Verify the columns in train_features and test_features
cat("Train Features Columns Before Scaling:\n", colnames(train_features), "\n")
cat("Test Features Columns Before Scaling:\n", colnames(test_features), "\n")

# Scale the features and targets using MinMaxScaler
scaler_features <- preProcess(train_features, method = "range")
train_features_scaled <- predict(scaler_features, train_features)
test_features_scaled <- predict(scaler_features, test_features)

scaler_target <- preProcess(as.data.frame(train_target), method = "range")
train_target_scaled <- predict(scaler_target, as.data.frame(train_target))
test_target_scaled <- predict(scaler_target, as.data.frame(test_target))

# Convert scaled targets to numeric vectors
train_target_scaled <- as.numeric(train_target_scaled$close)
test_target_scaled <- as.numeric(test_target_scaled$close)

# Convert data to data frame
train_features_scaled <- as.data.frame(train_features_scaled)
test_features_scaled <- as.data.frame(test_features_scaled)

# Verify the shapes
cat("Train Features Shape:", dim(train_features_scaled), "\n")
cat("Train Target Shape:", length(train_target_scaled), "\n")
cat("Test Features Shape:", dim(test_features_scaled), "\n")
cat("Test Target Shape:", length(test_target_scaled), "\n")

# Verify column names
cat("Train Features Columns After Scaling:\n", colnames(train_features_scaled), "\n")
cat("Test Features Columns After Scaling:\n", colnames(test_features_scaled), "\n")





























# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")
if (!require("caret")) install.packages("caret")
if (!require("tabnet")) devtools::install_github("mlverse/tabnet")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(caret)
library(tabnet)
library(ggplot2)

# Function to create lagged features with a window size
create_lagged_features <- function(data, lag_days=5) {
  lagged_features <- data %>%
    mutate(across(c("close", starts_with("PC")), 
                  list(lag_1 = ~ lag(.x, 1), lag_2 = ~ lag(.x, 2), lag_3 = ~ lag(.x, 3), 
                       lag_4 = ~ lag(.x, 4), lag_5 = ~ lag(.x, 5)), 
                  .names = "{.col}_{.fn}")) %>%
    drop_na()
  return(lagged_features)
}

# Load the CSV file
file_path <- 'final.feature.feed_to_other_models19.05.2024better pca +++.csv'
df <- read_csv(file_path)
df$Date <- as.Date(df$Date)

# Split data based on the target date
target_date <- as.Date("2021-01-01")
train_data <- df %>% filter(Date < target_date)
test_data <- df %>% filter(Date > target_date)

# Create lagged features with a 5-day window size
train_data <- create_lagged_features(train_data, lag_days = 5)
test_data <- create_lagged_features(test_data, lag_days = 5)

# Prepare features and target
train_features <- train_data %>% select(-close)
train_target <- train_data$close
test_features <- test_data %>% select(-close)
test_target <- test_data$close

# Scale the features and targets using MinMaxScaler
scaler_features <- preProcess(train_features, method = "range")
train_features_scaled <- predict(scaler_features, train_features)
test_features_scaled <- predict(scaler_features, test_features)

scaler_target <- preProcess(as.data.frame(train_target), method = "range")
train_target_scaled <- predict(scaler_target, as.data.frame(train_target))
test_target_scaled <- predict(scaler_target, as.data.frame(test_target))

# Convert scaled targets to numeric vectors
train_target_scaled <- as.numeric(train_target_scaled$close)
test_target_scaled <- as.numeric(test_target_scaled$close)

# Convert data to data frame
train_features_scaled <- as.data.frame(train_features_scaled)
test_features_scaled <- as.data.frame(test_features_scaled)

# Verify the shapes
cat("Train Features Shape:", dim(train_features_scaled), "\n")
cat("Train Target Shape:", length(train_target_scaled), "\n")
cat("Test Features Shape:", dim(test_features_scaled), "\n")
cat("Test Target Shape:", length(test_target_scaled), "\n")

# Verify column names
cat("Train Features Columns:\n", colnames(train_features_scaled), "\n")
cat("Test Features Columns:\n", colnames(test_features_scaled), "\n")

# Train the TabNet model
tabnet_model <- tabnet_fit(train_features_scaled, train_target_scaled, epochs = 5, valid_split = 0.2)

# Predict using the model
predictions_scaled <- predict(tabnet_model, test_features_scaled)

# Inverse transform the predictions to original scale
predictions <- predict(scaler_target, as.data.frame(predictions_scaled))

# Calculate RMSE for real data
rmse_real <- sqrt(mean((test_target - predictions$close)^2))
print(paste("RMSE on real data:", rmse_real))

# Print the first 200 predictions along with the corresponding test target values
print("First 200 Predictions and Respective Test Target Values:")
for (i in 1:200) {
  print(paste("Predicted:", predictions$close[i], "Actual:", test_target[i]))
}

# Calculate residuals
residuals <- test_target - predictions$close

# Plot residuals
ggplot(data.frame(Predicted = predictions$close, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point() +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs Predicted Values") +
  theme_minimal()

# Extract training and validation losses
train_losses <- tabnet_model$history$train_loss
val_losses <- tabnet_model$history$valid_loss

# Plot the training and validation loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training and Validation Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red")) +
  theme_minimal()

# Extract test loss from validation loss if you want to treat it as test loss
test_losses <- val_losses

# Plot the training, validation, and test loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses, TestLoss = test_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  geom_line(aes(y = TestLoss, color = "Test Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training, Validation, and Test Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red", "Test Loss" = "green")) +
  theme_minimal()


































# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")
if (!require("caret")) install.packages("caret")
if (!require("tabnet")) devtools::install_github("mlverse/tabnet")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(caret)
library(tabnet)
library(ggplot2)

# Function to create lagged features with a window size
create_lagged_features <- function(data, lag_days=10) {
  lagged_features <- data %>%
    mutate(across(starts_with("PCA"), list(lag_1 = ~ lag(.x, 1), lag_2 = ~ lag(.x, 2), lag_3 = ~ lag(.x, 3), 
                                           lag_4 = ~ lag(.x, 4), lag_5 = ~ lag(.x, 5)), .names = "{.col}_{.fn}")) %>%
    mutate(close_lag_1 = lag(close, 1), close_lag_2 = lag(close, 2), close_lag_3 = lag(close, 3), 
           close_lag_4 = lag(close, 4), close_lag_5 = lag(close, 5)) %>%
    drop_na()
  return(lagged_features)
}

# Load the CSV file
file_path <- 'final.feature.feed_to_other_models19.05.2024better pca +++.csv'
df <- read_csv(file_path)
df$Date <- as.Date(df$Date)

# Split data based on the target date
target_date <- as.Date("2021-01-01")
train_data <- df %>% filter(Date < target_date)
test_data <- df %>% filter(Date > target_date)

# Create lagged features with a 5-day window size
train_data <- create_lagged_features(train_data, lag_days = 5)
test_data <- create_lagged_features(test_data, lag_days = 5)

# Prepare features and target
train_features <- train_data %>% select(-close)
train_target <- train_data$close
test_features <- test_data %>% select(-close)
test_target <- test_data$close

# Scale the features and targets using MinMaxScaler
scaler_features <- preProcess(train_features, method = "range")
train_features_scaled <- predict(scaler_features, train_features)
test_features_scaled <- predict(scaler_features, test_features)

scaler_target <- preProcess(as.data.frame(train_target), method = "range")
train_target_scaled <- predict(scaler_target, as.data.frame(train_target))
test_target_scaled <- predict(scaler_target, as.data.frame(test_target))

# Convert scaled targets to numeric vectors
train_target_scaled <- as.numeric(train_target_scaled$V1)
test_target_scaled <- as.numeric(test_target_scaled$V1)

# Convert data to data frame
train_features_scaled <- as.data.frame(train_features_scaled)
test_features_scaled <- as.data.frame(test_features_scaled)

# Train the TabNet model
tabnet_model <- tabnet_fit(train_features_scaled, train_target_scaled, epochs = 5, valid_split = 0.2)

# Predict using the model
predictions_scaled <- predict(tabnet_model, test_features_scaled)

# Inverse transform the predictions to original scale
predictions <- predict(scaler_target, as.data.frame(predictions_scaled))

# Calculate RMSE for real data
rmse_real <- sqrt(mean((test_target - predictions$V1)^2))
print(paste("RMSE on real data:", rmse_real))

# Print the first 200 predictions along with the corresponding test target values
print("First 200 Predictions and Respective Test Target Values:")
for (i in 1:200) {
  print(paste("Predicted:", predictions$V1[i], "Actual:", test_target[i]))
}

# Calculate residuals
residuals <- test_target - predictions$V1

# Plot residuals
ggplot(data.frame(Predicted = predictions$V1, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point() +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs Predicted Values") +
  theme_minimal()

# Extract training and validation losses
train_losses <- tabnet_model$history$train_loss
val_losses <- tabnet_model$history$valid_loss

# Plot the training and validation loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training and Validation Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red")) +
  theme_minimal()

# Extract test loss from validation loss if you want to treat it as test loss
test_losses <- val_losses

# Plot the training, validation, and test loss
ggplot(data.frame(Epoch = 1:length(train_losses), TrainLoss = train_losses, ValLoss = val_losses, TestLoss = test_losses), aes(x = Epoch)) +
  geom_line(aes(y = TrainLoss, color = "Train Loss")) +
  geom_line(aes(y = ValLoss, color = "Validation Loss")) +
  geom_line(aes(y = TestLoss, color = "Test Loss")) +
  labs(x = "Epoch", y = "Loss", title = "Training, Validation, and Test Loss") +
  scale_color_manual(name = "Loss", values = c("Train Loss" = "blue", "Validation Loss" = "red", "Test Loss" = "green")) +
  theme_minimal()
























#------------------#####################  Ft-transformer >>>>>>>>>>>>>in python 

# Install reticulate 

  #install.packages("reticulate")

library(reticulate)
use_python("/usr/bin/python") # Specify the Python path that has ft-transformer installed

# Assuming you have ft-transformer available in Python, import it
ft_transformer <- import("ft_transformer_package_name") # Replace with the actual Python package name




# Install Torch for R
#install.packages("torch")

# Assuming 'ft_transformer' is a function or package for the FT-Transformer model
install.packages("ft_transformer")




















class(train_features) 
class(train_target) 
class(test_target) 
class(test_features) 

?tabnet_fit



















# Make predictions on the test set
predictions <- predict(tabnet_model, newdata = as.matrix(test_features))

# Evaluate the model
rmse <- rmse(test_target, predictions)
cat(sprintf("Root Mean Square Error on Test Set: %f", rmse))





























load("datatabnetwithpca.RData")

#install.packages("torch")
library(tabnet)
library(torch)
library(recipes)  # Ensure it's installed and loaded
library(dplyr)
set.seed(123)  # For reproducibility
# Assuming 'data' is already loaded with the last column as the target

# Split data into training and testing sets
n <- nrow(data)
train_indices <- sample(seq_len(n), size = 0.8 * n)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


train_features <- torch_tensor(as.matrix(train_data[, -ncol(train_data)]), dtype = torch_float32())
train_targets <- torch_tensor(train_data[, ncol(train_data)], dtype = torch_float32())
test_features <- torch_tensor(as.matrix(test_data[, -ncol(test_data)]), dtype = torch_float32())
test_targets <- torch_tensor(test_data[, ncol(test_data)], dtype = torch_float32())




train_dataset <- tensor_dataset(list(features = train_features, targets = train_targets))
test_dataset <- tensor_dataset(list(features = test_features, targets = test_targets))

train_dl <- dataloader(train_dataset, batch_size = 64, shuffle = TRUE)
test_dl <- dataloader(test_dataset, batch_size = 64)

# Assuming 'train_features' and 'train_targets' are already tensors
train_dataset <- tensor_dataset(list(features = train_features, targets = train_targets))
train_dl <- dataloader(train_dataset, batch_size = 64, shuffle = TRUE)

# Defining the TabNet model
model <- tabnet(
  num_features = ncol(as.matrix(train_data[, -ncol(train_data)])), 
  num_targets = 1,
  feature_dim = 64,
  output_dim = 64
)

# Training loop
optimizer <- optim_adam(model$parameters, lr = 0.02)
num_epochs <- 50

for (epoch in seq_len(num_epochs)) {
  total_loss <- 0
  model$train()
  for (b in enumerate(train_dl)) {
    optimizer$zero_grad()
    output <- model(b$features)
    loss <- nnf_binary_cross_entropy_with_logits(output, b$targets)
    loss$backward()
    optimizer$step()
    total_loss <- total_loss + loss$item()
  }
  cat(sprintf("Epoch: %d, Loss: %.4f\n", epoch, total_loss / length(train_dl)))
}

# Evaluate the model
model$eval()
total_loss <- 0
for (b in enumerate(test_dl)) {
  output <- model(b$features)
  loss <- nnf_binary_cross_entropy_with_logits(output, b$targets)
  total_loss <- total_loss + loss$item()
}
cat(sprintf("Test Loss: %.4f\n", total_loss / length(test_dl)))



















# Assuming 'data' contains 'reduced_data' and 'cnn_output'
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create and prepare the recipe
recipe <- recipe(cnn_output ~ ., data = train_data) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Prepare processed training and testing sets
prep_recipe <- prep(recipe)
train_processed <- bake(prep_recipe, new_data = train_data)
test_processed <- bake(prep_recipe, new_data = test_data)

# Ensure all data is numeric
train_processed[] <- lapply(train_processed, as.numeric)
test_processed[] <- lapply(test_processed, as.numeric)

# Convert the data to tensors
train_features <- torch_tensor(as.matrix(train_processed[, -ncol(train_processed)]), dtype = torch_float32())
train_targets <- torch_tensor(as.matrix(train_processed[, ncol(train_processed)]), dtype = torch_float32())
test_features <- torch_tensor(as.matrix(test_processed[, -ncol(test_processed)]), dtype = torch_float32())
test_targets <- torch_tensor(as.matrix(test_processed[, ncol(test_processed)]), dtype = torch_float32())

# Create tensor datasets
train_dataset <- tensor_dataset(list(features = train_features, targets = train_targets))
test_dataset <- tensor_dataset(list(features = test_features, targets = test_targets))

# Create dataloaders
train_dl <- dataloader(train_dataset, batch_size = 64, shuffle = TRUE)
test_dl <- dataloader(test_dataset, batch_size = 64)

# Fit the TabNet model
tabnet_model <- tabnet(
  train_dl = train_dl,
  valid_dl = test_dl,
  num_features = ncol(train_features_matrix), # Adjust this to the number of features
  num_targets = 1,  # Since you're predicting one output
  epochs = 3,
  batch_size = 64,
  virtual_batch_size = 32
)








































library(tabnet)
library(torch)
library(recipes)  # Ensure it's installed and loaded

# Assuming 'data' contains 'reduced_data' and 'cnn_output'
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create and prepare the recipe
recipe <- recipe(cnn_output ~ ., data = train_data) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Prepare processed training and testing sets
prep_recipe <- prep(recipe)
train_processed <- bake(prep_recipe, new_data = train_data)
test_processed <- bake(prep_recipe, new_data = test_data)



# Ensure all data in train_processed and test_processed is numeric
if (any(sapply(train_processed, class) != "numeric")) {
  train_processed[] <- lapply(train_processed, as.numeric)
}
if (any(sapply(test_processed, class) != "numeric")) {
  test_processed[] <- lapply(test_processed, as.numeric)
}

# Convert the numeric data frame to a matrix
train_features_matrix <- as.matrix(train_processed[, -ncol(train_processed)])
train_targets_matrix <- as.matrix(train_processed[, ncol(train_processed)])

# Now convert to tensors
train_features <- torch_tensor(train_features_matrix, dtype = torch_float32())
train_targets <- torch_tensor(train_targets_matrix, dtype = torch_float32())

# Repeat for test data
test_features_matrix <- as.matrix(test_processed[, -ncol(test_processed)])
test_targets_matrix <- as.matrix(test_processed[, ncol(test_processed)])
test_features <- torch_tensor(test_features_matrix, dtype = torch_float32())
test_targets <- torch_tensor(test_targets_matrix, dtype = torch_float32())

# Convert the numeric data frame to a matrix
train_features_matrix <- as.matrix(train_processed[, -ncol(train_processed)])
train_targets_matrix <- as.matrix(train_processed[, ncol(train_processed)])

# Convert matrices to tensors
train_features <- torch_tensor(train_features_matrix, dtype = torch_float32())
train_targets <- torch_tensor(train_targets_matrix, dtype = torch_float32())

# Prepare test data
test_features_matrix <- as.matrix(test_processed[, -ncol(test_processed)])
test_targets_matrix <- as.matrix(test_processed[, ncol(test_processed)])
test_features <- torch_tensor(test_features_matrix, dtype = torch_float32())
test_targets <- torch_tensor(test_targets_matrix, dtype = torch_float32())



# Define and fit the TabNet model
tabnet_model <- tabnet_pretrain(
  x = train_features,  # Input features tensor
  y = train_targets,   # Target tensor
  dims = ncol(train_features_matrix), # Adjust this to the number of features
  out_dims = 1, # Since you're predicting one output (close prices)
  batch_size = 64,
  virtual_batch_size = 32,
  epochs = 2 # You can adjust the number of training epochs
)

# Evaluate the model on the test dataset
predictions <- predict(tabnet_model, test_features)







































library(tabnet)
library(torch)
library(recipes)  # For data pre-processing
#install.packages("recipes")
# Assuming 'data' is your DataFrame with 'reduced_data' and 'cnn_output'
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create a recipe for pre-processing
recipe <- recipe(cnn_output ~ ., data = train_data) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Fit the TabNet model
tabnet_fit <- tabnet_fit(
  recipe,
  train_data,
  valid_data = test_data,
  epochs = 2,
  valid_split = 0.1,
  learn_rate = 1e-3
)

# Use the model to make predictions and evaluate
predictions <- predict(tabnet_fit, test_data)




























# Convert training data to tensors
features_tensor <- torch_tensor(as.matrix(train_data[, -ncol(train_data)]))
targets_tensor <- torch_tensor(train_data[, ncol(train_data)], dtype = torch_float32())

# Create a dataset from tensors for training
train_dataset <- tensor_dataset(list(features = features_tensor, targets = targets_tensor))

# Define a function to collate data into batches
collate_fn <- function(batch) {
  list(
    features = torch_stack(lapply(batch, function(x) x$features)),
    targets = torch_stack(lapply(batch, function(x) x$targets))
  )
}

# Create dataloaders for training and testing
batch_size <- 64  # adjust this based on  memory capacity
train_dl <- dataloader(train_dataset, batch_size = batch_size, shuffle = TRUE, collate_fn = collate_fn)
test_dataset <- tensor_dataset(list(features = torch_tensor(as.matrix(test_data[, -ncol(test_data)])),
                                    targets = torch_tensor(test_data[, ncol(test_data)], dtype = torch_float32())))
test_dl <- dataloader(test_dataset, batch_size = batch_size, collate_fn = collate_fn)


# Define and train the TabNet model
tabnet_model <- tabnet_fit(
  x = as.matrix(train_data[, -ncol(train_data)]),  # input features
  y = as.matrix(train_data[, ncol(train_data)]),  # target variable (close prices)
  dims = ncol(train_data) - 1, # number of features
  out_dims = 1,  # predicting one output
  batch_size = batch_size,
  virtual_batch_size = 32  # can be tuned based on  specific dataset
)

# Fit the model
tabnet_model$fit(train_dl, valid_data = test_dl, max_epochs = 50)




















# Define and train the TabNet model
tabnet_model <- tabnet_pretrain(
  dims = ncol(train_data) - 1, # minus one to exclude the target variable
  out_dims = 1,  # predicting one output, the close price
  batch_size = batch_size,
  virtual_batch_size = 32  # can be tuned based on your specific dataset
)
tabnet_model$fit(train_dl, valid_data = test_dl, max_epochs = 5)


















# 
features_tensor <- torch_tensor(as.matrix(train_data[, -ncol(train_data)]))
targets_tensor <- torch_tensor(train_data[, ncol(train_data)], dtype = torch_float32())

# Create a dataset from tensors
train_dataset <- tensor_dataset(list(features = features_tensor, targets = targets_tensor))

# Define a function to collate data into batches
collate_fn <- function(batch) {
  list(
    features = torch_stack(lapply(batch, function(x) x$features)),
    targets = torch_stack(lapply(batch, function(x) x$targets))
  )
}

# Create dataloaders
train_dl <- dataloader(train_dataset, batch_size = 64, shuffle = TRUE, collate_fn = collate_fn)
















batch_size <- 64  #  adjust this based on your memory capacity
train_dl <- dataloader(train_data, batch_size = batch_size, shuffle = TRUE)
test_dl <- dataloader(test_data, batch_size = batch_size)



# Define the TabNet model
tabnet_model <- tabnet_pretrain(
  dims = ncol(train_data) - 1, # minus one to exclude the target variable
  out_dims = 1,  # predicting one output, the close price
  batch_size = batch_size,
  virtual_batch_size = 32  # can be tuned based on your specific dataset
)

# Train the model
tabnet_model$fit(train_dl, valid_data = test_dl, max_epochs = 50)



# Predict on test data
predictions <- tabnet_model$predict(test_dl)

# Calculate RMSE or any other metric
rmse <- sqrt(mean((predictions - test_data$cnn_output)^2))
print(paste("RMSE on test data:", rmse))




























# Assuming your data is loaded into a 3D array data of dimension (1514047, 5, 32)
# You might need to adjust the dimensions according to how your data is stored
datapca <- array(runif(1514047 * 5 * 32), dim = c(1514047, 5, 32))

# Flatten the data
library(reshape2)
flattened_data <- melt(data, id.vars = c("5", "32"))
flattened_data <- acast(flattened_data, Var1 ~ Var2 + Var3)




# Apply PCA to the flattened data
pca <- prcomp(flattened_data, scale. = TRUE, center = TRUE)

# Selecting the number of components that explain a significant amount of variance, e.g., 95%
explained_variance <- summary(pca)$importance[2,]
cumulative_variance <- cumsum(explained_variance)
num_components <- which(cumulative_variance >= 0.95)[1]

# Using the number of components to reduce dimensions
reduced_data <- pca$x[, 1:num_components]




# Assuming 'data' is your 3D array of size 1,514,047 x 5 x 32
# Load your data or create the array here if not already defined

# Flatten the array into a 2D matrix
flattened_data <- array(data, dim = c(dim(data)[1], dim(data)[2] * dim(data)[3]))

# The resulting 'flattened_data' is now a 1,514,047 x 160 matrix
# You can now proceed to apply PCA or any other analysis suitable for your task









# Extract features
feature_extractor <- keras_model(inputs = model$input, outputs = get_layer(model, index = -1)$output)

#feature_extractor <- keras_model(inputs = model$input, outputs = get_layer(model, name = "lstm_layer_name")$output)

features <- predict(feature_extractor, cnn_input)
 

# Save the CNN extracted features
saveRDS(features, file = "C:/Users/a/Desktop/cnn_extracted_features.rds")



 
# Get range of values in each column
feature_ranges <- apply(features, 2, range)
print(feature_ranges)

# Get summary statistics for each column
feature_summaries <- apply(features, 2, summary)
print(feature_summaries)

# If you want to see the frequency of specific values (like 0, Inf, -Inf) in each column
# This can be computationally intensive for large datasets with many unique values
feature_value_counts <- lapply(1:ncol(features), function(i) table(features[, i]))
names(feature_value_counts) <- colnames(features)
print(feature_value_counts)






#### TPM  #########1111111111111111111111111111111111111

dim(features)



n_samples <- 981        # Number of samples
sequence_length <- 320  # Number of time steps (assuming each feature is a time step)
n_features <- 1         # Number of features per time step (assuming a univariate time series)

# Define LSTM units (k)
lstm_units <- 50  # Example value, adjust as needed

# Define the simple attention mechanism
transformer <- function(input, num.heads, dropout.rate = 0) {
  input.shape <- input$shape
  
  # Multi-head self-attention layer
  multi_head_attention <- layer_multi_head_attention(
    key_dim = input.shape[2], 
    num_heads = num.heads
  )
  
  # Apply attention mechanism
  attn_output <- multi_head_attention(input, input) %>%
    layer_dropout(rate = dropout.rate)
  
  # Residual connection and layer normalization after attention
  attn_output <- layer_add(list(input, attn_output)) %>%
    layer_layer_normalization()
  
  # Feedforward neural network
  feedforward_output <- attn_output %>%
    layer_dense(units = input.shape[3], activation = "relu") %>%
    layer_dropout(rate = dropout.rate)
  
  # Residual connection and layer normalization after feedforward layer
  output <- layer_add(list(attn_output, feedforward_output)) %>%
    layer_layer_normalization()
  
  output
}

 

# Define the main model
library(keras)

 
 
# Define the main model
data_input <- layer_input(shape = c(sequence_length, n_features))
lstm_out <- layer_lstm(units = lstm_units, return_sequences = TRUE)(data_input)
attention_out <- transformer(list(data_input, lstm_out), num.heads = 2)




# Final output layer
output <- layer_dense(units = 1)(attention_out) # Assuming a single output unit for regression

model <- keras_model(inputs = data_input, outputs = output)

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(lr = 0.001),
  loss = 'mse'  # Mean Squared Error for regression tasks
)

# Summary to check the model
summary(model)

# Prepare your data for training
# Ensure 'features' is reshaped or transformed as needed for your model
input_data <- array_reshape(features, c(n_samples, sequence_length, n_features))

# Fit the model
history <- model %>% fit(
  x = input_data,  # Your reshaped input features
  y = output_data,  # Your output data
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
)


























666666666666666
# Assuming you have a cleaned feature matrix 'features' with the shape [number_of_samples, T-1, m]
sequence_length <- 21 - 1  # Replace T with your actual sequence length + 1
n_features <- 19           # Replace m with your actual number of features per time step

# Define the LSTM model with attention
model <- keras_model_sequential() %>%
  layer_lstm(units = k, return_sequences = TRUE, input_shape = c(sequence_length, n_features)) %>%
  # Here you would insert your attention mechanism
  # The attention mechanism needs to be a custom layer or a function that returns an appropriate tensor
  # ...
  layer_dense(units = num_output)  # num_output is the number of output units (e.g., 1 for regression)

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(lr = 0.001),
  loss = 'mse'  # Mean Squared Error for regression tasks
)

# Summary to check the model
summary(model)

# Fit the model (assuming you have input and output data prepared)
history <- model %>% fit(
  x = input_data,  # This would be your cleaned 'features' reshaped appropriately
  y = output_data, # This would be your target variable
  epochs = 100,    # Number of epochs to train
  batch_size = 32, # Batch size for training
  validation_split = 0.2
)






























library(keras)

# Assuming you have pre-processed and split your data into 'train_X', 'train_Y', 'test_X', and 'test_Y'
n_samples <- dim(features)[1]  # Number of sequences
n_timesteps <- 20              # Length of time sequence, as indicated
n_features <- dim(features)[2] # Number of features per time step

input_shape <- c(n_timesteps, n_features)

# Define model
model <- keras_model_sequential()

# Add LSTM layer - you can tweak the number of units based on your dataset
model %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = input_shape) %>%
  layer_dropout(rate = 0.2)

# Define a simple attention mechanism
SimpleAttention <- function(inputs) {
  # Define the layers to be used inside the function
  dense_layer <- layer_dense(units = 1, activation = 'tanh')
  repeat_vector_layer <- layer_repeat_vector(n = dim(inputs)[2])
  permute_layer <- layer_permute(c(2, 1))
  
  # Apply the layers
  attention_scores <- inputs %>%
    dense_layer() %>%
    layer_flatten() %>%
    layer_activation(activation = 'softmax')
  attention_scores <- repeat_vector_layer(attention_scores)
  attention_scores <- permute_layer(attention_scores)
  
  applied_attention <- list(inputs, attention_scores) %>%
    layer_multiply()
  
  # Sum over the time steps dimension
  output <- applied_attention %>%
    layer_lambda(f = function(x) k_sum(x, axis = 1))
  
  return(output)
}

# Assume 'input_shape' is defined correctly
#input_shape <- c(20, 19)  # for example

# Build the model with the custom attention layer
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = input_shape) %>%
  layer_dropout(rate = 0.2) %>%
  SimpleAttention() %>%
  layer_dense(units = 1)


# Compile model
model %>% compile(
  loss = 'mean_squared_error', # or 'binary_crossentropy'/'categorical_crossentropy' if classification
  optimizer = optimizer_adam(lr = 0.001, clipnorm = 1.0),  # clipnorm to prevent exploding gradients
  metrics = c('accuracy')
)

# Summary to get an overview of the model
summary(model)

# Fit model on the training data
history <- model %>% fit(
  train_X, train_Y,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2,
  shuffle = TRUE
)

# Evaluate model on test data
model %>% evaluate(test_X, test_Y)

# Make predictions
predictions <- model %>% predict(test_X)







































library(keras)
library(tensorflow)

# Assuming you have pre-processed your data and extracted the CNN features and PLR features.
# Assuming cnn_features are the features extracted from the CNN, and plr_features are the PLR-extracted features.
# cnn_features would be a 3D array of shape (samples, timesteps, features).
# plr_features would be a 2D array of shape (samples, features).

# Load the CNN extracted features
cnn_extracted_features <- readRDS(file = "cnn_extracted_features.rds")


666666666666
print(dim(cnn_extracted_features))
print(head(cnn_extracted_features))

# Assuming cnn_extracted_features is loaded correctly and has the shape [samples, sequence_length, features]
#sequence_length <- dim(cnn_extracted_features)[2]
#n_cnn_features <- dim(cnn_extracted_features)[3]

# Choose a number of timesteps per sample (this is something you need to decide based on your data)
n_timesteps <- 20  # Example value
n_features <- 320 / n_timesteps  # Calculate the number of features per timestep
hidden_units <- 50  # Adjust this value based on your model's complexity

# Reshape your data into a 3D array
# We will assume that the total number of samples is divisible by n_timesteps for simplicity
n_samples <- nrow(cnn_extracted_features) / n_timesteps

# The array will have dimensions [samples, timesteps, features]
cnn_input_3d <- array(data = as.vector(t(cnn_extracted_features)), dim = c(n_samples, n_timesteps, n_features))


# Correctly calculate the sequence_length and n_cnn_features from the reshaped 3D array
sequence_length <- n_timesteps  # This is the number you defined for how many timesteps per sample
n_cnn_features <- n_features  # This is the calculated number of features per timestep

# Now, use these correct values to define your model input shape
encoder_inputs <- layer_input(shape = c(sequence_length, n_cnn_features))

# Create the LSTM layer with the correct number of hidden units
encoder_lstm_out <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_inputs)
55555555555555555



library(keras)
library(tensorflow)
library(R6)

# Assuming cnn_extracted_features is loaded correctly
n_timesteps <- 20  # Number of timesteps per sample
n_features <- 320 / n_timesteps  # Number of features per timestep
hidden_units <- 50  # Number of hidden units in LSTM
sequence_length <- n_timesteps
n_cnn_features <- n_features

# Define the LSTM encoder
encoder_inputs <- layer_input(shape = c(sequence_length, n_cnn_features))
encoder_lstm_out <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_inputs)

# Define the custom attention layer
AttentionLayer <- R6::R6Class(
  "AttentionLayer",
  inherit = KerasLayer,
  public = list(
    Wq = NULL,
    Wv = NULL,
    V = NULL,
    
    build = function(input_shape) {
      self$Wq <- self$add_weight(
        name = 'Wq',
        shape = list(hidden_units, hidden_units),
        initializer = 'glorot_uniform',
        trainable = TRUE
      )
      self$Wv <- self$add_weight(
        name = 'Wv',
        shape = list(hidden_units, hidden_units),
        initializer = 'glorot_uniform',
        trainable = TRUE
      )
      self$V <- self$add_weight(
        name = 'V',
        shape = list(hidden_units, 1),
        initializer = 'glorot_uniform',
        trainable = TRUE
      )
    },
    
    call = function(inputs, mask = NULL) {
      # Unpack the list of inputs
      query <- inputs[[1]]
      value <- inputs[[2]]
      
      # Perform the attention mechanism calculations here
      query_with_weights <- k_dot(query, self$Wq)
      value_with_weights <- k_dot(value, self$Wv)
      scores <- k_dot(query_with_weights, k_transpose(value_with_weights))
      
      # Calculating the attention weights
      attention_weights <- k_softmax(scores)
      
      # Calculating the context vector as a weighted sum of the values
      context_vector <- k_dot(attention_weights, value)
      context_vector <- k_sum(context_vector, axis = 2)
      context_vector <- k_reshape(context_vector, shape = c(-1, hidden_units))
      
      return(list(context_vector, attention_weights))
    }
  )
)

# Create an instance of the custom attention layer
attention_layer <- AttentionLayer$new()

# Define the model with the attention mechanism
model <- keras_model(inputs = encoder_inputs, outputs = encoder_lstm_out)

# Use the model to get the LSTM outputs
encoder_lstm_out <- model(encoder_inputs)

# Check the shape of the LSTM output
print(k_int_shape(encoder_lstm_out))

# Reshape LSTM output before passing to the attention layer
encoder_lstm_out_reshaped <- k_reshape(encoder_lstm_out, shape = c(-1, sequence_length, hidden_units))

# Check the shape after reshaping
print(k_int_shape(encoder_lstm_out_reshaped))

# Apply the attention layer to the reshaped output of the LSTM
encoder_attention_output <- attention_layer$call(list(encoder_lstm_out_reshaped, encoder_lstm_out_reshaped))


# Define a model from inputs to attention output
attention_model <- keras_model(inputs = encoder_inputs, outputs = encoder_attention_output[[1]])

# Compile the model
model %>% compile(
  loss = 'binary_crossentropy',  # Choose loss function appropriate for your problem
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Placeholder for training the model - replace with your actual data
# model %>% fit(
#   x_train, y_train,
#   batch_size = 32,
#   epochs = 10,
#   validation_split = 0.2
# )






666666666666

library(keras)

# Assuming cnn_extracted_features is a 3D array with dimensions [samples, timesteps, features]
# Assuming you have already loaded cnn_extracted_features from an RDS file



encoder_inputs <- layer_input(shape = c(sequence_length, n_features))
encoder_lstm <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_inputs)

AttentionLayer <- R6::R6Class("AttentionLayer",
                              inherit = KerasLayer,
                              public = list(
                                Wq = NULL,
                                Wv = NULL,
                                V = NULL,
                                
                                initialize = function() {
                                  self$Wq <- NULL
                                  self$Wv <- NULL
                                  self$V <- NULL
                                },
                                
                                build = function(input_shape) {
                                  self$Wq <- self$add_weight(
                                    name = 'Wq',
                                    shape = list(hidden_units, hidden_units),
                                    initializer = 'glorot_uniform',
                                    trainable = TRUE
                                  )
                                  self$Wv <- self$add_weight(
                                    name = 'Wv',
                                    shape = list(hidden_units, hidden_units),
                                    initializer = 'glorot_uniform',
                                    trainable = TRUE
                                  )
                                  self$V <- self$add_weight(
                                    name = 'V',
                                    shape = list(hidden_units, 1),
                                    initializer = 'glorot_uniform',
                                    trainable = TRUE
                                  )
                                },
                                
                                call = function(inputs, mask = NULL) {
                                  # Assuming inputs is a list with two elements: query and value
                                  query <- inputs[[1]]
                                  value <- inputs[[2]]
                                  
                                  # Perform the attention mechanism calculations here
                                  # This is a placeholder for the actual implementation
                                  # You will need to replace it with the actual attention calculations as per the paper
                                  
                                  # Example calculation (this is not the actual attention formula and needs to be replaced):
                                  scores <- k_matmul(query, k_transpose(self$Wq))
                                  attention_weights <- k_softmax(scores)
                                  context_vector <- k_matmul(attention_weights, value)
                                  
                                  return(context_vector)
                                }
                              )
)

# Create an instance of the custom attention layer
attention_layer <- AttentionLayer$new()

# Apply the attention layer to the output of the LSTM
encoder_attention_output <- attention_layer$call(list(encoder_lstm_out, encoder_lstm_out))

encoder_attention_output <- attention_layer(list(encoder_lstm, encoder_lstm))
# Continue building the rest of your model architecture as needed
# ...

# When you're ready, compile and fit the model
# model %>% compile(...)
# model %>% fit(...)

666666666666666




















# Define the attention mechanism
attention_probs <- layer_dense(units = sequence_length, activation = 'softmax')(encoder_lstm_out)
attention_mul <- layer_multiply(list(encoder_lstm_out, attention_probs))

# Further steps to define the rest of the model go here


















6666666666
# Define the encoder with attention
# Define model parameters
sequence_length <- dim(cnn_extracted_features)[2]  # assuming the second dimension of cnn_features represents the sequence length
n_cnn_features <- dim(cnn_extracted_features)[3]  # assuming the third dimension of cnn_features represents the number of features
hidden_units <- 50  # you can adjust this value based on your model's complexity

# Define the encoder model with attention
encoder_inputs <- layer_input(shape = c(sequence_length, n_cnn_features))
encoder_lstm_out <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_inputs)

# Define custom attention layer (make sure you have this function properly defined as per the paper)
attention_probs <- layer_dense(units = sequence_length, activation = 'softmax')(encoder_lstm_out)
attention_mul <- layer_multiply(list(encoder_lstm_out, attention_probs))














# Load the PLR extracted features
plr_extracted_features <- readRDS(file = "plr_extracted_features.rds") 

# Define the attention mechanism

Attention <- function(hidden_size) {
  keras_model_custom(name = 'Attention', function(self) {
    self$dense <- layer_dense(units = hidden_size, activation = 'tanh')
    self$softmax <- function(x) tf$nn$softmax(x)
    
    function(inputs, mask = NULL) {
      features <- inputs[[1]]
      hidden_states <- inputs[[2]]
      
      score <- self$dense(tf$concat(list(features, hidden_states), axis = -1))
      attention_weights <- self$softmax(score)
      context_vector <- tf$reduce_sum(tf$multiply(features, attention_weights), axis = 1)
      
      list(context_vector, attention_weights)
    }
  })
}











6666666


# Define the input layer for the LSTM
encoder_inputs <- layer_input(shape = c(sequence_length, n_cnn_features))

# Define the LSTM layer with return_sequences set to TRUE
encoder_lstm <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_inputs)

# Define the attention layer
attention_layer <- Attention(hidden_size = hidden_units)
attention_layer <- layer_attention()(query = encoder_lstm, value = encoder_lstm)

# Apply the attention layer to the output of the LSTM
# Assume attention_layer returns a list with two elements
attention_output <- attention_layer(list(encoder_inputs, encoder_lstm))

# If the attention_layer returns a list, you can then extract the elements like this:
encoder_attention <- attention_output[[1]]
attention_weights <- attention_output[[2]]







22222222
# Define the encoder with attention
n_cnn_features <- 19
encoder_inputs <- layer_input(shape = c(sequence_length, n_cnn_features))  # n_cnn_features is the number of features from CNN
encoder_lstm <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_inputs)
encoder_attention <- layer_attention()(inputs = list(query = encoder_lstm, value = encoder_lstm))

encoder_outputs <- encoder_attention[[1]]


# Define the decoder with attention
decoder_lstm <- layer_lstm(units = hidden_units, return_sequences = TRUE)(encoder_outputs)
decoder_attention <- Attention(hidden_size = hidden_units)(list(decoder_lstm, encoder_lstm))
decoder_outputs <- decoder_attention[[1]]

# Define the decoder with attention
# Combine PLR features with the context vector from the encoder's attention
decoder_inputs <- layer_input(shape = c(n_plr_features))  # n_plr_features is the number of PLR features
decoder_combined_inputs <- layer_concatenate(c(encoder_outputs, decoder_inputs))
decoder_lstm <- layer_lstm(units = hidden_units, return_sequences = TRUE)(decoder_combined_inputs)
decoder_attention <- Attention(hidden_size = hidden_units)(list(decoder_lstm, encoder_lstm))
decoder_outputs <- decoder_attention[[1]]

# Final prediction layer
predictions <- layer_dense(units = 1)(decoder_outputs)

# Build the model
model <- keras_model(inputs = c(encoder_inputs), outputs = predictions)

model <- keras_model(inputs = c(encoder_inputs, decoder_inputs), outputs = predictions)

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error'
)

# Train the model
history <- model %>% fit(
  x = list(encoder_input_data), 
  y = output_data, 
  epochs = 100, 
  batch_size = 64, 
  validation_split = 0.2
)

# Evaluate the model
# Similarly, the test data needs to be split accordingly.

model %>% evaluate(test_data, test_labels)
model %>% evaluate(list(test_cnn_features, test_plr_features), test_labels)




 






































6666666666666666666666666


777777777777

# Print dimensions of cleaned data
print(dim(input_clean))
print(dim(output_clean))





555555555555555


# Assuming cnn_output is a vector of stock prices
min_val_output <- min(cnn_output, na.rm = TRUE)
max_val_output <- max(cnn_output, na.rm = TRUE)

# Normalize the output
cnn_output_normalized <- (cnn_output - min_val_output) / (max_val_output - min_val_output)

# Now cnn_output_normalized can be used as the target for training the CNN

5555















# Train the model with the input data
history <- model %>% fit(
  x = cnn_input, # Your input data
  y = cnn_output, # Your target data
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model
model %>% evaluate(cnn_input, cnn_output)

# Make predictions
predictions <- model %>% predict(cnn_input)


intermediate_layer_model <- keras_model(inputs = model$input, outputs = model$get_layer(index = 3)$output)
extracted_features <- predict(intermediate_layer_model, input_data)



