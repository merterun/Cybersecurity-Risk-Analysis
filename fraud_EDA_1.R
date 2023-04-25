fraud_data <- read.csv("Online Payments Fraud Detection Dataset.csv")

library(magrittr)
library(tidyverse)

# get summary statistics
summary(fraud_data)

# get dimensions
dim(fraud_data)

# check for missing values
sum(is.na(fraud_data))

# check data types
str(fraud_data)

# type column is categorical and should be converted to a factor
fraud_data$type <- as.factor(fraud_data$type)

# create a new column for the difference between the old and new balance for originator and recipient accounts
fraud_data$orig_balance_diff <- fraud_data$newbalanceOrig - fraud_data$oldbalanceOrg
fraud_data$dest_balance_diff <- fraud_data$newbalanceDest - fraud_data$oldbalanceDest

# convert the type column to a factor
fraud_data$type <- as.factor(fraud_data$type)

# convert the isFraud column to a factor
fraud_data$isFraud <- as.factor(fraud_data$isFraud)

# convert the isFlaggedFraud column to a factor
fraud_data$isFlaggedFraud <- as.factor(fraud_data$isFlaggedFraud)

# check the new data types
str(fraud_data)

# create a histogram of the amount column
ggplot(fraud_data, aes(x=amount)) + 
  geom_histogram(bins=30, fill="dodgerblue", color="black") + 
  ggtitle("Distribution of Transaction Amounts") + 
  xlab("Transaction Amount") + 
  ylab("Count")

# create a boxplot of the amount column by transaction type
ggplot(fraud_data, aes(x=type, y=amount)) + 
  geom_boxplot(fill="dodgerblue", color="black") + 
  ggtitle("Distribution of Transaction Amounts by Type") + 
  xlab("Transaction Type") + 
  ylab("Transaction Amount")


# Density plot of transaction amount by isFraud
ggplot(fraud_data, aes(x = amount, fill = isFraud)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Transaction Amount Density by Fraud Status")

# Bar plot of isFraud count
ggplot(fraud_data, aes(x = factor(isFraud), fill = factor(isFraud))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Count of Fraud and Non-Fraud Transactions")

