# Create a copy of the original data frame
all_imputed <- all_df_imp

# Define custom imputation function
commonality_impute <- function(variable) {
  # Get indices of missing values in the current variable
  missing_indices <- is.na(variable)
  
  # Check if all values are missing
  if (all(missing_indices)) {
    # If all values are missing, return NA
    return(NA)
  }
  
  # Get the values of other variables for rows where the current variable is not missing
  other_values <- variable[!missing_indices]
  
  # Check if there are other values available
  if (length(other_values) == 0) {
    # If there are no other values available, return the original variable
    return(variable)
  }
  
  # Find the most common value across the other variables
  common_value <- as.character(names(which.max(table(other_values))))
  
  # Replace missing values with the common value
  variable[missing_indices] <- common_value
  
  # Return the imputed variable
  return(variable)
}

# Apply the commonality imputation function to each column
all_imputed[] <- lapply(all_imputed, commonality_impute)

# Convert character columns to their original types
char_cols <- sapply(all_imputed, is.character)
all_imputed[char_cols] <- lapply(all_imputed[char_cols], as.character)

# Convert date columns to their original types
date_cols <- sapply(all_imputed, is.Date)
all_imputed[date_cols] <- lapply(all_imputed[date_cols], as.Date)

all_imputed

sum(is.na(all_imputed))
# Retrieve the imputed data frame
# imputed_data <- all_imputed
