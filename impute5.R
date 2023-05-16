# Count missing values in each column
colSums(is.na(all_df_imp))

sum(is.na(all_df_imp))

# Create a copy of the original data frame
all_imputed <- all_df_imp




commonality_impute <- function(variable) {
  # Get indices of missing values in the current variable
  missing_indices <- is.na(variable)
  
  # Check if all values are missing
  if (all(missing_indices)) {
    # If all values are missing, return NA
    return(NA)
  }
  
  # Check if the variable is a data frame
  if (is.data.frame(variable)) {
    # Get the row indices where the current variable has a missing value
    missing_rows <- which(missing_indices)
    
    # Iterate over the missing rows
    for (row in missing_rows) {
      # Get the values of other variables for the current missing row
      other_values <- variable[row, !missing_indices]
      
      # Check if there are at least 7 same values in the other rows
      if (sum(rowSums(variable == other_values, na.rm = TRUE) >= 11, na.rm = TRUE) > 0) {
        # Find the most common value across the other variables
        common_value <- as.character(names(which.max(table(other_values))))
        
        # Replace the missing value in the current row with the common value
        variable[row, missing_indices[row]] <- common_value
      } else {
        # If there are not enough common values, leave the missing value as NA
        variable[row, missing_indices[row]] <- NA
      }
    }
  } else {
    # If the variable is a vector, impute missing values based on the entire vector
    other_values <- variable[!missing_indices]
    
    # Check if there are at least 7 same values in the vector
    if (sum(variable %in% other_values, na.rm = TRUE) > 7) {
      # Find the most common value across the vector
      common_value <- as.character(names(which.max(table(other_values))))
      
      # Replace the missing values with the common value
      variable[missing_indices] <- common_value
    } else {
      # If there are not enough common values, leave the missing values as NA
      variable[missing_indices] <- NA
    }
  }
  
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

# Count missing values in each column
colSums(is.na(all_imputed))

sum(is.na(all_imputed))

# Retrieve the imputed data frame
# imputed_data <- all_imputed
