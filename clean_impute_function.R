impute_and_clean_data <- function(all_imp) {
  # Impute product and short description columns
  all_imp$product[is.na(all_imp$product)] <- "fuel cms"
  all_imp$short_description[is.na(all_imp$short_description)] <- "na"
  
  # Impute pub_date for NA values based on cve_id
  all_imp <- all_imp %>%
    group_by(cve_id) %>%
    mutate(pub_date = ifelse(is.na(pub_date), max(pub_date, na.rm = TRUE), pub_date))
  
  all_imp <- na.omit(all_imp)
  
  # Identify numeric columns
  numeric_cols <- sapply(all_imp, is.numeric)
  
  # Convert numeric columns to numeric type
  all_num_clean <- all_imp
  all_num_clean[numeric_cols] <- lapply(all_num_clean[numeric_cols], function(x) as.numeric(as.character(x)))
  
  return(all_num_clean)
}

all_num_clean <- impute_and_clean_data(all_imp)

