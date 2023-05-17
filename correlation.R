# Convert numeric columns to numeric type
all_num_clean <- all_imp

# encode categorical variables using label encoding
all_num_clean$product <- as.numeric(as.factor(all_imp$product))
all_num_clean$vulnerability_name <- as.numeric(as.factor(all_imp$vulnerability_name))
all_num_clean$short_description <- as.numeric(as.factor(all_imp$short_description))
all_num_clean$required_action <- as.numeric(as.factor(all_imp$required_action))
all_num_clean$cwe <- as.numeric(as.factor(all_imp$cwe))
all_num_clean$vector <- as.numeric(as.factor(all_imp$vector))
all_num_clean$complexity <- as.numeric(as.factor(all_imp$complexity))
all_num_clean$severity <- as.numeric(as.factor(all_imp$severity))
all_num_clean$vendor_project <- as.numeric(as.factor(all_imp$vendor_project))
all_num_clean$cve_id <- as.numeric(as.factor(all_imp$cve_id))
all_num_clean$date_added <- as.numeric(as.factor(all_imp$date_added))
all_num_clean$due_date <- as.numeric(as.factor(all_imp$due_date))
all_num_clean$pub_date <- as.numeric(as.factor(all_imp$pub_date))

# Verify the structure of the new data frame
str(all_num_clean)

colSums(is.na(all_num_clean))


correlation_matrix3 <-  cor(all_num_clean)

corrplot(correlation_matrix3)
