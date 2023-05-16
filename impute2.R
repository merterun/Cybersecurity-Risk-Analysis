# Group the data by cve_id and count the number of unique values in each column
grouped_df <- all_df %>%
  group_by(cve_id) %>%
  summarize_all(n_distinct)

# Check if all columns have the same number of unique values for each cve_id
identical(grouped_df[-1], colSums(!is.na(grouped_df[-1]))[1])

# Check which columns have different numbers of unique values
which(!identical(grouped_df[-1], colSums(!is.na(grouped_df[-1]))[1]))

sapply(all_df, function(x) sum(is.na(x)))

# Load necessary packages
library(dplyr)

# Impute missing values in all columns
all_df_imp <- all_df %>%
  group_by(cve_id) %>%
  mutate(across(everything(), ~if_else(is.na(.), first(.[!is.na(.)]), .)))


str(all_df_imp)

sapply(all_df_imp, function(x) sum(is.na(x)))




