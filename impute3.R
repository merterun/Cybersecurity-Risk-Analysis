library(tidyverse)
library(mice)

# create a copy of the original data frame
all_num2 <- all_df_imp

# encode categorical variables using label encoding
all_num2$product <- as.numeric(as.factor(all_df_imp$product))
all_num2$vulnerability_name <- as.numeric(as.factor(all_df_imp$vulnerability_name))
all_num2$short_description <- as.numeric(as.factor(all_df_imp$short_description))
all_num2$required_action <- as.numeric(as.factor(all_df_imp$required_action))
all_num2$cwe <- as.numeric(as.factor(all_df_imp$cwe))
all_num2$vector <- as.numeric(as.factor(all_df_imp$vector))
all_num2$complexity <- as.numeric(as.factor(all_df_imp$complexity))
all_num2$severity <- as.numeric(as.factor(all_df_imp$severity))
all_num2$vendor_project <- as.numeric(as.factor(all_df_imp$vendor_project))
all_num2$cve_id <- as.numeric(as.factor(all_df_imp$cve_id))
all_num2$date_added <- as.numeric(as.factor(all_df_imp$date_added))
all_num2$due_date <- as.numeric(as.factor(all_df_imp$due_date))
all_num2$pub_date <- as.numeric(as.factor(all_df_imp$pub_date))

# impute missing values with mean imputation
all_num2 <- mice(all_num2, method="mean")

# perform imputation
all_num_imputed2 <- complete(all_num2)

head(all_num_imputed2)
sapply(all_num_imputed2, function(x) sum(is.na(x)))
