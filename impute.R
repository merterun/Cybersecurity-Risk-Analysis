library(tidyverse)
library(mice)

# create a copy of the original data frame
all_num <- all_df

# encode categorical variables using label encoding
all_num$product <- as.numeric(as.factor(all_df$product))
all_num$vulnerability_name <- as.numeric(as.factor(all_df$vulnerability_name))
all_num$short_description <- as.numeric(as.factor(all_df$short_description))
all_num$required_action <- as.numeric(as.factor(all_df$required_action))
all_num$cwe <- as.numeric(as.factor(all_df$cwe))
all_num$vector <- as.numeric(as.factor(all_df$vector))
all_num$complexity <- as.numeric(as.factor(all_df$complexity))
all_num$severity <- as.numeric(as.factor(all_df$severity))
all_num$vendor_project <- as.numeric(as.factor(all_df$vendor_project))
all_num$cve_id <- as.numeric(as.factor(all_df$cve_id))
all_num$date_added <- as.numeric(as.factor(all_df$date_added))
all_num$due_date <- as.numeric(as.factor(all_df$due_date))
all_num$pub_date <- as.numeric(as.factor(all_df$pub_date))

# create a regression imputation model
imp_model <- mice(all_num, method="norm.predict")

# perform imputation
all_num_imputed <- complete(imp_model)

head(all_num_imputed)
sapply(all_num_imputed, function(x) sum(is.na(x)))
