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
all_imp <- all_df %>%
  group_by(cve_id) %>%
  mutate(across(everything(), ~if_else(is.na(.), first(.[!is.na(.)]), .)))


str(all_imp)

sapply(all_imp, function(x) sum(is.na(x)))




# create a copy of the original data frame
all_num <- all_imp

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

all_num_omitted <- all_num

all_num_omitted <- na.omit(all_num_omitted)


# Load the necessary package for correlation analysis
library(corrplot)

# Select the relevant columns for correlation analysis
columns_of_interest <- c("severity", "complexity", "vector", "cvss")

# Subset the dataframe with the selected columns
subset_df <- all_num_omitted[, columns_of_interest]

# Calculate the correlation matrix
correlation_matrix2 <- cor(subset_df)

# Visualize the correlation matrix using a correlation plot
corrplot(correlation_matrix2, method = "color")



# Calculate the correlation matrix
correlation_matrix_all <- cor(all_num_omitted)

# Visualize the correlation matrix using a correlation plot
corrplot(correlation_matrix_all)



# Load the required libraries
library(ggplot2)


# Create the chart
ggplot(all_df, aes(x = cvss, y = severity)) +
  geom_point(shape = 1, size = 3, color = "black") +
  labs(x = "CVSS Score", y = "Severity") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal()


# New imputation points
# Severity > 4 = MEDIUM, > 7 = HIGH, > 9 = CRITICAL
# pub_date = cve_id
# fuel cms = fuel cms


library(dplyr)

# Impute product and short description columns
all_imp$product[is.na(all_imp$product)] <- "fuel cms"
all_imp$short_description[is.na(all_imp$short_description)] <- "na"

# Impute pub_date for NA values based on cve_id
all_imp <- all_imp %>%
  group_by(cve_id) %>%
  mutate(pub_date = ifelse(is.na(pub_date), max(pub_date, na.rm = TRUE), pub_date))


# Check if NA values in cvss and severity are in the same rows
same_na_rows <- all(is.na(all_imp$cvss) == is.na(all_imp$severity))
same_na_rows # :(

all_imp <- na.omit(all_imp)

colSums(is.na(all_imp))