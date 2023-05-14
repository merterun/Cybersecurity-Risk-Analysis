df1209 <- read_csv("2022-12-09-enriched.csv")
df0704 <- read_csv("2022-07-04-enriched.csv")
df0627 <- read_csv("2022-06-27-enriched.csv")
df0609 <- read_csv("2022-06-09-enriched.csv")
df0608 <- read_csv("2022-06-08-enriched.csv")

all_df <- rbind(df0608, df0609, df0627, df0704, df1209)

head(all_df)
dim(all_df)
str(all_df)
summary(all_df)

sapply(all_df, function(x) sum(is.na(x)))

all_df <- subset(all_df, select = -c(notes))

sapply(all_df, function(x) sum(is.na(x)))

str(all_df)



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



