## Distribution of vulnerabilities by product
library(plotly)

# Create a data frame with product counts
product_counts <- as.data.frame(table(all_imp$product))

# Sort the product counts in descending order
product_counts <- product_counts[order(-product_counts$Freq), ]

# Create a hoverable bar chart using plotly
plot_ly(data = product_counts, x = ~Freq, y = ~Var1, type = "bar", orientation = "h") %>%
  layout(xaxis = list(title = "Number of Vulnerabilities"),
         yaxis = list(title = "Product"),
         title = "Distribution of Vulnerabilities by Product",
         hovermode = "closest",
         hovertemplate = paste(
           "<b>%{y}</b><br>",
           "Number of Vulnerabilities: %{x}"
         )
  )


## Analysis of vulnerability severity levels
# Count the severity levels of vulnerabilities
severity_counts <- table(all_imp$severity)

# Create a pie chart to visualize the distribution of vulnerability severity levels
pie(severity_counts, labels = names(severity_counts), main = "Distribution of Vulnerability Severity Levels")


##Time series analysis of vulnerabilities over a specific time period

library(lubridate)

# Time series analysis
all_imp$date_added <- as.Date(all_imp$date_added)
vuln_time_series <- data.frame(Date = all_imp$date_added)
vuln_time_series$Count <- 1

# Aggregate by date
vuln_time_series <- aggregate(Count ~ Date, vuln_time_series, sum)

# Line plot
ggplot(data = vuln_time_series, aes(x = Date, y = Count)) +
  geom_line() +
  xlab("Date") +
  ylab("Number of vulnerabilities") +
  ggtitle("Time Series Analysis of Vulnerabilities")


##Proportion of vulnerabilities with due dates met
# Calculate the proportion of vulnerabilities with due dates met
proportion_due_dates_met <- sum(!is.na(all_imp$due_date)) / nrow(all_imp) * 100

# Print the result
print(paste("Proportion of vulnerabilities with due dates met:", proportion_due_dates_met, "%"))


##Distribution of vulnerabilities by vector (e.g., network, local)
# Count vulnerabilities by vector
vulnerabilities_by_vector <- all_imp %>%
  group_by(vector) %>%
  summarise(count = n()) %>% 
  drop_na()

# Print the result
vulnerabilities_by_vector

# Create a pie chart for vulnerabilities by vector
chart_vector <- ggplot(vulnerabilities_by_vector, aes(x = "", y = count, fill = vector)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Vector", title = "Distribution of Vulnerabilities by Vector") +
  theme_void()

chart_vector


##Distribution of vulnerabilities by complexity level
# Distribution of vulnerabilities by complexity level
complexity_counts <- table(all_imp$complexity)
complexity_counts

barplot(complexity_counts, main = "Distribution of Vulnerabilities by Complexity Level", xlab = "Complexity Level", ylab = "Count")


##Identification of top 10 vendors with the highest number of vulnerabilities
top_vendors <- all_imp %>%
  group_by(vendor_project) %>%
  summarize(total_vulnerabilities = n()) %>%
  arrange(desc(total_vulnerabilities)) %>%
  head(10)

top_vendors

# Bar chart for top vendors
top_vendors_chart <- ggplot(top_vendors, aes(x = reorder(vendor_project, -total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Top 10 Vendors with the Highest Number of Vulnerabilities",
       x = "Vendor",
       y = "Total Vulnerabilities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_vendors_chart

##Identification of top 10 products with the highest number of vulnerabilities
top_products <- all_imp %>%
  group_by(product) %>%
  summarize(total_vulnerabilities = n()) %>%
  arrange(desc(total_vulnerabilities)) %>%
  head(10)

top_products

# Bar chart for top products
top_products_chart <- ggplot(top_products, aes(x = reorder(product, -total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "lightgreen", width = 0.5) +
  labs(title = "Top 10 Products with the Highest Number of Vulnerabilities",
       x = "Product",
       y = "Total Vulnerabilities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_products_chart

##Analysis of the most common CWE categories among vulnerabilities

common_cwe <- all_imp %>%
  group_by(cwe) %>%
  summarize(total_vulnerabilities = n()) %>%
  arrange(desc(total_vulnerabilities)) %>% 
  head(10) %>% 
  drop_na()

common_cwe

# Bar chart for common CWE categories
common_cwe_chart <- ggplot(common_cwe, aes(x = reorder(cwe, -total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.5) +
  labs(title = "Most Common CWE Categories among Vulnerabilities",
       x = "CWE Category",
       y = "Total Vulnerabilities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

common_cwe_chart


# Analyze patching speed
all_imp$patching_time <- all_imp$due_date - all_imp$pub_date

all_imp$patching_time <- as.numeric(all_imp$patching_time)

summary(all_imp$patching_time)


hist(all_imp$patching_time, breaks = 20, xlab = "Patching Time (Days)", main = "Distribution of Patching Time")

sum(is.na(all_imp$severity))
sum(is.na(all_imp$patching_time))

is.finite(all_imp$severity)
is.finite(all_imp$patching_time)



# Severity vs Patching Time
plot(all_num_clean$severity, all_imp$patching_time, xlab = "Severity", ylab = "Patching Time (Days)", main = "Severity vs. Patching Time")


cor(all_num_clean$severity, all_imp$patching_time)



library(zoo)

# Convert date_added column to year-month format
all_imp$date_added <- as.yearmon(all_imp$date_added)

# Count the number of vulnerabilities by year and month
vuln_counts <- table(all_imp$date_added)

# Plot the time series of vulnerability counts
plot(vuln_counts, xlab = "Date", ylab = "Number of Vulnerabilities",
     main = "Trends in Vulnerability Counts")

# If needed, you can also aggregate the data by severity level
severity_counts <- table(all_imp$date_added, all_imp$severity)

# Plot the time series of vulnerability counts by severity
plot(severity_counts, xlab = "Date", ylab = "Number of Vulnerabilities",
     main = "Trends in Vulnerability Counts by Severity")

