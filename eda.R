##Distribution of vulnerabilities by vendor
library(ggplot2)

# Count the number of vulnerabilities per vendor
vendor_counts <- table(all_imp$vendor_project)

# Create a bar plot to visualize the distribution of vulnerabilities by vendor
ggplot(data = data.frame(vendor = names(vendor_counts), count = as.numeric(vendor_counts)),
       aes(x = reorder(vendor, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Vendor", y = "Number of Vulnerabilities") +
  ggtitle("Distribution of Vulnerabilities by Vendor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

##Frequency of different types of vulnerabilities (e.g., SQL injection, command injection)

# Frequency of vulnerability types
vuln_type_counts <- table(all_imp$cwe)
vuln_type_counts <- as.data.frame(vuln_type_counts, stringsAsFactors = FALSE)
vuln_type_counts <- vuln_type_counts[order(vuln_type_counts$Freq, decreasing = TRUE), ]
colnames(vuln_type_counts) <- c("Vulnerability_Type", "Count")

# Bar plot
ggplot(data = vuln_type_counts, aes(x = Vulnerability_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "#496713") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Vulnerability Type") +
  ylab("Count") +
  ggtitle("Frequency of Vulnerability Types")

##Correlation between severity ratings and complexity of vulnerabilities
##Proportion of vulnerabilities with due dates met
##Analysis of vulnerabilities by Common Weakness Enumeration (CWE) categories
##Distribution of vulnerabilities by vector (e.g., network, local)
##Distribution of vulnerabilities by complexity level
##Analysis of required actions for vulnerabilities
##Comparison of vulnerability patterns across different vendors or products
##Analysis of vulnerability publication dates(seasonal)
##Trend analysis of vulnerability discoveries over time
##Identification of top 10 vendors with the highest number of vulnerabilities
##Identification of top 10 products with the highest number of vulnerabilities
##Analysis of the most common CWE categories among vulnerabilities

