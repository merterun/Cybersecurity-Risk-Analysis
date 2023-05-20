## Vendor/Product Resilience:
## Which vendors/products have a higher resilience against vulnerabilities? In other words, which ones are less affected by vulnerabilities in the network?
  
# Calculate the number of vulnerabilities per vendor
vendor_resilience <- all_imp %>%
  group_by(vendor_project) %>%
  summarize(total_vulnerabilities = n_distinct(cve_id)) %>%
  arrange(total_vulnerabilities)

# Calculate the number of vulnerabilities per product
product_resilience <- all_imp %>%
  group_by(product) %>%
  summarize(total_vulnerabilities = n_distinct(cve_id)) %>%
  arrange(total_vulnerabilities)

library(ggplot2)

# Create a bar chart for vendor resilience (top 15 and bottom 15)
vendor_plot <- ggplot(vendor_resilience, aes(x = reorder(vendor_project, total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Vendor", y = "Total Vulnerabilities", title = "Vendor Resilience Against Vulnerabilities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  scale_x_discrete(limits = c(head(vendor_resilience$vendor_project, 10), tail(vendor_resilience$vendor_project, 25)))

# Create a bar chart for product resilience (top 15 and bottom 15)
product_plot <- ggplot(product_resilience, aes(x = reorder(product, total_vulnerabilities), y = total_vulnerabilities)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Product", y = "Total Vulnerabilities", title = "Product Resilience Against Vulnerabilities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  scale_x_discrete(limits = c(head(product_resilience$product, 10), tail(product_resilience$product, 25)))

# Display the vendor and product plots
vendor_plot
product_plot


## Are there any vendors/products that consistently address vulnerabilities quickly, reducing their impact on the network?

# Filter the data to include only rows with non-null due_date
filtered_df <- all_imp[!is.na(all_imp$due_date), ]

# Calculate the time taken to address vulnerabilities
filtered_df$time_taken <- filtered_df$due_date - filtered_df$date_added

# Calculate the average time taken by each vendor/product
avg_time_taken <- aggregate(time_taken ~ vendor_project + product, filtered_df, FUN = mean)

# Sort the vendors/products based on average time taken in ascending order
sorted_avg_time_taken <- avg_time_taken[order(avg_time_taken$time_taken), ]

# Display the top 5 vendors/products that address vulnerabilities quickly
head(sorted_avg_time_taken, 5)


library(ggplot2)

# Subset the data to include only rows with non-null due_date
filtered_df <- all_imp[!is.na(all_imp$due_date), ]

# Calculate the time taken to address vulnerabilities
filtered_df$time_taken <- as.numeric(filtered_df$due_date - filtered_df$date_added)

# Calculate the average time taken by each vendor/product
avg_time_taken <- aggregate(time_taken ~ vendor_project + product, filtered_df, FUN = mean)

# Sort the vendors/products based on average time taken in ascending order
sorted_avg_time_taken <- avg_time_taken[order(avg_time_taken$time_taken), ]

# Create a subset of top 25 and bottom 10 vendors/products
top_25 <- head(sorted_avg_time_taken, 25)
bottom_10 <- tail(sorted_avg_time_taken, 10)

# Combine the top 25 and bottom 10 subsets
combined_data <- rbind(top_25, bottom_10)

# Create a lollipop chart
ggplot(combined_data, aes(x = reorder(product, time_taken), y = time_taken)) +
  geom_segment(aes(x = product, xend = product, y = 0, yend = time_taken), color = "#093a53") +
  geom_point(color = "#093a53", size = 2) +
  coord_flip() +
  labs(x = "Product", y = "Average Time Taken (days)") +
  ggtitle("Top 25 and Bottom 10 Vendors/Products by Average Time Taken to Address Vulnerabilities") +
  theme_minimal()


