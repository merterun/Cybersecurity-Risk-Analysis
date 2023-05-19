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

