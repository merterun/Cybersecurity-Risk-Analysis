## Product Relationships: Are there any dependencies or relationships between products? For example, do certain vulnerabilities affect multiple products from the same vendor or different vendors?

product_relationships <- all_num_clean %>%
  select(vulnerability_name, product) %>%
  distinct() %>%
  group_by(vulnerability_name) %>%
  summarise(products = paste(product, collapse = ", "))

ggplot(product_relationships, aes(x = reorder(vulnerability_name, -nchar(products)), y = nchar(products))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Vulnerability Name") +
  ylab("Number of Products Affected") +
  ggtitle("Number of Products Affected by Vulnerability") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


product_relationships2 <- all_imp %>%
  select(vulnerability_name, product) %>%
  distinct() %>%
  group_by(vulnerability_name) %>%
  summarise(num_products_affected = n(),
            products = paste(product, collapse = ", ")) %>%
  arrange(desc(num_products_affected)) %>%
  head(15)

product_relationships2 <- product_relationships2 %>%
  mutate(num_products_affected = sapply(strsplit(products, ", "), length))

product_relationships2


library(visNetwork)

# Create nodes dataframe
nodes <- data.frame(id = product_relationships2$vulnerability_name, label = product_relationships2$vulnerability_name)

# Create edges dataframe
edges <- data.frame(from = rep(1, nrow(product_relationships2)), to = seq_len(nrow(product_relationships2)), label = product_relationships2$num_products_affected)

# Create the network
network <- visNetwork(nodes, edges) %>%
  visNodes() %>%
  visEdges()

# Display the network chart
network



## Vulnerability Networks: Can you identify networks or clusters of vulnerabilities based on common attributes such as severity, CWE (Common Weakness Enumeration) category, or vector? Are there any patterns or relationships among these vulnerabilities?

library(ggrepel)

# Subset the data to include only the relevant columns
subset_df <- all_num_clean[, c("severity", "cwe", "vector")]

# Plotting CWE vs. Severity
cwe_severity_plot <- subset_df %>%
  group_by(cwe, severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = cwe, y = severity, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "CWE (Common Weakness Enumeration)", y = "Severity") +
  ggtitle("Vulnerability Clusters: CWE vs. Severity") +
  theme_bw()


# Plotting Vector vs. Severity
vector_severity_plot <- subset_df %>%
  group_by(vector, severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = vector, y = severity, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Vector", y = "Severity") +
  ggtitle("Vulnerability Clusters: Vector vs. Severity") +
  theme_bw()

# Plotting CWE vs. Severity with labels
cwe_severity_labels_plot <- subset_df %>%
  group_by(cwe, severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = cwe, y = severity, label = count, fill = count)) +
  geom_tile() +
  geom_text_repel() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "CWE (Common Weakness Enumeration)", y = "Severity") +
  ggtitle("Vulnerability Clusters: CWE vs. Severity (with Labels)") +
  theme_bw()

# Plotting Vector vs. Severity with labels
vector_severity_labels_plot <- subset_df %>%
  group_by(vector, severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = vector, y = severity, label = count, fill = count)) +
  geom_tile() +
  geom_text_repel(max.overlaps = Inf) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Vector", y = "Severity") +
  ggtitle("Vulnerability Clusters: Vector vs. Severity (with Labels)") +
  theme_bw()


# Display the plots
cwe_severity_plot
vector_severity_plot
cwe_severity_labels_plot
vector_severity_labels_plot

