## Influence Analysis: Which entities (vendors, products, or vulnerabilities) have the most influence or impact within the network? Are there any central entities that are crucial for the functioning or security of the network?


# Calculate the co-occurrence matrix
co_occurrence_matrix <- table(all_imp$vendor_project, all_imp$product)

# Check the dimensions of the co-occurrence matrix
dim(co_occurrence_matrix)

# Verify the data type and convert if necessary
co_occurrence_matrix <- as.matrix(co_occurrence_matrix)

# Calculate the row and column sums of the co-occurrence matrix
row_sums <- rowSums(co_occurrence_matrix)
col_sums <- colSums(co_occurrence_matrix)

# Identify the top entities with the highest row and column sums
top_row_entities <- rownames(co_occurrence_matrix)[order(row_sums, decreasing = TRUE)[1:10]]
top_col_entities <- colnames(co_occurrence_matrix)[order(col_sums, decreasing = TRUE)[1:10]]

# Print the top entities
cat("Top entities based on row sums (vendors):\n")
top_row_entities
cat("\n")
cat("Top entities based on column sums (products):\n")
top_col_entities


##  Vulnerability Propagation: 
## Can you identify any patterns of vulnerability propagation within the network? For example, are there certain vendors/products that tend to be affected by vulnerabilities in other vendors/products?

# Group the data by affected vendor/product and count vulnerabilities
propagation_counts <- all_imp %>%
  group_by(vendor_project) %>%
  summarise(total_vulnerabilities = n()) %>%
  arrange(desc(total_vulnerabilities))

# Display the top affected vendors/products
top_affected <- head(propagation_counts, 10)

top_affected

# Create a matrix/table to show vulnerability propagation
propagation_matrix <- all_imp %>%
  group_by(vendor_project, product) %>%
  summarise(total_vulnerabilities = n(), .groups = "drop") %>%
  pivot_wider(names_from = product, values_from = total_vulnerabilities, values_fill = 0)

propagation_matrix


## Are there any “super spreader” vulnerabilities that affect multiple vendors/products?
# Count the occurrences of each vulnerability
vulnerability_counts <- table(all_imp$vulnerability_name)

# Filter for vulnerabilities that appear in more than one row
super_spreader_vulnerabilities <- names(vulnerability_counts[vulnerability_counts > 1])

# Filter the data frame to include only rows with super spreader vulnerabilities
super_spreader_data <- all_imp[all_imp$vulnerability_name %in% super_spreader_vulnerabilities, ]

# Count the occurrences of each super spreader vulnerability
super_spreader_counts <- table(super_spreader_data$vulnerability_name)

# Get the top 25 super spreader vulnerabilities
top_spreaders <- head(names(super_spreader_counts), 25)

# Filter the data frame to include only the top 25 super spreader vulnerabilities
top_spreader_data <- super_spreader_data[super_spreader_data$vulnerability_name %in% top_spreaders, ]

top_spreader_data


library(igraph)

# Create a graph object
graph <- graph.data.frame(top_spreader_data[, c("vendor_project", "vulnerability_name")], directed = FALSE)

# Set node colors and sizes
node_colors <- ifelse(V(graph)$name %in% top_spreader_data$vendor_project, "#564c53", "#baaf9b")
node_sizes <- ifelse(V(graph)$name %in% top_spreader_data$vulnerability_name, 6, 5)

# Define the layout for better spacing and avoid collisions
layout <- layout_with_fr(graph)

# Plot the network graph with labels
plot(graph, vertex.color = node_colors, vertex.size = node_sizes, edge.arrow.size = 1.5, layout = layout, vertex.label.dist = 1, vertex.label.cex = 0.7)

