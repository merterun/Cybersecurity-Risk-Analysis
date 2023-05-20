## Which vendors/products are most central in the vulnerability network? (i.e., have the most connections)
# Create an empty graph object
g <- graph(edges = numeric(0))

# Get unique vendors/products
vendors_products <- unique(all_imp$vendor_project)

# Add vendors/products as vertices to the graph
g <- add_vertices(g, length(vendors_products), name = vendors_products)

# Loop through each pair of vendors/products and count the number of vulnerabilities shared
for (i in 1:(length(vendors_products) - 1)) {
  for (j in (i + 1):length(vendors_products)) {
    vendor_product1 <- vendors_products[i]
    vendor_product2 <- vendors_products[j]
    
    # Check for missing values and exclude self-loops
    if (!is.na(vendor_product1) && !is.na(vendor_product2) && vendor_product1 != vendor_product2) {
      # Count the number of vulnerabilities shared between the pair
      shared_vulnerabilities <- sum(all_imp$vendor_project == vendor_product1 & all_imp$vendor_project == vendor_product2, na.rm = TRUE)
      
      # Add an edge between the pair with a weight equal to the number of shared vulnerabilities
      if (shared_vulnerabilities > 0) {
        g <- add_edge(g, vendor_product1, vendor_product2, weight = shared_vulnerabilities)
      }
    }
  }
}

# Calculate degree centrality
degree_centrality <- degree(g)

# Sort vendors/products based on degree centrality in descending order
sorted_vendors_products <- names(degree_centrality)[order(degree_centrality, decreasing = TRUE)]

# Print the top 5 most central vendors/products
head(sorted_vendors_products, 5)

# Assign vertex labels to vendors/products
V(g)$label <- sorted_vendors_products

# Set node size based on degree centrality
node_size <- degree_centrality * 5

# Plot the vulnerability network graph
plot(g, vertex.size = node_size, vertex.label = V(g)$label, edge.arrow.size = 0.5)


## Are there any vendors/products that are highly interconnected with other vendors/products, indicating a higher level of vulnerability?

g2 <- graph(edges = numeric(0))

for (i in 1:nrow(all_num_clean)) {
  vendor <- all_num_clean$vendor_project[i]
  product <- all_num_clean$product[i]
  
  # Add the vendor and product as vertices if they don't already exist
  if (!(vendor %in% V(g2))) {
    g2 <- add_vertices(g2, vendor)
  }
  if (!(product %in% V(g2))) {
    g2 <- add_vertices(g2, product)
  }
  
  # Add an edge between the vendor and product
  g2 <- add_edges(g2, c(vendor, product))
}


# Calculate the degree centrality
degree_centrality2 <- degree(g2)

# Plot the degree centrality distribution
hist(degree_centrality2, main = "Degree Centrality Distribution", xlab = "Degree Centrality")

plot(g2, layout = layout_with_fr(g2), vertex.size = 10, vertex.label.cex = 0.8, main = "Network Graph")
