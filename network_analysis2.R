library(igraph)

# Create an empty graph object
g <- make_empty_graph(n = length(vendors_products))

# Set the names of vertices (vendors/products)
V(g)$name <- vendors_products

# Get the connections between vendors/products
connections <- table(all_imp$vendor_project)

# Create a data frame with the connections
connections_df <- data.frame(
  vendor_project = names(connections),
  connections = as.numeric(connections)
)

# Convert vendor_project to factors with labels
connections_df$vendor_project <- factor(
  connections_df$vendor_project,
  levels = vendors_products
)

# Create a matrix of edges
edges_matrix <- matrix(
  nrow = nrow(connections_df),
  ncol = 2,
  dimnames = list(NULL, c("from", "to")),
  data = connections_df$vendor_project
)

# Add edges (connections) to the graph
g <- add_edges(
  g,
  edges = edges_matrix,
  weight = connections_df$connections
)

# Visualize the graph (optional)
plot(g)


# Calculate degree centrality
degree_centrality <- degree(g)

# Sort the vendors/products based on degree centrality in descending order
sorted_vendors_products <- sort(degree_centrality, decreasing = TRUE)

# Print the most central vendors/products
head(sorted_vendors_products)

