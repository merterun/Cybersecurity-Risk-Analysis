## Which vendors/products are most central in the vulnerability network

# Load required library
library(igraph)

# Create an igraph object from the data frame
graph <- graph.data.frame(all_imp, directed = FALSE)

# Calculate degree centrality
degree_centrality <- degree(graph, mode = "all")

# Sort the degree centrality values in descending order
sorted_degree_centrality <- sort(degree_centrality, decreasing = TRUE)

# Display the top 10 vendors/products with the highest degree centrality
top_vendors <- names(sorted_degree_centrality)[1:10]
top_vendors



# Install and load necessary packages
library(igraph)
library(ggplot2)
library(ggrepel)
library(plotly)

# Extract relevant columns for analysis
network_data <- all_imp[, c("vendor_project", "product")]

# Create edge list
edge_list <- as.matrix(network_data)

# Create graph object
graph <- graph_from_edgelist(edge_list, directed = FALSE)

# Create layout using Fruchterman-Reingold algorithm
layout <- layout_with_fr(graph)

# Convert graph layout to data frame
layout_df <- data.frame(layout)

# Add vendor/product names to layout data frame
layout_df$names <- V(graph)$name

library(ggraph)

graph_plot <- ggraph(graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(shape = 21, fill = "skyblue", color = "black") +
  geom_node_text(aes(label = name), repel = TRUE, box.padding = 0.5, size = 3) +
  theme_void()

# Alternative with all texts displayed but there are a lot of overlaps
# graph_plot <- ggraph(graph, layout = "fr") +
#    geom_edge_link() +
#    geom_node_point(shape = 21, fill = "skyblue", color = "black") +
#    geom_node_text(aes(label = name), repel = TRUE, box.padding = 0.5, size = 3, max.overlaps = 1000) +
#    theme_void()

graph_plot





