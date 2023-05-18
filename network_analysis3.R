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



## Are there any vendors/products that are highly interconnected with other vendors/products, indicating a higher level of vulnerability?

# Create an empty graph object
graph <- graph.empty(directed = FALSE)

# Add nodes (vendors and products) to the graph
nodes <- unique(c(all_imp$vendor_project, all_imp$product))
graph <- add.vertices(graph, length(nodes), name = nodes)

# Add edges between interconnected vendors/products
edges <- cbind(all_imp$vendor_project, all_imp$product)
graph <- add.edges(graph, edges)

degree_centrality <- degree(graph)

top_interconnected <- names(degree_centrality)[degree_centrality == max(degree_centrality)]

top_interconnected



# Install and load necessary packages
install.packages("visNetwork")
library(visNetwork)

# Create a data frame of nodes
nodes <- data.frame(id = unique(c(all_imp$vendor_project, all_imp$product)), 
                    label = unique(c(all_imp$vendor_project, all_imp$product)))

# Create a data frame of edges
edges <- data.frame(from = all_imp$vendor_project, to = all_imp$product)

# Create the visNetwork object with nodes and edges data
network <- visNetwork(nodes, edges)

# Set the network options
network <- visOptions(network, width = "100%", height = "500px")
network <- visInteraction(network, hover = TRUE, hoverConnectedEdges = TRUE)
network <- visPhysics(network, enabled = TRUE)

# Display the interactive chart
network
