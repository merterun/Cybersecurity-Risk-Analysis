library(igraph)

# Create an edge list
edges <- data.frame(source = all_imp$vendor_project, target = all_imp$product)

# Remove missing values or empty strings
edges <- na.omit(edges)
edges <- edges[edges$source != "", ]
edges <- edges[edges$target != "", ]

# Create a graph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Calculate degree centrality
centrality <- degree(g)

# Get the nodes with the highest degree centrality
most_central <- names(centrality)[centrality == max(centrality)]

# Set up the plot parameters
plot(g, vertex.size = 10, vertex.label.cex = 0.6, edge.arrow.size = 0.5, edge.width = 0.5)

# Calculate the layout
layout <- layout_with_fr(g)

# Plot with adjusted layout
plot(g, layout = layout, vertex.size = 10, vertex.label.cex = 0.6, edge.arrow.size = 0.5, edge.width = 0.5)




library(networkD3)
# Convert the igraph object to a networkD3 object
nd <- igraph_to_networkD3(g)
# Create an interactive plot
forceNetwork(Links = nd$links, Nodes = nd$nodes, Source = "source", Target = "target",
             NodeID = "name", width = 800, height = 600,
             opacity = 0.8, linkDistance = 100,
             bounded = TRUE, linkColour = "#999999", colourScale = JS("d3.scaleOrdinal().range(['#66CCFF'])"),
             legend = TRUE)
