## Are there any clusters or communities of vendors/products that share a high number of vulnerabilities?

# Perform k-means clustering
library(cluster)

cluster_data <- all_num_clean[, c("vendor_project", "product")]

k <- 4  # Number of clusters
kmeans_model <- kmeans(cluster_data, centers = k)

cluster_assignments <- kmeans_model$cluster

cluster_counts <- table(cluster_assignments)
print(cluster_counts)

plot(cluster_data, col = cluster_assignments, pch = 16)

barplot(cluster_counts)

heatmap(table(cluster_data$vendor_project, cluster_data$product))


## Vendor Collaboration: Which vendors have collaborated on multiple projects or products? Are there any patterns or clusters of vendors working together?

# Group the data by vendor and count the number of unique projects or products
vendor_collaboration <- all_imp %>%
  group_by(vendor_project) %>%
  summarise(collaborations = n_distinct(product)) %>%
  filter(collaborations > 1) %>%
  arrange(desc(collaborations))

# Plot the vendor collaborations
ggplot(vendor_collaboration, aes(x = reorder(vendor_project, collaborations), y = collaborations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Vendor Project", y = "Number of Collaborations") +
  ggtitle("Vendors with Multiple Collaborations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# cluster analysis to identify patterns of vendor collaboration
# Compute distance matrix
dist_matrix <- dist(all_num_clean$vendor_project)

# Perform hierarchical clustering
hc <- hclust(dist_matrix)

# Cut the dendrogram to obtain clusters
clusters <- cutree(hc, k = 3)  # Adjust the value of k as needed

# Create a heatmap of the distance matrix with cluster assignments
heatmap(as.matrix(dist_matrix), Rowv = as.dendrogram(hc), Colv = NA, col = heat.colors(256),
        main = "Clustered Heatmap", labRow = "", labCol = clusters)
