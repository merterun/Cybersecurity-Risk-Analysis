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

