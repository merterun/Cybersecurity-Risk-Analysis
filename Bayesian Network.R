# Install the necessary packages
library(bnlearn)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")

library(Rgraphviz)

# Learn the structure of the Bayesian network using the dataset
bn <- hc(all_num_clean)


g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(bn))
graph::nodeRenderInfo(g) <- list(fontsize=100)
Rgraphviz::renderGraph(g)


#CRITICAL = 1, HIGH = 2, LOW = 3, MEDIUM 4