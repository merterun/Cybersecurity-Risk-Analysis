# Install the necessary packages
library(bnlearn)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")

library(Rgraphviz)

# Learn the structure of the Bayesian network
graphviz.plot(bn, main = "Bayesian Network")

plot(bn)

#CRITICAL = 1, HIGH = 2, LOW = 3, MEDIUM 4

