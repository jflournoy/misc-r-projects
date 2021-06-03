library(igraph)
library(igraphdata)
data("UKfaculty")
cluster_walktrap(UKfaculty)

n <- 60

fully_connected <- matrix(runif(n^2, 0, 1), nrow = n)

diag(fully_connected) <- 1

fully_connected[upper.tri(fully_connected)] <- 0
# fully_connected[(floor(n/2)+1):n,1:floor(n/2)] <- .09

fully_connected_graph <- graph_from_adjacency_matrix(fully_connected, mode = 'undirected', weighted = T)

plot(fully_connected_graph, edge.width=E(fully_connected_graph)$weight)

cluster_walktrap(fully_connected_graph)
cluster_infomap(fully_connected_graph)
