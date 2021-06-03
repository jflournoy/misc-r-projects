library(psych)
library(data.table)
library(igraph)
library(ggplot2)

data(bfi)

names(bfi)

EFA_stability <- function(x, N, k){
  x_sub <- x[sample(1:dim(x)[[1]], N), ]
  
  fa <- fa(x_sub, nfactors = k, rotate="oblimin", scores=TRUE, fm = "ml") 
  
  #just turn it into a data.table
  d <- as.data.table(lapply(1:dim(fa$loadings)[[2]], function(i) return(as.numeric(fa$loadings[,i]))))
  d$item <- dimnames(fa$loadings)[[1]]
  d_l <- data.table::melt(d, id.vars = 'item', variable.name = 'factor_name')
  
  d_sum <- d_l[, .(maxfactor = factor_name[which(abs(value) == max(abs(value)))],
                   maxloading = value[which(abs(value) == max(abs(value)))]), by = 'item']
  d_adj <- crossprod(t(table(d_sum[, 1:2])))
  
  return(list(primary_loadings = d_sum, adjacency = d_adj))
}

#check the function
r <- EFA_stability(bfi[, 1:25], 300, 5)

#do many many samples
iter <- 1000
many_r <- parallel::mclapply(1:iter, function(i) {
  EFA_stability(bfi[, 1:25], 300, 5)
}, mc.cores = 7)

#Create an adjacency matrix to answer the question for each item, how often does
#that item load on the same factor as other items. We will see which items end
#up switching between different factors.

#this is just collapsing the 1000 adjaceny matrices
many_a <- lapply(many_r, `[[`, 'adjacency')
many_a_array <- array(as.numeric(unlist(many_a)), dim=c(25, 25, iter))
a_array <- apply(many_a_array, c(1,2), sum)
dimnames(a_array) <- list(names(bfi)[1:25], names(bfi)[1:25])

#create a network graph
g <- igraph::graph_from_adjacency_matrix(a_array, mode = 'undirected', diag = FALSE, weighted = TRUE)
V(g)$scale <- gsub('(\\w)\\d', '\\1', V(g)$name)
pallet <- c('A' = 'red', 'C' = 'blue', 'E' = 'green', 'N' = 'gray', 'O' = 'yellow')
V(g)$color <- pallet[V(g)$scale]

#+fig.width=10, fig.height=10
plot(g, vertex.label=V(g)$name, 
     margin = 0, 
     edge.width = E(g)$weight/(iter/10), 
     edge.color = 'black',
     vertex.label.color = 'black', 
     vertex.size = 10)
#Each edge is weighted by how many times an item loads onto the same factor as
#other items.

#Now we can look at consistency of max loadings.

many_loadings <- lapply(many_r, `[[`, 'primary_loadings')
many_loadings_dt <- data.table::rbindlist(many_loadings)

#+fig.width=10, fig.height=10
ggplot(many_loadings_dt, aes(x = abs(maxloading))) + 
  geom_vline(xintercept = .5, color = 'gray') + 
  geom_density(fill = 'red', color = 'darkred') + 
  facet_wrap(~ item, nrow = 5) + 
  coord_cartesian(x = c(0, 1)) + 
  scale_x_continuous(breaks = c(0, .5, 1), labels = c('0', '.5', '1')) +
  theme_minimal() + 
  theme(axis.text.y = element_blank()) + 
  labs(x = 'Maximum Loading', y = '')
