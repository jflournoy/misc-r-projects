library(tidyverse)

get_p_value <- function(eff_size = 0, n = 20){
  sample <- rnorm(n, eff_size, 1)
  test <- t.test(sample)
  return(test$p.value)
}

get_p_dist <- function(n = 20, iter = 1000, eff_size = 0){
  return(replicate(iter, get_p_value(eff_size, n)))
}

sample_sizes <- seq(20, 160, 20)
niter = 5000
h0 <- 0
h1_d <- .2 #Approximately r = .1 -- not big, but probably big enough to care about as a confound.

manydists <- sapply(as.character(sample_sizes), get_p_dist, iter = niter, eff_size = h0, USE.NAMES=T)
head(manydists)

manydists_l <- manydists %>% 
  as_data_frame %>%
  gather(N, p_value) 
  
null_p_dist_graph <- ggplot(manydists_l, aes(x = p_value)) +
  geom_histogram(aes(group = N,
                     fill = as.numeric(N),
                     color = as.numeric(N)),
                 alpha = .3, binwidth = .01,
                 position = position_identity()) +
  labs(color = 'N', fill = 'N', x = 'p value', y = 'Number of experiments') +
  lims(x=c(1e-10, 1-1e-10))

manydists_small_effect <- sapply(as.character(sample_sizes), get_p_dist, iter = niter, eff_size = h1_d, USE.NAMES=T)
head(manydists_small_effect)

manydists_small_effect_l <- manydists_small_effect %>% 
  as_data_frame %>%
  gather(N, p_value)

ggplot(manydists_small_effect_l, aes(x = p_value)) +
  geom_histogram(aes(y = ..density..,
                     group = N,
                     fill = as.numeric(N)
                     # color = as.numeric(N)
                     ),
                 alpha = .1, binwidth = .01,
                 position = position_identity())

alldists_l <- bind_rows("true" = manydists_l, "false" = manydists_small_effect_l, .id = "h0")

N_labeller <- function(labels){
  labels <- lapply(labels, function(x){
    x <- as.character(x)
    x[1] <- paste0('N = ', x[1])
    x
  })
  return(labels)
}

library(wesanderson)
colors <- wes_palette('Royal1')
proportions_graph <- alldists_l %>%
  mutate(N = factor(N, levels = sort(as.numeric(unique(N)))),
         choice = c('retain h0', 'reject h0')[1+(p_value < .05)]) %>%
  ggplot(aes(x = h0)) +
  geom_bar(aes(y = ..count.., fill = interaction(choice, h0)),
                 alpha = 1, 
                 position = position_fill()) +
  scale_fill_manual(breaks = c('retain h0.false',
                               'retain h0.true',
                               'reject h0.false', 
                               'reject h0.true'), 
                    labels = c('Type II Error',
                               'Correct Acceptance',
                               'Correct Rejection', 
                               'Type I Error'),
                    values = c(colors[3], colors[4], colors[2], colors[1])) + 
  geom_hline(yintercept = .2, linetype = 1) +
  geom_hline(yintercept = .05, linetype = 2) +
  scale_y_continuous(breaks = c(1, .95, .2, .05, 0)) +
  facet_grid(~N, scales = 'free_x', labeller = N_labeller)+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = 'Proportion of 5,000 experiments', x= paste0('d=', h1_d, ' versus d=0'),
       fill = 'Your decision about\nH0 will be a...')+
  coord_cartesian(y = c(0,1))

library(gridExtra)
grid.arrange(null_p_dist_graph, proportions_graph, ncol = 2)
