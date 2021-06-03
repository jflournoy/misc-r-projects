#'
#' # Simulate the data
#'
#' I'll examine the case where one variable is truly different from 0, with mean = .2 (SD = 1), and the other is truly 0 (SD = 1), using repeated t.tests using sample size of 69.
#'

library(MASS)
do_test <- function(N, means = c(0, 0)){
    cormat <- diag(2)
    some_data <- mvrnorm(n = N, mu = means, Sigma = cormat)
    ttest_diff <- t.test(some_data[,1], some_data[,2], paired = T)
    ttest_v1 <- t.test(some_data[,1], paired = F)
    ttest_v2 <- t.test(some_data[,2], paired = F)
    arow <- c(lapply(list(ttest_diff, ttest_v1, ttest_v2), function(x) x$p.value),
              lapply(list(ttest_diff, ttest_v1, ttest_v2), function(x) x$estimate))
    names(arow) <- unlist(lapply(c('p', 'mean'), paste, c('diff', 'v1', 'v2'), sep = '_'))
    return(as.data.frame(arow, row.names = 1))
}

library(future)
library(listenv)
plan(list(tweak(multicore, workers = 8)))

rezList <- listenv()

for(i in 1:(8*4)){
    rezList[[i]] <- future(
        dplyr::bind_rows(
            replicate(1000, 
                      do_test(N = 69, means = c(.2, 0)), 
                      simplify = F))
        )
}

rezListList <- lapply(as.list(rezList), value)
test_df <- dplyr::bind_rows(rezListList)

#'
#' # Examine selection effects
#'
#' ## Statistics under the null
#' 
#' We should see mean p value for v2 of .5 , and mean of v2 means close to 0.
sapply(test_df, mean)

#'
#' ## Type I error, no selection
#' 
#' The proportion for significant effects for p_diff and p_v1 is power (because we know there are real differences from 0 for the difference and the mean of v1), and the proportion significant for p_v2 is the observed type 1 error rate, and so should be .05.
#' 
sapply(test_df[,1:3], function(acol) mean(acol < .05))

#'
#' ## Type I error, with selection on sig diff
#' 
#' We see the error rate is inflated for deciding the mean of v2 is different from 0.
#' 
sapply(test_df[test_df$p_diff < .05,1:3], function(acol) mean(acol < .05))

#'
#' ## Unbiased estimates
#' 
#' Note the difference in where the center of mass is for these estimates versus the estimates after selection on significance.
#' 
nothing <- lapply(sapply(test_df[,4:6], hist, plot = F, simplify = F), function(x) {plot(x); cat('\n\n\n')})

#'
#' ## Biased estimates
#' 
nothing <- lapply(sapply(test_df[test_df$p_diff < .05,4:6], hist, plot = F, simplify = F), function(x) {plot(x); cat('\n\n\n')})

