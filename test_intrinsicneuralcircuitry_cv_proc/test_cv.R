n=174
d=780
n_90=n-n%/%10
crit.t <- qt(.025, n_90-2, lower.tail = F)
crit.r <- sqrt(crit.t^2 / (crit.t^2 + n_90-2))

get_a_cor <- function(crit.r, test_i, data, shuffle_when, ztor = FALSE){
    tenth <- length(test_i)
    if(ztor){
        mat_X <- data[,-1]
    } else {
        mat_X <- psych::fisherz2r(data[,-1])
    }
    test_mat_X <- mat_X[test_i,-1]
    test_mat_y <- data[test_i,1]
    train_mat_X <- mat_X[-test_i,-1]
    train_mat_y <- data[-test_i,1]
    if(shuffle_when %in% c('before', 'both')){
        train_mat_y <- sample(train_mat_y, replace = F)
    }
    cors <- apply(train_mat_X, 2, cor, train_mat_y)
    sigpos <- which(cors > crit.r)
    signeg <- which(cors < -crit.r)
    netstr_train_X <- cbind(pos = rowSums(train_mat_X[,sigpos]), neg = rowSums(train_mat_X[,signeg]))
    netstr_test_X <- cbind(pos = rowSums(test_mat_X[,sigpos]), neg = rowSums(test_mat_X[,signeg]))
    if(shuffle_when %in% c('after', 'both')){
        train_mat_y <- sample(train_mat_y, replace = F)
    }
    amod <- glm(train_mat_y ~ netstr_train_X)
    test_mat_yhat <- cbind(rep(1, tenth), netstr_test_X)%*%coef(amod)
    return(cor(test_mat_yhat, test_mat_y))
}

cv_cors <- function(...){
    fold_i <- sample(rep_len(sample(1:10), length.out = n))
    cv_cors <- lapply(1:10, function(i){
        get_a_cor(test_i = which(fold_i == i), ...)
    })
    return(mean(unlist(cv_cors)))
}

shuffle_never_fn <- 'Intrinsic_neural_circuitry-noshuffle.RDS'
if(!file.exists(shuffle_never_fn)){
    library(parallel)
    manycors_shuffle_never <- mclapply(1:(7*3), function(i){
        replicate(50, {
            nulldata <- mvnfast::rmvn(n=n, mu = rep(0, d), sigma = diag(d))
            cv_cors(crit.r, data = nulldata, shuffle_when = 'never')
        })
    }, mc.cores = 7)
    saveRDS(manycors_shuffle_never, shuffle_never_fn)
} else {
    manycors_shuffle_never <- readRDS(shuffle_never_fn)
}
hist(unlist(manycors_shuffle_never), breaks = 100)
round(quantile(unlist(manycors_shuffle_never), probs = c(.025, .5, .975)), 2)

shuffle_after_fn <- 'Intrinsic_neural_circuitry-shuffleafter.RDS'
if(!file.exists(shuffle_after_fn)){
    library(parallel)
    nulldata <- mvnfast::rmvn(n=n, mu = rep(0, d), sigma = diag(d))
    manycors_shuffle_after <- mclapply(1:(7*3), function(i){
        replicate(50, {
            cv_cors(crit.r, data = nulldata, shuffle_when = 'after')
        })
    }, mc.cores = 7)
    saveRDS(manycors_shuffle_after, shuffle_after_fn)
} else {
    manycors_shuffle_after <- readRDS(shuffle_after_fn)
}
hist(unlist(manycors_shuffle_after), breaks = 100)
round(quantile(unlist(manycors_shuffle_after), probs = c(.025, .5, .975)), 2)

shuffle_before_fn <- 'Intrinsic_neural_circuitry-shufflebefore.RDS'
if(!file.exists(shuffle_before_fn)){
    library(parallel)
    nulldata <- mvnfast::rmvn(n=n, mu = rep(0, d), sigma = diag(d))
    manycors_shuffle_before <- mclapply(1:(7*3), function(i){
        replicate(50, {
            cv_cors(crit.r, data = nulldata, shuffle_when = 'before')
        })
    }, mc.cores = 7)
    saveRDS(manycors_shuffle_before, shuffle_before_fn)
} else {
    manycors_shuffle_before <- readRDS(shuffle_before_fn)
}
hist(unlist(manycors_shuffle_before), breaks = 100)
round(quantile(unlist(manycors_shuffle_before), probs = c(.025, .5, .975)),2)

shuffle_before_newdat_fn <- 'Intrinsic_neural_circuitry-shufflebeforenewdat.RDS'
if(!file.exists(shuffle_before_newdat_fn)){
    library(parallel)
    manycors_shuffle_before_newdat <- mclapply(1:(7*3), function(i){
        replicate(50, {
            nulldata <- mvnfast::rmvn(n=n, mu = rep(0, d), sigma = diag(d))
            cv_cors(crit.r, data = nulldata, shuffle_when = 'before')
        })
    }, mc.cores = 7)
    saveRDS(manycors_shuffle_before_newdat, shuffle_before_newdat_fn)
} else {
    manycors_shuffle_before_newdat <- readRDS(shuffle_before_newdat_fn)
}
hist(unlist(manycors_shuffle_before_newdat), breaks = 100)
round(quantile(unlist(manycors_shuffle_before_newdat), probs = c(.025, .5, .975)),2)

shuffle_both_fn <- 'Intrinsic_neural_circuitry-shuffleboth.RDS'
if(!file.exists(shuffle_both_fn)){
    library(parallel)
    nulldata <- mvnfast::rmvn(n=n, mu = rep(0, d), sigma = diag(d))
    manycors_shuffle_both <- mclapply(1:(7*3), function(i){
        replicate(50, {
            cv_cors(crit.r, data = nulldata, shuffle_when = 'both')
        })
    }, mc.cores = 7)
    saveRDS(manycors_shuffle_both, shuffle_both_fn)
} else {
    manycors_shuffle_both <- readRDS(shuffle_both_fn)
}
hist(unlist(manycors_shuffle_both), breaks = 100)
quantile(unlist(manycors_shuffle_both), probs = c(.025, .5, .975))

shuffle_never_r_fn <- 'Intrinsic_neural_circuitry-noshuffle-r.RDS'
if(!file.exists(shuffle_never_r_fn)){
    library(parallel)
    manycors_shuffle_never_r <- mclapply(1:(7*3), function(i){
        replicate(50, {
            nulldata <- mvnfast::rmvn(n=n, mu = rep(0, d), sigma = diag(d))
            cv_cors(crit.r, data = nulldata, shuffle_when = 'never', ztor = TRUE)
        })
    }, mc.cores = 7)
    saveRDS(manycors_shuffle_never_r, shuffle_never_r_fn)
} else {
    manycors_shuffle_never_r <- readRDS(shuffle_never_r_fn)
}
hist(unlist(manycors_shuffle_never_r), breaks = 100)
quantile(unlist(manycors_shuffle_never_r), probs = c(.025, .5, .975))
