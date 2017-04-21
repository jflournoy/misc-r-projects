library(tidyverse)
library(data.table)
library(knitr)
get_poisson_data <- function(n, lambda1, lambda2=NA){
	if(is.na(lambda2)){
		return(data.frame(g1=rpois(n, lambda1)))
	} else {
		return(data.frame(g1=rpois(n, lambda=lambda1),
				  g2=rpois(n, lambda=lambda2)))
	}
}

two_sample_p <- function(n, lambda1, lambda2){
	afunction <- function(){
		asample <- get_poisson_data(n, lambda1, lambda2)
		test_result <- t.test(asample[,1], asample[,2])
		return(data.frame(est=diff(test_result$estimate),
				  p=unique(test_result$p.value)))
	}
}

one_sample_p <- function(n, lambda1, mu=0){
	afunction <- function(){
		asample <- get_poisson_data(n, lambda1)
		test_result <- t.test(asample[,1], mu=mu)
		return(data.frame(est=test_result$estimate,
				  p=test_result$p.value))
	}
}
poisson_regression <- function(n, beta0, beta1){
	afunction <- function(){
		beta1_ <- rnorm(n, beta1, 1)
		x_vec <- runif(n, -1, 1)
		lambda <- exp(beta0 + beta1*x_vec)
		y_vec <- rpois(n=n, lambda=lambda)
		amodSum <- summary(lm(y_vec~1+x_vec))
		p <- amodSum$coefficients['x_vec', 'Pr(>|t|)']
		est <- amodSum$coefficients['x_vec', 'Estimate']
		return(data.frame(est=est, p=p))
	}
}
test_k_samples <- function(iter, afunction, multicore=T, mc.cores=NA){
	if(multicore){
		require(parallel)
		if(is.na(mc.cores)){
			mc.cores <- detectCores()
		}
		iters <- 1:iter
		p_values <- mclapply(iters, function(x) afunction(), mc.cores=mc.cores)
		return(p_values)
	} else {
		iters <- 1:iter
		p_values <- lapply(iters, function(x) afunction())
		return(p_values)
	}
}

n=100

system.time({
	twoSampleTest <- test_k_samples(100000, 
					afunction=two_sample_p(n, lambda1=1, lambda2=1), 
					multicore=T)
})

twoSampleTestDF <- rbindlist(twoSampleTest)

qplot(twoSampleTestDF$p, binwidth=1/50)
qplot(twoSampleTestDF$est, binwidth=1/50)

twoSampleTestDF %>%
	summarize(power=sum(p<.05)/n(),
		  mean_est=mean(est),
		  sd_est=sd(est)) %>%
	kable()

system.time({
	oneSampleTest <- test_k_samples(100000, afunction=one_sample_p(n, lambda1=1, mu=1))
})

oneSampleTestDF <- rbindlist(oneSampleTest)

qplot(oneSampleTestDF$p, binwidth=1/50)
qplot(oneSampleTestDF$est, binwidth=1/50)

oneSampleTestDF %>%
	summarize(power=sum(p<.05)/n(),
		  mean_est=mean(est),
		  sd_est=sd(est)) %>%
	kable()

#'
#' for $Y$ and uses the $\log$ link function. So, for a single explanatory variable $x$, it is assumed that $Y \sim P(\mu)$ (so that $E(Y) = V(Y) = \mu$) and that $\log(\mu) = \beta_0 + \beta_1 x$.
#'

system.time({
	regTest <- test_k_samples(100000, 
				  afunction=poisson_regression(n, 1, 0)) 
})

regTestDF <- rbindlist(regTest)

qplot(regTestDF$p, binwidth=1/50)
qplot(regTestDF$est, binwidth=1/50)

regTestDF %>%
	summarize(power=sum(p<.05)/n(),
		  mean_est=mean(est),
		  sd_est=sd(est)) %>%
	kable()

beta0 <- 1
beta1 <- rnorm(n, 0, 1)
x_vec <- runif(n, -1, 1)
lambda <- exp(beta0 + beta1*x_vec)
y_vec <- rpois(n=n, lambda=lambda)
summary(lm(y_vec~1+x_vec))
qplot(x_vec, y_vec)
