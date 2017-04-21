library(lavaan)
library(stringr)
library(simsem)
library(semPlot)

to_optimize_full_model_residuals <- function(residual_vector, dgm, return_model=F){
  require(stringr) #we use stringr to easily substitute the residual values in for the path notation, e.g., `r1` using `str_replace_all`
  #
  #This function takes a vector of values to set residuals, then applies those settings
  #to the data generating model in `dgm`. It returns the sum of the squared differences
  #between the unstandardize and standardized path weigts.
  #
  #For each element of residual_vector, it will try to replace `r[0-9]` in teh
  #dgm text with that number.
  
  #how many residual values do we need to find?
  len_resid_vec <- length(residual_vector)
  #for every residual value, sub in for paths marked with "r1" ... "rn" in the model dgm syntax
  names(residual_vector) <- paste0('r', 1:len_resid_vec) 
  #do the substitution
  dgm_resid <- str_replace_all(dgm, residual_vector)
  
  #fit the model without data so we can access the standardized path weights
  afit <- sem(dgm_resid)
  #calculate the sum of squared differences in the beta matrix
  ss_beta_diffs <- sum((inspect(afit, what='est')$beta-inspect(afit, what='std')$beta)^2)
  
  #sometimes we may already know the optimal residual weights. In that case we just want this function to return
  #the model syntax and also tell us the values for both standardized and unstandardized paths so we can
  #confirm this is correct.
  if(return_model){
    cat("Starting Values Beta Matrix\n")
    print(inspect(afit, what='est')$beta)
    cat("Std Values Beta Matrix\n")
    print(inspect(afit, what='std')$beta)
    cat("Sum of the squared differences\n")
    print(sum((inspect(afit, what='est')$beta-inspect(afit, what='std')$beta)^2))
    return(dgm_resid)
  } else {
    return(ss_beta_diffs)
  }
}

### MEDIATION MODEL

model1 <- "
z ~ b*y
y ~ a*x
z ~~ r1*z
y ~~ r2*y
"

#Define the path weights
path_weights <- data.frame(
  a=c(.4),
  b=c(.8)
)

#Apply the path weights to the model
dgm <- str_replace_all(model1, path_weights[1,])

optimizedResidual <- optim(c(.5, .5), to_optimize_full_model_residuals, dgm = dgm, lower=.001, upper=1, method='L-BFGS-B')
dgm_resid <- to_optimize_full_model_residuals(optimizedResidual$par, dgm=dgm, return_model=T)

cat(dgm_resid)

aSim <- sim(nRep = 1, model = model1, generate = dgm_resid, n=10000, dataOnly = T)
summary(sim(nRep = 1, model = model1, generate = dgm_resid, n=10000, dataOnly = F))

class(aSim[[1]])

library(ggplot2)

agraph <- semPaths(dgm_resid, layout=igraph::layout_with_kk, whatLabels = 'est', nCharNodes=6)

ggplot(aSim[[1]], aes(x=x, y=y))+
  geom_point(aes(color=z))


### MODERATION MODEL
N = 10000
# Moderation has a simpler structure so we can just build a regression equation

#2 variabls, with standardized covariance structure
#[x    y   ]
#[1.00 0.25]
#[0.25 1.00]
library(MASS)
xycormat <- matrix(c(1.00, 0.25, 0.25, 1.00), nrow=2)
xymeans <- c(0, 0) #centered

moderationDF <- as.data.frame(mvrnorm(n = N, mu = xymeans, Sigma = xycormat, empirical = T))
names(moderationDF) <- c('x', 'y')
cov(moderationDF)

moderationDF$z <- scale(rnorm(n=N, mean = with(moderationDF, 0*x + 0*y + .5*x*y), sd = 1))

cov(moderationDF)

summary(lm(z~x*y, data=moderationDF))

ggplot(moderationDF, aes(x=x, y=y))+
  geom_point(aes(color=z), alpha=.8)

