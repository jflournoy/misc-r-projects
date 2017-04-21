library(MCMCpack)
library(ggplot2)

#draw a sample from something with true d=.3
set.seed(11232)
someData <- rnorm(50, .3, 1)
mean(someData)
sd(someData)
summary(lm(someData~1))
qplot(someData)

#I'll start with an optimistic prior where d=.4, and precision B0 is 1/Var with Var=SD^2. Here I'll
# use SD=.2, so B0=1/.2^2=25. We also have to put a prior on the parameter disturbances, so I'll
# set it to what we know, which is that the population disturbance should be 1, but I'll 
# give it a wide default variance of 25 (which corresponds to a SD of 5). 
modH1 <- MCMCregress(someData ~ 1, data=NULL, b0=.4, B0=25, sigma.mu=1, sigma.var=25)	
x=seq(-.2, 1, .01), y=dnorm(seq(-.2, 1, .01), .4, .2)
#visualize priors

qplot(, geom='line')

plot(modH1)
summary(modH1)


