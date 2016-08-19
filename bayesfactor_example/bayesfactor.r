library(MCMCpack)
library(ggplot2)
library(dplyr)

set.seed(3243)
theData <- rnorm(50, .3, 1)

qplot(theData)

xrange <- seq(-.2, 1, .01)
H1mu <- .4
H1sd <- .2
H0mu <- 0
H0sd <- .01
H0sdWide <- .1

sigma.mu <- 1
sigma.var <- 100
priors <- data.frame(x=rep(xrange, 3),
		     y=c(dnorm(xrange, H1mu, H1sd),
			 dnorm(xrange, H0mu, H0sd),
			 dnorm(xrange, H0mu, H0sdWide)),
		     H=rep(c('H1', 'H0', 'H0Wide'), each=length(xrange)))

ggplot(priors, aes(x=x, y=y, group=H))+
	geom_line(aes(color=H))

mean(theData)
sd(theData)
H1mod <- MCMCregress(theData~1, data=NULL, b0=H1mu, B0=1/H1sd^2,
		     sigma.mu=sigma.mu, sigma.var=sigma.var,
		     marginal.likelihood='Chib95')
summary(H1mod)
plot(H1mod)

H0mod <- MCMCregress(theData~1, data=NULL, b0=H0mu, B0=1/H0sd^2,
		     sigma.mu=sigma.mu, sigma.var=sigma.var,
		     marginal.likelihood='Chib95')
summary(H0mod)
plot(H0mod)

H0Widemod <- MCMCregress(theData~1, data=NULL, b0=H0mu, B0=1/H0sdWide^2,
		     sigma.mu=sigma.mu, sigma.var=sigma.var,
		     marginal.likelihood='Chib95')
summary(H0Widemod)
plot(H0Widemod)

aBF <- BayesFactor(H1mod, H0mod, H0Widemod)
summary(aBF)

t.test(theData)
