library(lme4)
library(ggplot2)

set.seed(112358)
adf <- data.frame(wave=rep(c(1,2,3), each=100),
		  id=rep(1:100, 3),
		  score=c(rnorm(100, 0, 1), 
			  rnorm(100, 2, 1),
			  rnorm(100, -10, 1)))

summary(adf)

ggplot(adf, aes(x=wave, y=score, group=id)) +
	geom_point(aes(color=id))+
	geom_line(aes(color=id))

amod <- lmer(score ~ 1 + (1|id) + (1|wave), data=adf)
summary(amod)

var.id <- VarCorr(amod)$id["(Intercept)", "(Intercept)"]
var.wave <- VarCorr(amod)$wave["(Intercept)", "(Intercept)"]
residual <- sigma(amod)
total.var <- var.id+var.wave+residual

(id.icc <- var.id/total.var) #basically nothing
(wave.icc <- var.wave/total.var) #all of it
