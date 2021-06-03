#' ---
#' title: Regression, correlation, partial correlation
#' author: jcf
#' date: '`r print('')`'
#' output:
#'   tufte::tufte_handout:
#'     citation_package: natbib
#'     latex_engine: xelatex
#' ---
#install.packages(c('lm.beta','ppcor'))
#+message=F
library(lm.beta);library(ppcor);library(tidyverse);library(broom);library(knitr)
set.seed(1000)
#+fig.margin=T
#Create fake data, and a situation in which one or two variables cause an outcome
a = rnorm(1e3,0,1)
b = rnorm(1e3,5,4)
y1 = .5*a + rnorm(1e3, 0, 1)
y2 = .5*a + .5*b + rnorm(1e3,0,1)


#Correlation is equivalent to standardized 
#+results='asis'
cor(y1,a) %>% tidy %>% kable %>% tufte::margin_note()
lm.beta(lm(y1~1+a)) %>% tidy %>% kable 

#You can "control" for a third variable two different ways: partial correlation or regression
pcor.test(y2,a,b)

#The standardized regression estimate is different than the estimate as pcor (.4640914)
lm.beta(lm(y2~1+a+b))

#This is because a partial correlation is the correlation between y2 and a when
#the variable b has been regressed out of each of them.
axb <- residuals(lm(a~b)) #a with b partialed out
y2xb <- residuals(lm(y2~b)) #y2 with b partialed out
cor(axb,y2xb) #matches pcor
