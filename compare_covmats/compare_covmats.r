library(lavaan)
library(MASS)

#' Set seed so this is reproducible
set.seed(534435)
#' create two distinct population level correlation matrices
cor1 <- matrix(c(1.00,  .20, -.20,  .60,
		  .20, 1.00,  .05,  .30,
		 -.20,  .05, 1.00, -.40,
		  .60,  .30, -.40, 1.00), nrow=4)
cor2 <- matrix(c(1.00,  .50,  .15, -.30,
		  .50, 1.00, -.25,  .60,
		  .15, -.25, 1.00, -.05,
		 -.30,  .60, -.05, 1.00), nrow=4)

#' Randomly draw 100 observations from each correlation matrix using multi-variate
#'  normal distributions with mean set to 0 for all 4 variables. The mean doesn't
#'  matter for our purposes.
#' 
#' Quick note: it is possible to give lavaan the correlation matrix as data rather than
#'  the observations directly: http://lavaan.ugent.be/tutorial/cov.html
matG1 <- mvrnorm(100, mu=rep(0, 4), Sigma=cor1)
matG2 <- mvrnorm(100, mu=rep(0, 4), Sigma=cor2)

#'  Check observed correlation matrices
cor(matG1)
cor(matG2)

#' Create a data frame from the above observations with group membership noted.
aDF <- data.frame(rbind(matG1, matG2), group=rep(c('one', 'two'), each=100))

cor(aDF[, 1:4])

#' Create lavaan model that is just the full correlation matrix
corVarNames <- names(aDF)[1:4]

#' Here we generate the lavaan model code -- we want every var name in corVarNames
#'  to be covaried ("~~") with itself and with all other variables
#' 
#' First, we get each pairing:
allPairs <- outer(corVarNames, corVarNames, function (x,y) paste(x,"~~",y))
print(allPairs)

#' And then we just want the lower triangle with the diagonal:
allPairs.lower <- allPairs[lower.tri(allPairs, diag=T)]

#' And then collapse it into lavaan code:
covModel <- paste(allPairs.lower, collapse='\n')
cat(c(covModel, '\n'))

#' Now we can run the model with and without the grouping variable and compare model fit:
#'  We can set meanstructure=T or F in both models -- we want them to be the same
#'  and it doesn't seem like we care about means being the same or different between
#'  the groups.
fit.homogeneity <- lavaan(model=covModel, data=aDF, meanstructure=F) 
fit.grouped <- lavaan(model=covModel, data=aDF, group='group', meanstructure=F)

#' Here we see that distinguishing observations drawn from the multivariate normal 
#'  distributions with the two  different correlation matrices is the preferred model. 
n2llDiff <- -2*logLik(fit.homogeneity) - -2*logLik(fit.grouped) 
print(n2llDiff)
pchisq(as.numeric(n2llDiff),  df=attr(n2llDiff, 'df'), lower.tail=F)

#' The AIC and BIC tell a similar story -- a whopping difference in fit
AIC(fit.homogeneity)-AIC(fit.grouped)
BIC(fit.homogeneity)-BIC(fit.grouped)

#' Inspect the results, and compare to the results above from `cor` if you wish:
summary(fit.homogeneity)
summary(fit.grouped)

#' We should confirm that grouping doesn't give us spurious better fit, so lets
#'  draw 200 observations from a single multivariate normal distribution and see
#'  if arbitrary grouping gives us a better fitting model:

mat.homogeneous <- mvrnorm(200, mu=rep(0, 4), Sigma=cor1)
aDF.h <- data.frame(mat.homogeneous, group=rep(c('one', 'two'), each=100))

print(cor1)
cor(aDF.h[, 1:4])

fit.homogeneity.h <- lavaan(model=covModel, data=aDF.h, meanstructure=F) 
fit.grouped.h <- lavaan(model=covModel, data=aDF.h, group='group', meanstructure=F)

#' Interestingly, here you do get a little bit better fit just by breaking up
#'  the observations. This is perhaps a nice demonstration that just adding
#'  parameters for no reason can give you better fit with the likelihood ratio
#'  test (but the p value is still > .05). I *have* run this code with
#'  a different seed and seen improved fit by adding an arbitrary grouping, 
#'  but hopefully this shouldn't happen very often. This will 
#'  be the topic of a future snippet.
n2llDiff.h <- -2*logLik(fit.homogeneity.h) - -2*logLik(fit.grouped.h) 
print(n2llDiff.h)
pchisq(as.numeric(n2llDiff.h),  df=attr(n2llDiff.h, 'df'), lower.tail=F)

#' The AIC and BIC attenuate the effect of just adding parameters, and they don't
#'  show an improvement of fit at all: the AIC and BIC for the homogeneity model
#'  are both smaller -- especially the BIC that really penalizes parameters.
AIC(fit.homogeneity.h)-AIC(fit.grouped.h)
BIC(fit.homogeneity.h)-BIC(fit.grouped.h)

summary(fit.homogeneity.h)
summary(fit.grouped.h)
