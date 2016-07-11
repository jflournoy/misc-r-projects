#'---
#' author: john
#' title: False Positive Rate and FWE
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#'---
#'
#'Use this to look at how false positive rate for 1 sample
#' t.test changes with a bunch of parameters.
#'
#' # False positive rate
#'
#' A quick definition: the FPR is the number of false positives (Type-I, rejection
#' of the null when H0=T) out of the total number of positives (Type-I errors + true positives).
#'
#' In math: 
#'
#' $$\text{fp.rate} = \frac{\text{fp}}{\text{fp} + \text{tp}}$$
#'
#' where **fp** is the number of false positives, and **tp** is
#' the number of true positives *that we are able to detect* (more on that below).
#'
#' # Simulating false positives
#'
#' A preview of the heart of the simulation below. Basically, we're going to look
#' at how the false positive rate changes when you perform a bunch of study replications (`iter`)
#' each with some number of tests (`tests`, which you can think of as the number of voxels),
#' for a particular sample size (`N` observations, i.e., participants, per study). For some
#' proportion of the tests, the null hypothesis is actually true (that proportion is `pH0`), 
#' and for some, the alternative is true (`pH1`). Here's the heart of the simulations:
#'
#'```r
#'replicate(iter, data.frame(fp=sum(replicate(round(pH0*tests),
#'                                            t.test(rnorm(N, 0, 1))$p.value<alpha)),
#'                           tp=sum(replicate(round(pH1*tests),
#'                                            t.test(rnorm(N, d, 1))$p.value<alpha)))
#'```
#'
#' I'll gradually build up the above code, from the inside out, following `fp=` (I'll
#' describe the code following `tp=` below: 
#'
#' 1. We draw *N* random numbers from a normal distribution centered at 0 
#' (our null hypothesis): **`rnorm(N, 0, 1)`**
#' 2. We then calculate a 1 sample *t*-test to see what the probability is
#' that we would get a set of values like those we drew in (1) if we were drawning
#' them from a distribution with a mean that is not equal to 0. That is our *p*-value.
#' We check if that *p*-value is < alpha: **`t.test(`**`rnorm(N, 0, 1)`**`)$p.value<alpha`**
#' 3. We replicate the above for however many of the tests in `tests` are supposed
#' to actually have H0=T (`pH0*tests`). So if we have 100 tests, and 90% are
#' really null effects, we'll replication the code in (1-2) 90 times. Putting it all
#' together gives us: 
#'     ```
#'     replicate(round(pH0*tests), t.test(rnorm(N, 0, 1))$p.value<alpha))
#'     ```
#' 4. We don't save the value of every replicated test out of those 90 -- we just want
#' to know how many false positives we get -- so we use `sum` and save that in the `fp` column.
#' In other words, `fp` will become a column where each row is the number of false positives
#' for 1 of the `iter` studies we're simulating.
#' 5. Skipping over `tp=` for now, suffice it to say that we want to simulate a bunch
#' (however many in `iter`) of studies. So we're going to use `replicate` to wrap
#' the whole thing, so that for each study in `iter` we create a data.frame that
#' has a count of the false positives for that simulated study, as well as a count of the
#' true positives.
#' 
#' On average (e.g., over 1000 repetitions), about
#' 5% of the tests where H0=T in any study should be significant (if alpha=.05). 
#' Any given study
#' may have more or less than 5% of the tests of true-negative voxels showing activation,
#' but over time, the expectation is that only 5% of the tests will give a false positive.
#'
#' ## FWE
#'
#' When we control the family-wise error rate, we're not
#' trying to just control the error rate for each test in a particular study.
#' If we were okay with any given activation being a false positive about 5% of the time, 
#' that would be a fine thing to do. What we're actually going for with FWE is to
#' control the rate at which we see at least 1 false activation *out of all the tests*. 
#' That is, for
#' some number of tests per study, say 100, we only want 5% of all studies (each with 
#' multiple tests) to give us one or more false positives. That's why FWE involves
#' ratcheting down alpha so much (using bonferroni, for example). 
#'
#' To go into this logic a bit further: if you repeat a study consisting of a single test 
#' where the null is actually true,  in the long
#' run there is a 95% chance (if alpha=.05) that any given study will ***not*** yield a 
#' test statistic that
#' has a *p* value less than .05. If you have two tests per study, 
#' there is only a 95% x 95% = `r .95^2`
#' chance that *neither* of the two tests will yield a test statistic with a *p*-value
#' < .05 (and so your alpha across the family of those two tests is now effectively 
#' 1 - .95<sup>2</sup> = `r 1-.95^2`,  rather
#' than .05. Intuitively, now you've got two tests to worry about producing
#' an extreme statistic just by chance, and it doesn't matter which one does. This
#' problem blows up pretty quickly. For 100 tests, each with alpha set at .05, your
#' family-wise alpha becomes 1 - .95<sup>100</sup> = `r round(1-.95^100,3)`. 
#' Yep, that means out of 1000 
#' studies, each with 100 tests, about 994 of them will have at least 1 false positive. You'll
#' see this actually happen in our simulation.
#' Bonferroni correction reduces this problem with brute force by just decreasing the
#' per-test (or per-voxel) alpha by a factor of the number of tests. So for 100 test, p<.05
#' become p<.0005, and now our FWE alpha is 1 - .9995<sup>100</sup> = `r round(1-.9995^100, 3)`. 
#' It's slightly conservative, but super easy to apply.
#'
#' The simulation will show how FWE and the false positive rate both behave across
#' a range of test-wise alpha levels.
#' 
#' # True positives
#'
#' Again, a preview of the simulation code:
#' 
#'```r
#'replicate(iter, data.frame(fp=sum(replicate(round(pH0*tests),
#'                                            t.test(rnorm(N, 0, 1))$p.value<alpha)),
#'                           tp=sum(replicate(round(pH1*tests),
#'                                            t.test(rnorm(N, d, 1))$p.value<alpha)))
#'```
#' In the `replicate` call after `tp=`, we do the same sort of thing as I described
#' above -- the steps are exactly the same --  except now we draw our 
#' random numbers from a normal distribution centered
#' at our effect size (say, .2 for a very common psychological effect size):
#' `rnorm(N, d, 1)`, where `d` is the effect size (the `1` following it is the 
#' standard deviation). In this case, the sum of this set of tests with *p* < alpha
#' is the number of true positives out of the proportion of tests (`pH1`) where
#' we know H1=T (because we've set it up to be true). The
#' number of true positives out of possible true positives is a function of power,
#' and indeed, the long-run average proportion of the number of true positives we can detect
#' out of all of them is going to be equivalent to power (so if, on average, we detect
#' 2 out of 10 true positives, our power is .2).
#'
#' The number of true positives we can detect will go into the total number 
#' of detected activations (*fp + tp*), which will be the denominator of our false positive rate.
#' So you can already see that power, along with the number of tests where H1=T
#' out of the total number of tests, will play a big role in determining
#' the false positive rate.
#'
#' # The Simulation parameters
#'
#' So, looking again at the simulation code, above or below, you'll notice that
#' we get the number of true positives and false positives for each study a bunch
#' of times -- that's what the first call to `replicate(iter, ...` is doing. I've
#' set `iter=1000` below, and the other parameters should match up with the 
#' text above, more or less. 
#'
#' You can play with these, though, to see how they effect the numbers we get out below.
#'

#percent voxels/clusters etc where H0=T
pH0 <- .9
#percent voxels/clusters etc where H1=T
pH1 <- 1-pH0
#sample size for 1 study
N <- 50
#standardized H1 effect (e.g., d=.2)
d <- .2
#alpha level
alpha <- .05
#number of tests in 1 study (e.g., voxels)
tests <- 100
#number of study replications
iter <- 1000

set.seed(1337)

#'
#' Generate the data... (this takes about 10 seconds on my computer)
#'
simfunction <- function(iter, pH0, pH1, tests, N, alpha, d) {
	manyreps<-replicate(iter, 
			    data.frame(fp=sum(replicate(round(pH0*tests),
							t.test(rnorm(N, 0, 1))$p.value<alpha)),
				       tp=sum(replicate(round(pH1*tests),
							t.test(rnorm(N, d, 1))$p.value<alpha))),
			    simplify=T)
	manyrepsDF <- data.frame(fp=unlist(manyreps['fp', ]),
				 tp=unlist(manyreps['tp', ]))
	repStats <- within(manyrepsDF, { #For each row do:
				   FWE <- as.numeric(fp>0) #is there at least 1 false positive?
				   total.hits <- fp+tp #how many detections?
				   FP.rate <- fp/total.hits #fp rate
				   fp.prop <- fp/round(pH0*tests) #out of all our tests of H0 voxels, how many positives?
				   tp.prop <- tp/round(pH1*tests) #out of all our tests of H1 voxels, how many positives?
				 })	       
	return(repStats)
}

firstSimFN <- './first_sim.rds'
if(file.exists(firstSimFN)){
	repStats <- readRDS(firstSimFN)
} else {
	repStats <- simfunction(iter, pH0, pH1, tests, N, alpha, d)
	saveRDS(repStats, firstSimFN)
}

#'
#' # Summary of replications
#'
#This is a summary of the distribution of the percent of false positives
library(knitr)
kable(data.frame(mean=repSums <- round(colMeans(repStats, na.rm=T),4)),
      format='pandoc')

#' 
#' In the summary means above you can see that on average, we get about
#' `r repSums['fp']` false positives per study, and `r repSums['tp']` true
#' positives. This gives us an average false positive rate of `r repSums['FP.rate']`.
#' Notice that the average proportion of tests within each study that are
#' false positives is what we'd expect with alpha = `r alpha` (fp.prop = `r repSums['fp.prop']`).
#' Also notice our `r ifelse(repSums['tp.prop']>.8, 'fantastic', 'less-than-ideal')`
#' power of `r repSums['tp.prop']`. Finally, check out the FWE = `r repSums['FWE']`. That's
#' about what we calculated above, assuming voxel wise alpha is actually set at .05.
#'
#' Here are some histograms over our iterations:
#'
#+fig.width=4, fig.height=4, echo=F
library(ggplot2)
qplot(repStats$fp.prop)
qplot(repStats$tp.prop)
qplot(repStats$FP.rate)

#'
#' # FP Rate at different alphas
#'
#' What might be interesting is to see how this stuff changes as we start
#' to control our FWE, and as our sample size changes (I'm going to leave our
#' effect size at .2 because this is pretty good guess for effect sizes
#' and we really don't have much control over this parameter).
#'
library(parallel)

fweSimsFN <- './fwe_sims.rds'
if(file.exists(fweSimsFN)){
	fweSims <- readRDS(fweSimsFN)
} else {
	fweSims <- mclapply(seq(.05/100, .05, length.out=16), function(x){
				    repStats <- simfunction(iter=1000,
							    pH0=.9,
							    pH1=.1,
							    tests=100,
							    N=50,
							    alpha=x,
							    d=.2)
				    repSums <- data.frame(t(colMeans(repStats, na.rm=T)))
				    repSums$alpha=x
				    return(repSums)
      }, mc.cores=8)
	saveRDS(fweSims, fweSimsFN)
}
			  
fweSimsDF <- do.call(rbind, fweSims)

with(fweSimsDF, qplot(alpha, FWE)+geom_smooth()+scale_x_reverse()+geom_hline(yintercept=.05))
#'
#' In the above plot, you can see that we improve FWE down to alpha=.05 by 
#' reducing the test- or voxel-wise alpha.
#'
with(fweSimsDF, qplot(FWE, FP.rate)+geom_smooth()+scale_x_reverse())
#'
#' This helps the rate of false positives, which drops as we approach FWE<.05.
#'
with(fweSimsDF, qplot(FWE, tp.prop)+geom_smooth()+scale_x_reverse())
#'
#' But by controlling FWE we also reduce power (in each study, or proportion of 
#' detected true positives (tp) to total true positives descends to 
#' `r round(min(fweSimsDF$tp.prop),3)`. Yep, `r 100*round(min(fweSimsDF$tp.prop),3)`%
#' power.
#'
#' To give you an idea of why the false positive rate goes down even though
#' *both* false positive detection and true positive detection goes down, look
#' at this graph:
#'
with(fweSimsDF, qplot(fp.prop, tp.prop)+geom_smooth()+scale_x_reverse())
#' 
#' You can see that tp.prop decreases less quickly than fp.prop (at least at first).
#'
#' # False positive rates in practice (not simulation)
#'
#' Though this simulation can tell us a good deal about the behavior of false positive rates
#' relative to FWE in general, there is very little we can actually know about the 
#' false positive rate in any given study. Recall the above formula for calculating
#' the false positive rate:
#'
#' $$\text{fp.rate} = \frac{\text{fp}}{\text{fp} + \text{tp}}$$
#' 
#' Imagine trying to take the number of significant tests (activated voxels) in some
#' particular study and breaking them out into true positives (**tp**) and false positives
#' (**fp**). What really matters in this case is the base rate of true positives and false 
#' positives. Imagine that 99 of the 100 tests are performed on data with a mean that is
#' really not 0 (H1=T). You'd be right to guess that a lot more of the positive detections
#' in the denominator ought to arise from those true effects. Of course, if you knew that, 
#' you wouldn't have to science it out. So how does the false positive rate change as
#' a function of the proportion of truly H0 tests and truly H1 tests?
#'
#' In the plots below, I'm going to narrow the range of alpha a bit, because nobody
#' uses a test-wise alpha=.05. We'll start at .025 and go to .0005 (bonferroni corrected
#' for 100 tests).
#' 

h0toh1grid <- unlist(apply(expand.grid(pH0=seq(.1, .9, length.out=10),
			  alpha=seq(.05/100, .025, length.out=40)),
		    1, list), recursive=F)

h0SimsFN <- './h0_sims.rds'
if(file.exists(h0SimsFN)){
	h0Sims <- readRDS(h0SimsFN)
} else {
	h0Sims <- mclapply(h0toh1grid, function(x){
				   alpha=x['alpha']
				   pH0=x['pH0']
				   repStats <- simfunction(iter=1000,
							   pH0=pH0,
							   pH1=1-pH0,
							   tests=100,
							   N=50,
							   alpha=alpha,
							   d=.2)
				   repSums <- data.frame(t(colMeans(repStats, na.rm=T)))
				   repSums$alpha=alpha
				   repSums$pH0=pH0
				   return(repSums)
      }, mc.cores=8)
	saveRDS(h0Sims, h0SimsFN)
}

h0SimsDF <- do.call(rbind, h0Sims)

h0SimsDF$pH1 <- as.factor(round(1-h0SimsDF$pH0, 2))

h1graph <- function(yvarname){
	ggplot(h0SimsDF, aes_string(x='alpha', y=yvarname, group='pH1'))+
		geom_vline(xintercept=.0005, color='red', alpha=.5)+
		geom_point(aes(color=pH1))+
		geom_line(aes(color=pH1), stat='smooth', method='loess')+
		scale_x_reverse()
}

h1graph('FP.rate')+geom_vline(xintercept=1-exp(log(.3)/100), alpha=.3)

#'
#' In the above graph, you can see that controlling FWE via test-wise alpha 
#' makes a difference to the false positive rate, but also as a function
#' of the proportion of H1=T and H0=T tests (pH1 is the proportion of H1=T tests). 
#' And of course in practice, you never know that proportion. To interpret
#' the y axis in terms of FWE, the gray line is at Eklund et al.'s estimate
#' of a true FWE of alpha=.7 for some cluster FWE corrections (70% error level). Depending on the proportion
#' of true activations to false ones, we get false-positive rates between barely
#' anything and above 40%. 
#'
#' Even at FWE alpha=.05 (the red line), the potential false positive rate
#' is pretty broad. This is largely because with *d*=.2 and N=50, we have very poor
#' power. 
#'

h1graph('tp.prop')  

#'
#' Power is not a function of the proportion of H1=T and H0=T tests.
#'

h1graph('total.hits')

#'
#' Total hits is a function of H1=T : H0=T.
#'

h1graph('FWE')+geom_hline(yintercept=.05)+geom_vline(xintercept=1-exp(log(.3)/100), alpha=.3)

#'
#' The above graph shows that when you control FWE, you're only setting an upper limit.
#' Notice that depending on the proportion of true versus false activations, the actual
#' FWE for any given test-wise alpha can vary considerably.
#' This limit applies to the case where the proportion of H1=T tests is 0. If you're
#' in a situation with some tests where H1=T, your actual FWE is going to be less than
#' this threshold. You can also see that at our bonferroni corrected test-wise alpha
#' of .0005 (red vertical line), our FWE is < .05 (black horizontal line).
#'

#'
#' # Sample Size
#'
#' Because power is such a huge consideration (and the major problem in the 
#' above graphs), let's do the same thing now varying sample size. We'll keep alpha constant now
#' at a bonferroni corrected .05/100 = .0005. We'll also keep *d* at .2 (again, a pretty 
#' reasonable effect size estimate). We can now
#' look at how the false positive rate and power change as a function of sample size
#' and the proportion of H1=T versus H0=T tests.
#'

NandH0toH1grid <- unlist(apply(expand.grid(pH0=seq(.1, .9, length.out=10),
					   N=seq(20, 200, length.out=40)),
			       1, list), recursive=F)

alpha.c <- .05/100

NandH0SimsFN <- './NandH0_sims.rds'
if(file.exists(NandH0SimsFN)){
	NandH0Sims <- readRDS(NandH0SimsFN)
} else {
	NandH0Sims <- mclapply(NandH0toH1grid, function(x){
				       N=x['N']
				       pH0=x['pH0']
				       repStats <- simfunction(iter=1000,
							       pH0=pH0,
							       pH1=1-pH0,
							       tests=tests,
							       N=N,
							       alpha=alpha.c,
							       d=.2)
				       repSums <- data.frame(t(colMeans(repStats, na.rm=T)))
				       repSums$N=N
				       repSums$pH0=pH0
				       return(repSums)
      }, mc.cores=8)
	saveRDS(NandH0Sims, NandH0SimsFN)
}

NandH0SimsDF <- do.call(rbind, NandH0Sims)

NandH0SimsDF$pH1 <- as.factor(round(1-NandH0SimsDF$pH0, 2))

NandH1graph <- function(yvarname){
	ggplot(NandH0SimsDF, aes_string(x='N', y=yvarname, group='pH1'))+
		geom_point(aes(color=pH1))+
		geom_line(aes(color=pH1), stat='smooth', method='loess')
}

#+"N graphs"
NandH1graph('FP.rate') 
#'
#' What's a decent FP rate to aim for, anyway? There seems to be a definite elbow here
#' at about N=100, so maybe that's a good aim. FP rate < 10% at d=.2 and 10% H1=T? 
#' But for d=.2, power is still terrible at 100 (see below...a little over 5%). 
#'
NandH1graph('tp.prop') 
NandH1graph('total.hits')
#'
#' Notice below that the number of false positives out of all the H0=T tests, and FWE
#' don't care at all about the sample size. And only FWE cares about the proportion
#' of H0=T and H1=T tetsts, though you're always below an FWE alpha=.05 if you
#' correct correctly.
#'

NandH1graph('fp.prop')
NandH1graph('FWE')+geom_hline(yintercept=.05)
