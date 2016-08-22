    library(lavaan)
    library(MASS)

Set seed so this is reproducible

    set.seed(534435)

create two distinct population level correlation matrices

    cor1 <- matrix(c(1.00,  .20, -.20,  .60,
              .20, 1.00,  .05,  .30,
             -.20,  .05, 1.00, -.40,
              .60,  .30, -.40, 1.00), nrow=4)
    cor2 <- matrix(c(1.00,  .50,  .15, -.30,
              .50, 1.00, -.25,  .60,
              .15, -.25, 1.00, -.05,
             -.30,  .60, -.05, 1.00), nrow=4)

Randomly draw 100 observations from each correlation matrix using
multi-variate normal distributions with mean set to 0 for all 4
variables. The mean doesn't matter for our purposes.

Quick note: it is possible to give lavaan the correlation matrix as data
rather than the observations directly:
<http://lavaan.ugent.be/tutorial/cov.html>

    matG1 <- mvrnorm(100, mu=rep(0, 4), Sigma=cor1)
    matG2 <- mvrnorm(100, mu=rep(0, 4), Sigma=cor2)

Check observed correlation matrices

    cor(matG1)

    ##            [,1]       [,2]        [,3]       [,4]
    ## [1,]  1.0000000 0.34973397 -0.17721571  0.5891794
    ## [2,]  0.3497340 1.00000000  0.08031153  0.3968863
    ## [3,] -0.1772157 0.08031153  1.00000000 -0.3279552
    ## [4,]  0.5891794 0.39688635 -0.32795524  1.0000000

    cor(matG2)

    ##            [,1]       [,2]       [,3]       [,4]
    ## [1,]  1.0000000  0.4654156  0.2837148 -0.2881132
    ## [2,]  0.4654156  1.0000000 -0.1662641  0.6527712
    ## [3,]  0.2837148 -0.1662641  1.0000000 -0.1008540
    ## [4,] -0.2881132  0.6527712 -0.1008540  1.0000000

Create a data frame from the above observations with group membership
noted.

    aDF <- data.frame(rbind(matG1, matG2), group=rep(c('one', 'two'), each=100))

    cor(aDF[, 1:4])

    ##            X1          X2          X3         X4
    ## X1 1.00000000  0.40845051  0.06540408  0.1120015
    ## X2 0.40845051  1.00000000 -0.05373308  0.5354223
    ## X3 0.06540408 -0.05373308  1.00000000 -0.2072048
    ## X4 0.11200149  0.53542226 -0.20720480  1.0000000

Create lavaan model that is just the full correlation matrix

    corVarNames <- names(aDF)[1:4]

Here we generate the lavaan model code -- we want every var name in
corVarNames to be covaried ("~~") with itself and with all other
variables

First, we get each pairing:

    allPairs <- outer(corVarNames, corVarNames, function (x,y) paste(x,"~~",y))
    print(allPairs)

    ##      [,1]       [,2]       [,3]       [,4]      
    ## [1,] "X1 ~~ X1" "X1 ~~ X2" "X1 ~~ X3" "X1 ~~ X4"
    ## [2,] "X2 ~~ X1" "X2 ~~ X2" "X2 ~~ X3" "X2 ~~ X4"
    ## [3,] "X3 ~~ X1" "X3 ~~ X2" "X3 ~~ X3" "X3 ~~ X4"
    ## [4,] "X4 ~~ X1" "X4 ~~ X2" "X4 ~~ X3" "X4 ~~ X4"

And then we just want the lower triangle with the diagonal:

    allPairs.lower <- allPairs[lower.tri(allPairs, diag=T)]

And then collapse it into lavaan code:

    covModel <- paste(allPairs.lower, collapse='\n')
    cat(c(covModel, '\n'))

    ## X1 ~~ X1
    ## X2 ~~ X1
    ## X3 ~~ X1
    ## X4 ~~ X1
    ## X2 ~~ X2
    ## X3 ~~ X2
    ## X4 ~~ X2
    ## X3 ~~ X3
    ## X4 ~~ X3
    ## X4 ~~ X4

Now we can run the model with and without the grouping variable and
compare model fit: We can set meanstructure=T or F in both models -- we
want them to be the same and it doesn't seem like we care about means
being the same or different between the groups.

    fit.homogeneity <- lavaan(model=covModel, data=aDF, meanstructure=F) 
    fit.grouped <- lavaan(model=covModel, data=aDF, group='group', meanstructure=F)

Here we see that distinguishing observations drawn from the multivariate
normal distributions with the two different correlation matrices is the
preferred model.

    n2llDiff <- -2*logLik(fit.homogeneity) - -2*logLik(fit.grouped) 
    print(n2llDiff)

    ## 'log Lik.' 365.799 (df=10)

    pchisq(as.numeric(n2llDiff),  df=attr(n2llDiff, 'df'), lower.tail=F)

    ## [1] 1.761741e-72

The AIC and BIC tell a similar story -- a whopping difference in fit

    AIC(fit.homogeneity)-AIC(fit.grouped)

    ## [1] 345.799

    BIC(fit.homogeneity)-BIC(fit.grouped)

    ## [1] 312.8158

Inspect the results, and compare to the results above from `cor` if you
wish:

    summary(fit.homogeneity)

    ## lavaan (0.5-19) converged normally after  19 iterations
    ## 
    ##   Number of observations                           200
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ##   Minimum Function Value               0.0000000000000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.426    0.080    5.347    0.000
    ##     X3                0.063    0.069    0.923    0.356
    ##     X4                0.120    0.076    1.574    0.115
    ##   X2 ~~                                               
    ##     X3               -0.050    0.066   -0.759    0.448
    ##     X4                0.553    0.083    6.675    0.000
    ##   X3 ~~                                               
    ##     X4               -0.199    0.069   -2.869    0.004
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.080    0.108   10.000    0.000
    ##     X2                1.006    0.101   10.000    0.000
    ##     X3                0.871    0.087   10.000    0.000
    ##     X4                1.062    0.106   10.000    0.000

    summary(fit.grouped)

    ## lavaan (0.5-19) converged normally after  61 iterations
    ## 
    ##   Number of observations per group         
    ##   one                                              100
    ##   two                                              100
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ##   Minimum Function Value               0.0000000000000
    ## 
    ## Chi-square for each group:
    ## 
    ##   one                                            0.000
    ##   two                                            0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## 
    ## Group 1 [one]:
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.331    0.100    3.301    0.001
    ##     X3               -0.164    0.094   -1.745    0.081
    ##     X4                0.575    0.113    5.076    0.000
    ##   X2 ~~                                               
    ##     X3                0.070    0.088    0.801    0.423
    ##     X4                0.368    0.100    3.689    0.000
    ##   X3 ~~                                               
    ##     X4               -0.297    0.095   -3.116    0.002
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                0.995    0.141    7.071    0.000
    ##     X2                0.898    0.127    7.071    0.000
    ##     X3                0.856    0.121    7.071    0.000
    ##     X4                0.957    0.135    7.071    0.000
    ## 
    ## 
    ## Group 2 [two]:
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.527    0.125    4.220    0.000
    ##     X3                0.287    0.105    2.729    0.006
    ##     X4               -0.336    0.121   -2.769    0.006
    ##   X2 ~~                                               
    ##     X3               -0.164    0.100   -1.640    0.101
    ##     X4                0.740    0.135    5.466    0.000
    ##   X3 ~~                                               
    ##     X4               -0.102    0.102   -1.003    0.316
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.163    0.164    7.071    0.000
    ##     X2                1.102    0.156    7.071    0.000
    ##     X3                0.883    0.125    7.071    0.000
    ##     X4                1.167    0.165    7.071    0.000

We should confirm that grouping doesn't give us spurious better fit, so
lets draw 200 observations from a single multivariate normal
distribution and see if arbitrary grouping gives us a better fitting
model:

    mat.homogeneous <- mvrnorm(200, mu=rep(0, 4), Sigma=cor1)
    aDF.h <- data.frame(mat.homogeneous, group=rep(c('one', 'two'), each=100))

    print(cor1)

    ##      [,1] [,2]  [,3] [,4]
    ## [1,]  1.0 0.20 -0.20  0.6
    ## [2,]  0.2 1.00  0.05  0.3
    ## [3,] -0.2 0.05  1.00 -0.4
    ## [4,]  0.6 0.30 -0.40  1.0

    cor(aDF.h[, 1:4])

    ##            X1         X2          X3         X4
    ## X1  1.0000000 0.26403583 -0.20620181  0.6015739
    ## X2  0.2640358 1.00000000  0.01159095  0.3246643
    ## X3 -0.2062018 0.01159095  1.00000000 -0.4059123
    ## X4  0.6015739 0.32466435 -0.40591233  1.0000000

    fit.homogeneity.h <- lavaan(model=covModel, data=aDF.h, meanstructure=F) 
    fit.grouped.h <- lavaan(model=covModel, data=aDF.h, group='group', meanstructure=F)

Interestingly, here you do get a little bit better fit just by breaking
up the observations. This is perhaps a nice demonstration that just
adding parameters for no reason can give you better fit with the
likelihood ratio test (but the p value is still \> .05). I *have* run
this code with a different seed and seen improved fit by adding an
arbitrary grouping, but hopefully this shouldn't happen very often. This
will be the topic of a future snippet.

    n2llDiff.h <- -2*logLik(fit.homogeneity.h) - -2*logLik(fit.grouped.h) 
    print(n2llDiff.h)

    ## 'log Lik.' 7.327219 (df=10)

    pchisq(as.numeric(n2llDiff.h),  df=attr(n2llDiff.h, 'df'), lower.tail=F)

    ## [1] 0.6942343

The AIC and BIC attenuate the effect of just adding parameters, and they
don't show an improvement of fit at all: the AIC and BIC for the
homogeneity model are both smaller -- especially the BIC that really
penalizes parameters.

    AIC(fit.homogeneity.h)-AIC(fit.grouped.h)

    ## [1] -12.67278

    BIC(fit.homogeneity.h)-BIC(fit.grouped.h)

    ## [1] -45.65595

    summary(fit.homogeneity.h)

    ## lavaan (0.5-19) converged normally after  20 iterations
    ## 
    ##   Number of observations                           200
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.260    0.072    3.610    0.000
    ##     X3               -0.187    0.066   -2.856    0.004
    ##     X4                0.570    0.078    7.290    0.000
    ##   X2 ~~                                               
    ##     X3                0.011    0.067    0.164    0.870
    ##     X4                0.322    0.074    4.367    0.000
    ##   X3 ~~                                               
    ##     X4               -0.371    0.070   -5.319    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                0.941    0.094   10.000    0.000
    ##     X2                1.030    0.103   10.000    0.000
    ##     X3                0.876    0.088   10.000    0.000
    ##     X4                0.955    0.096   10.000    0.000

    summary(fit.grouped.h)

    ## lavaan (0.5-19) converged normally after  37 iterations
    ## 
    ##   Number of observations per group         
    ##   one                                              100
    ##   two                                              100
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ##   Minimum Function Value               0.0000000000000
    ## 
    ## Chi-square for each group:
    ## 
    ##   one                                            0.000
    ##   two                                            0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## 
    ## Group 1 [one]:
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.266    0.108    2.453    0.014
    ##     X3               -0.191    0.092   -2.082    0.037
    ##     X4                0.596    0.118    5.066    0.000
    ##   X2 ~~                                               
    ##     X3               -0.086    0.095   -0.909    0.363
    ##     X4                0.370    0.113    3.278    0.001
    ##   X3 ~~                                               
    ##     X4               -0.369    0.098   -3.753    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                0.997    0.141    7.071    0.000
    ##     X2                1.105    0.156    7.071    0.000
    ##     X3                0.807    0.114    7.071    0.000
    ##     X4                1.031    0.146    7.071    0.000
    ## 
    ## 
    ## Group 2 [two]:
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.255    0.095    2.673    0.008
    ##     X3               -0.178    0.092   -1.927    0.054
    ##     X4                0.544    0.103    5.254    0.000
    ##   X2 ~~                                               
    ##     X3                0.107    0.095    1.128    0.259
    ##     X4                0.274    0.096    2.864    0.004
    ##   X3 ~~                                               
    ##     X4               -0.371    0.098   -3.792    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                0.882    0.125    7.071    0.000
    ##     X2                0.955    0.135    7.071    0.000
    ##     X3                0.933    0.132    7.071    0.000
    ##     X4                0.880    0.124    7.071    0.000
