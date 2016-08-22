    library(lavaan)
    library(MASS)

Set seed so this is reproducible

    set.seed(990022)

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

    ##            [,1]      [,2]       [,3]       [,4]
    ## [1,]  1.0000000 0.2923944 -0.1403575  0.6251536
    ## [2,]  0.2923944 1.0000000  0.1350494  0.3710911
    ## [3,] -0.1403575 0.1350494  1.0000000 -0.3921865
    ## [4,]  0.6251536 0.3710911 -0.3921865  1.0000000

    cor(matG2)

    ##            [,1]       [,2]       [,3]       [,4]
    ## [1,]  1.0000000  0.3560850  0.2929822 -0.4136605
    ## [2,]  0.3560850  1.0000000 -0.1682998  0.6407975
    ## [3,]  0.2929822 -0.1682998  1.0000000 -0.0926462
    ## [4,] -0.4136605  0.6407975 -0.0926462  1.0000000

Create a data frame from the above observations with group membership
noted.

    aDF <- data.frame(rbind(matG1, matG2), group=rep(c('one', 'two'), each=100))

    cor(aDF[, 1:4])

    ##            X1          X2           X3          X4
    ## X1 1.00000000 0.315013446  0.073680788  0.07140227
    ## X2 0.31501345 1.000000000  0.003607704  0.49415841
    ## X3 0.07368079 0.003607704  1.000000000 -0.24226174
    ## X4 0.07140227 0.494158409 -0.242261743  1.00000000

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

    ## 'log Lik.' 405.2443 (df=10)

    pchisq(as.numeric(n2llDiff),  df=attr(n2llDiff, 'df'), lower.tail=F)

    ## [1] 7.202322e-81

The AIC and BIC tell a similar story -- a whopping difference in fit

    AIC(fit.homogeneity)-AIC(fit.grouped)

    ## [1] 385.2443

    BIC(fit.homogeneity)-BIC(fit.grouped)

    ## [1] 352.2611

Inspect the results, and compare to the results above from `cor` if you
wish:

    summary(fit.homogeneity)

    ## lavaan (0.5-19) converged normally after  24 iterations
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
    ##     X2                0.332    0.078    4.249    0.000
    ##     X3                0.071    0.068    1.039    0.299
    ##     X4                0.075    0.074    1.007    0.314
    ##   X2 ~~                                               
    ##     X3                0.004    0.071    0.051    0.959
    ##     X4                0.539    0.086    6.265    0.000
    ##   X3 ~~                                               
    ##     X4               -0.242    0.073   -3.330    0.001
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.011    0.101   10.000    0.000
    ##     X2                1.097    0.110   10.000    0.000
    ##     X3                0.922    0.092   10.000    0.000
    ##     X4                1.085    0.108   10.000    0.000

    summary(fit.grouped)

    ## lavaan (0.5-19) converged normally after  71 iterations
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
    ##     X2                0.339    0.121    2.806    0.005
    ##     X3               -0.142    0.102   -1.390    0.165
    ##     X4                0.614    0.116    5.301    0.000
    ##   X2 ~~                                               
    ##     X3                0.155    0.116    1.338    0.181
    ##     X4                0.414    0.119    3.479    0.001
    ##   X3 ~~                                               
    ##     X4               -0.381    0.104   -3.651    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.021    0.144    7.071    0.000
    ##     X2                1.318    0.186    7.071    0.000
    ##     X3                0.997    0.141    7.071    0.000
    ##     X4                0.945    0.134    7.071    0.000
    ## 
    ## 
    ## Group 2 [two]:
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.329    0.098    3.355    0.001
    ##     X3                0.264    0.094    2.812    0.005
    ##     X4               -0.451    0.118   -3.822    0.000
    ##   X2 ~~                                               
    ##     X3               -0.144    0.087   -1.660    0.097
    ##     X4                0.661    0.123    5.395    0.000
    ##   X3 ~~                                               
    ##     X4               -0.093    0.101   -0.923    0.356
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                0.977    0.138    7.071    0.000
    ##     X2                0.875    0.124    7.071    0.000
    ##     X3                0.832    0.118    7.071    0.000
    ##     X4                1.217    0.172    7.071    0.000

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

    ##            X1        X2         X3         X4
    ## X1  1.0000000 0.1395762 -0.1452857  0.6765313
    ## X2  0.1395762 1.0000000  0.1166640  0.3155918
    ## X3 -0.1452857 0.1166640  1.0000000 -0.3434810
    ## X4  0.6765313 0.3155918 -0.3434810  1.0000000

    fit.homogeneity.h <- lavaan(model=covModel, data=aDF.h, meanstructure=F) 
    fit.grouped.h <- lavaan(model=covModel, data=aDF.h, group='group', meanstructure=F)

Interestingly, here you do get a little bit better fit just by breaking
up the observations. This is perhaps a nice demonstration that just
adding parameters for no reason can give you better fit with the
likelihood ratio test (but the p value is still \> .05).

    n2llDiff.h <- -2*logLik(fit.homogeneity.h) - -2*logLik(fit.grouped.h) 
    print(n2llDiff.h)

    ## 'log Lik.' 22.68244 (df=10)

    pchisq(as.numeric(n2llDiff.h),  df=attr(n2llDiff.h, 'df'), lower.tail=F)

    ## [1] 0.0119814

The AIC and BIC attenuate the effect of just adding parameters, and they
don't show an improvement of fit at all: the AIC and BIC for the
homogeneity model are both smaller -- especially the BIC that really
penalizes parameters.

    AIC(fit.homogeneity.h)-AIC(fit.grouped.h)

    ## [1] 2.682445

    BIC(fit.homogeneity.h)-BIC(fit.grouped.h)

    ## [1] -30.30073

    summary(fit.homogeneity.h)

    ## lavaan (0.5-19) converged normally after  28 iterations
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
    ##     X2                0.155    0.079    1.955    0.051
    ##     X3               -0.140    0.069   -2.033    0.042
    ##     X4                0.777    0.098    7.924    0.000
    ##   X2 ~~                                               
    ##     X3                0.114    0.070    1.639    0.101
    ##     X4                0.366    0.086    4.256    0.000
    ##   X3 ~~                                               
    ##     X4               -0.346    0.075   -4.594    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.101    0.110   10.000    0.000
    ##     X2                1.126    0.113   10.000    0.000
    ##     X3                0.848    0.085   10.000    0.000
    ##     X4                1.197    0.120   10.000    0.000

    summary(fit.grouped.h)

    ## lavaan (0.5-19) converged normally after  38 iterations
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
    ##     X2                0.133    0.099    1.339    0.180
    ##     X3               -0.160    0.110   -1.459    0.145
    ##     X4                0.704    0.135    5.231    0.000
    ##   X2 ~~                                               
    ##     X3                0.112    0.101    1.109    0.267
    ##     X4                0.294    0.110    2.675    0.007
    ##   X3 ~~                                               
    ##     X4               -0.455    0.126   -3.626    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.064    0.150    7.071    0.000
    ##     X2                0.908    0.128    7.071    0.000
    ##     X3                1.107    0.156    7.071    0.000
    ##     X4                1.238    0.175    7.071    0.000
    ## 
    ## 
    ## Group 2 [two]:
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##   X1 ~~                                               
    ##     X2                0.175    0.124    1.410    0.159
    ##     X3               -0.128    0.082   -1.553    0.120
    ##     X4                0.851    0.142    5.978    0.000
    ##   X2 ~~                                               
    ##     X3                0.115    0.089    1.280    0.200
    ##     X4                0.439    0.132    3.322    0.001
    ##   X3 ~~                                               
    ##     X4               -0.236    0.086   -2.755    0.006
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  Z-value  P(>|z|)
    ##     X1                1.125    0.159    7.071    0.000
    ##     X2                1.343    0.190    7.071    0.000
    ##     X3                0.586    0.083    7.071    0.000
    ##     X4                1.156    0.164    7.071    0.000
