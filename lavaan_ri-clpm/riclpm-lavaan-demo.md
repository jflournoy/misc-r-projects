-   [RI-CLPM](#ri-clpm)
-   [Example data](#example-data)
-   [Fitting a RI-CLPM](#fitting-a-ri-clpm)
-   [References](#references)

RI-CLPM
=======

This is code for implementing the Random Intercept Cross Lagged Panel
Model proposed by Hamaker et al (Hamaker, Kuiper, and Grasman 2015).

The model looks like this:

![RI-CLPM Diagram](hamaker-diagram.png)

The first thing to not is the trick they use to separate out between and
within person variance. It's really cool actually. Instead of using the
items directly, they model a mean structure for the items, and then
essentially turn the residuals of that mean structure into
single-indicator factors (with loadings fixed at 1). After you've
"extracted" all the mean variance, you perform the cross-lagged analysis
on the residuals via these factors. Read the paper for a
less...casual...explantion.

Example data
============

This is a crack at the code. Please let me know if you see something
wrong.

First, we'll load the data

    #install.packages('lavaan')
    require(lavaan)
    require(tidyverse)
    data("Demo.growth")

    knitr::kable(summary(Demo.growth))

<table>
<thead>
<tr class="header">
<th></th>
<th align="center">t1</th>
<th align="center">t2</th>
<th align="center">t3</th>
<th align="center">t4</th>
<th align="center">x1</th>
<th align="center">x2</th>
<th align="center">c1</th>
<th align="center">c2</th>
<th align="center">c3</th>
<th align="center">c4</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td></td>
<td align="center">Min. :-4.3534</td>
<td align="center">Min. :-4.8583</td>
<td align="center">Min. :-6.0166</td>
<td align="center">Min. :-7.338</td>
<td align="center">Min. :-2.82378</td>
<td align="center">Min. :-2.8262</td>
<td align="center">Min. :-2.577343</td>
<td align="center">Min. :-2.536319</td>
<td align="center">Min. :-3.39650</td>
<td align="center">Min. :-2.45499</td>
</tr>
<tr class="even">
<td></td>
<td align="center">1st Qu.:-0.3604</td>
<td align="center">1st Qu.: 0.1589</td>
<td align="center">1st Qu.: 0.8794</td>
<td align="center">1st Qu.: 1.595</td>
<td align="center">1st Qu.:-0.87877</td>
<td align="center">1st Qu.:-0.5300</td>
<td align="center">1st Qu.:-0.689172</td>
<td align="center">1st Qu.:-0.574958</td>
<td align="center">1st Qu.:-0.56944</td>
<td align="center">1st Qu.:-0.61644</td>
</tr>
<tr class="odd">
<td></td>
<td align="center">Median : 0.6722</td>
<td align="center">Median : 1.8804</td>
<td align="center">Median : 2.7290</td>
<td align="center">Median : 3.721</td>
<td align="center">Median :-0.07608</td>
<td align="center">Median : 0.1300</td>
<td align="center">Median :-0.032083</td>
<td align="center">Median : 0.009501</td>
<td align="center">Median : 0.07427</td>
<td align="center">Median :-0.01404</td>
</tr>
<tr class="even">
<td></td>
<td align="center">Mean : 0.5947</td>
<td align="center">Mean : 1.6733</td>
<td align="center">Mean : 2.5932</td>
<td align="center">Mean : 3.639</td>
<td align="center">Mean :-0.09210</td>
<td align="center">Mean : 0.1383</td>
<td align="center">Mean : 0.007788</td>
<td align="center">Mean : 0.028926</td>
<td align="center">Mean : 0.06764</td>
<td align="center">Mean :-0.01800</td>
</tr>
<tr class="odd">
<td></td>
<td align="center">3rd Qu.: 1.6701</td>
<td align="center">3rd Qu.: 3.0275</td>
<td align="center">3rd Qu.: 4.4350</td>
<td align="center">3rd Qu.: 5.825</td>
<td align="center">3rd Qu.: 0.63925</td>
<td align="center">3rd Qu.: 0.8165</td>
<td align="center">3rd Qu.: 0.625950</td>
<td align="center">3rd Qu.: 0.603897</td>
<td align="center">3rd Qu.: 0.65894</td>
<td align="center">3rd Qu.: 0.63575</td>
</tr>
<tr class="even">
<td></td>
<td align="center">Max. : 5.2113</td>
<td align="center">Max. : 9.9472</td>
<td align="center">Max. :11.5305</td>
<td align="center">Max. :14.720</td>
<td align="center">Max. : 2.71832</td>
<td align="center">Max. : 2.8845</td>
<td align="center">Max. : 2.566558</td>
<td align="center">Max. : 2.710536</td>
<td align="center">Max. : 2.60935</td>
<td align="center">Max. : 2.65064</td>
</tr>
</tbody>
</table>

    Demo.growth %>%
      mutate(pid = 1:n()) %>%
      gather(key, value, -pid, -x1, -x2) %>%
      extract(col = key, into = c('var', 'wave'), regex = '(\\w)(\\d)') %>%
      ggplot(aes(x = wave, y = value, color = var, group = var)) +
      geom_point(position = position_jitter(w = .2), alpha = .1) +
      geom_line(stat = 'identity', aes(group = interaction(var, pid)), alpha = .04) + 
      geom_line(stat = 'smooth', method = 'lm', size = 1) + 
      theme_classic()

![](riclpm-lavaan-demo_files/figure-markdown_strict/lavaan%20demo%20growth%20data-1.png)

Well, look at that. The Demo.growth data has two time varrying
variables, `t`, and `c`. Just right for our purposes.

In the below `lavaan` code, I'll be using the notaiton from the diagram,
except instead of "x" and "y", I'll use "t" and "c". I am explicitly
specifying everything in the diagram, which is why in the call to
`lavaan` I set a bunch of `auto` options to false. This is because often
lavaan will try to automatically estimate things that you don't usually
write out but often want estimated, like residuals. Because this model
is unorthodox, I want to be as explicit as possible.

Fitting a RI-CLPM
=================

The lavaan code below uses syntax that can be found in their help docs
for the [basic stuff](http://lavaan.ugent.be/tutorial/syntax1.html) as
well as the more
[advanced](http://lavaan.ugent.be/tutorial/syntax2.html) labelling and
constraining.

    riclpmModel <- 
    '
    #Note, the data contain t1-3 and c1-3
    #Latent mean Structure with intercepts

    kappa =~ 1*t1 + 1*t2 + 1*t3
    omega =~ 1*c1 + 1*c2 + 1*c3

    t1 ~ mu1*1 #intercepts
    t2 ~ mu2*1
    t3 ~ mu3*1
    c1 ~ pi1*1
    c2 ~ pi2*1
    c3 ~ pi3*1

    kappa ~~ kappa #variance
    omega ~~ omega #variance
    kappa ~~ omega #covariance

    #laten vars for AR and cross-lagged effects
    p1 =~ 1*t1 #each factor loading set to 1
    p2 =~ 1*t2
    p3 =~ 1*t3
    q1 =~ 1*c1
    q2 =~ 1*c2
    q3 =~ 1*c3

    #constrain autoregression and cross lagged effects to be the same across both lags.
    p3 ~ alpha3*p2 + beta3*q2
    p2 ~ alpha2*p1 + beta2*q1

    q3 ~ delta3*q2 + gamma3*p2
    q2 ~ delta2*q1 + gamma2*p1

    p1 ~~ p1 #variance
    p2 ~~ u2*p2
    p3 ~~ u3*p3
    q1 ~~ q1 #variance
    q2 ~~ v2*q2
    q3 ~~ v3*q3

    p1 ~~ q1 #p1 and q1 covariance
    p2 ~~ q2 #p2 and q2 covariance
    p3 ~~ q3 #p2 and q2 covariance'

    fit <- lavaan(riclpmModel, data = Demo.growth,
                  int.ov.free = F,
                  int.lv.free = F,
                  auto.fix.first = F,
                  auto.fix.single = F,
                  auto.cov.lv.x = F,
                  auto.cov.y = F,
                  auto.var = F)

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated lv
    ## variances are negative

    summary(fit)

    ## lavaan (0.5-23.1097) converged normally after 103 iterations
    ## 
    ##   Number of observations                           400
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.883
    ##   Degrees of freedom                                 1
    ##   P-value (Chi-square)                           0.347
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   kappa =~                                            
    ##     t1                1.000                           
    ##     t2                1.000                           
    ##     t3                1.000                           
    ##   omega =~                                            
    ##     c1                1.000                           
    ##     c2                1.000                           
    ##     c3                1.000                           
    ##   p1 =~                                               
    ##     t1                1.000                           
    ##   p2 =~                                               
    ##     t2                1.000                           
    ##   p3 =~                                               
    ##     t3                1.000                           
    ##   q1 =~                                               
    ##     c1                1.000                           
    ##   q2 =~                                               
    ##     c2                1.000                           
    ##   q3 =~                                               
    ##     c3                1.000                           
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   p3 ~                                                
    ##     p2      (alp3)    1.044    0.047   22.250    0.000
    ##     q2      (bet3)   -0.306    0.073   -4.173    0.000
    ##   p2 ~                                                
    ##     p1      (alp2)    1.009    0.014   73.601    0.000
    ##     q1      (bet2)   -0.108    0.067   -1.610    0.107
    ##   q3 ~                                                
    ##     q2      (dlt3)    0.026    0.076    0.347    0.728
    ##     p2      (gmm3)    0.024    0.033    0.715    0.475
    ##   q2 ~                                                
    ##     q1      (dlt2)    0.110    0.067    1.633    0.102
    ##     p1      (gmm2)    0.010    0.032    0.321    0.749
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   kappa ~~                                            
    ##     omega            -0.122    0.515   -0.237    0.813
    ##   p1 ~~                                               
    ##     q1                0.248    0.524    0.474    0.636
    ##  .p2 ~~                                               
    ##    .q2                0.248    0.065    3.787    0.000
    ##  .p3 ~~                                               
    ##    .q3                0.400    0.070    5.754    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .t1       (mu1)    0.595    0.079    7.527    0.000
    ##    .t2       (mu2)    1.673    0.106   15.763    0.000
    ##    .t3       (mu3)    2.593    0.136   19.055    0.000
    ##    .c1       (pi1)    0.008    0.049    0.158    0.875
    ##    .c2       (pi2)    0.029    0.047    0.610    0.542
    ##    .c3       (pi3)    0.068    0.047    1.449    0.147
    ##     kappa             0.000                           
    ##     omega             0.000                           
    ##     p1                0.000                           
    ##    .p2                0.000                           
    ##    .p3                0.000                           
    ##     q1                0.000                           
    ##    .q2                0.000                           
    ##    .q3                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     kappa            -9.863   14.987   -0.658    0.510
    ##     omega            -0.033    0.054   -0.607    0.544
    ##     p1               12.359   14.995    0.824    0.410
    ##    .p2        (u2)    1.829    0.129   14.148    0.000
    ##    .p3        (u3)    1.758    0.127   13.893    0.000
    ##     q1                1.005    0.089   11.270    0.000
    ##    .q2        (v2)    0.918    0.076   12.144    0.000
    ##    .q3        (v3)    0.895    0.081   10.986    0.000
    ##    .t1                0.000                           
    ##    .t2                0.000                           
    ##    .t3                0.000                           
    ##    .c1                0.000                           
    ##    .c2                0.000                           
    ##    .c3                0.000

I'm not sure why some variances are negative, except that this is
probably an artificial data set. If you try this on your own real data
and get the same problem, please let me know.

References
==========

Hamaker, Ellen L., Rebecca M. Kuiper, and Raoul P. P. P. Grasman. 2015.
“A Critique of the Cross-Lagged Panel Model.” *Psychological Methods* 20
(1): 102–16. doi:[10.1037/a0038889](https://doi.org/10.1037/a0038889).
