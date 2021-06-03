aDF <- data.frame(expand.grid(effect_size = seq(.2,1.6,.2),
                              sample_size = seq(10,70,10)))

aDF$power <- apply(aDF, 1, function(x){
  aPrioriPower <- pwr::pwr.t.test(n = x['sample_size'],
                                  d = x['effect_size'], 
                                  sig.level = 0.05, 
                                  type = 'paired', 
                                  alternative = 'two.sided')$power
  return(round(aPrioriPower, 2))
})

powerTable <- tidyr::spread(aDF, 'sample_size', 'power')

knitr::kable(powerTable,
             col.names = c('Effect size (d)', 
                           paste0('N = ', names(powerTable)[2]),
                           names(powerTable)[-(1:2)]))



aDF$htest <- apply(aDF, 1, function(x){
  aPrioriPower <- pwr::pwr.t.test(n = x['sample_size'],
                                  d = x['effect_size'], 
                                  sig.level = 0.05, 
                                  type = 'paired', 
                                  alternative = 'two.sided')
  return(aPrioriPower)
})
