#'
#' # Is it tough to estimate significant slope variance?
#' 

#+ echo=F, results='hide'
knitr::opts_chunk$set(
    warning=F,message=F,error=F,collapse = T
)

#+ echo=T,results='hide'
library(lavaan)
library(simsem)
library(ggplot2)
library(dplyr)
library(tidyr)

#+ mainstuff
create_cohseq_design <- function(type = c('cohseq', 'missing')){
    start_year_inds <- sample(1:(40-2), size = N, replace = T)
    if(type == 'cohseq'){
        function(adf){
            cohort_seq_df <- data.frame(w1=NA,w2=NA,w3=NA,age=NA)
            for(i in 1:N){
                k <- start_year_inds[i]
                cohort_seq_df[i,c('w1','w2','w3')] <- adf[i, k:(k+2)]
                cohort_seq_df[i,'age'] <- k
            }
            cohort_seq_df$age_c <- cohort_seq_df$age - 20
            return(cohort_seq_df)
        }
    }  else if(type == 'missing'){
        function(adf){
            missing_df <- adf
            missing_df[,] <- NA
            for(i in 1:N){
                k <- start_year_inds[i]
                missing_df[i, k:(k+2)] <-
                    adf[i, k:(k+2)]
            }
            return(missing_df)
        }
    }
}

long_model_pop <- paste0(
    'i =~ ', paste(paste0('1*x', 1:40), collapse = ' + '), '
    s =~ ', paste(paste0((1:40)-20, '*x', 1:40), collapse = ' + '),'
    i ~~ 250*i
    s ~~ .2*s
    i ~~ -.1*s
    i ~ 50*1
    s ~ .25*1
    ', paste(paste0('x', 1:40, ' ~~ 10*x', 1:40), collapse = '\n')
    )

long_model_est <- paste0(
    'i =~ ', paste(paste0('1*x', 1:40), collapse = ' + '), '
    s =~ ', paste(paste0((1:40)-20, '*x', 1:40), collapse = ' + '),'
    i ~~ i
    s ~~ s
    i ~~ s
    i ~ 1
    s ~ 1
    ', paste(paste0('x', 1:40, ' ~~ r*x', 1:40), collapse = '\n'))

age_reg_mod <- paste0(
    'i =~ ', paste(paste0('1*w', 1:3), collapse = ' + '), '
    s =~ ', paste(paste0((1:3)-2, '*w', 1:3), collapse = ' + '),'
    i ~ age_c
    s ~ age_c
    i ~~ i
    s ~~ s
    i ~~ s
    i ~ 1
    s ~ 1
    age_c ~ 1')

age_reg_mod_r <- paste0(
  'i =~ ', paste(paste0('1*w', 1:3), collapse = ' + '), '
    s =~ ', paste(paste0((1:3)-2, '*w', 1:3), collapse = ' + '),'
    i ~ age_c
    s ~ age_c
    i ~~ i
    s ~~ s
    i ~~ s
    i ~ 1
    s ~ 1
    age_c ~ 1
    w1 ~~ r*w1
    w2 ~~ r*w2
    w3 ~~ r*w3')

N <- 500

adf <- lavaan::simulateData(model = long_model_pop, model.type = 'growth', sample.nobs = N)

adf_l <- adf %>%
    mutate(id = 1:n()) %>%
    gather(wave, x, -id) %>%
    mutate(age = as.numeric(sub('x(\\d+)', '\\1', wave)))

ggplot(adf_l, aes(x = age, y = x)) +
    geom_point(alpha = .2) +
    geom_line(stat = 'smooth', method = 'gam', aes(group = id), alpha = .2)

missing_df <- create_cohseq_design(type = 'missing')(adf)
cohort_seq_df <- create_cohseq_design(type = 'cohseq')(adf)

missing_df_l <- missing_df %>%
    mutate(id = 1:n()) %>%
    gather(wave, x, -id) %>%
    mutate(age = as.numeric(sub('x(\\d+)', '\\1', wave))) %>%
    filter(!is.na(x))

ggplot(missing_df_l, aes(x = age, y = x)) +
    geom_point(alpha = .2) +
    geom_line(stat = 'smooth', method = 'lm', aes(group = id), alpha = .2) +
    geom_smooth()

temprez_pop <- lavaan::growth(model = long_model_est, data = adf)
summary(temprez_pop)
temprez <- lavaan::growth(model = long_model_est, data = missing_df, missing = 'fiml',
                          optim.method = 'BFGS', estimator = 'MLR')
summary(temprez, standardized = T)

temprezage <- lavaan::growth(model = age_reg_mod, data = cohort_seq_df)
summary(temprezage, standardized = T)

options('simsem.multicore' = TRUE, 'numProc' = 7)
fulldata_sim <- simsem::sim(
    nRep = 100, model = long_model_est, n = N,
    generate = long_model_pop, lavaanfun = 'growth'
)
cohseq_sim <- simsem::sim(
  nRep = 100, model = age_reg_mod, n = N, lavaanfun = 'growth',
  generate = long_model_pop, datafun = create_cohseq_design(type = 'cohseq')
)
cohseq_sim_r <- simsem::sim(
  nRep = 100, model = age_reg_mod_r, n = N, lavaanfun = 'growth',
  generate = long_model_pop, datafun = create_cohseq_design(type = 'cohseq')
)
missing_sim <- simsem::sim(
  nRep = 100, model = long_model_est, n = N, lavaanfun = 'growth',
  generate = long_model_pop, datafun = create_cohseq_design(type = 'missing'),
  missing = 'fiml'#, estimator = 'MLR', optim.method = 'BFGS'
)

summary(fulldata_sim)
summary(cohseq_sim)
summary(cohseq_sim_r)
try(summary(missing_sim))
