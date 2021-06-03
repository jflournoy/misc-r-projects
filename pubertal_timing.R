library(data.table)
library(ggplot2)
get_intercept <- function(x, form){
    coef(lm(form, data = x))[['(Intercept)']]
}
get_slope <- function(x, form, age_term = 'age_c'){
    coef(lm(form, data = x))[[age_term]]
}
get_logistic_pub_start <- function(x, y_term, time_term, time_l, time_u, eps, y_cut = .25){
    #`age_offset` is the number to add back in to get real age. We can estimate
    #the growth function using nls, and use predictions to see where it
    #appreciably diverges from 0 (no puberty). 
    nl_mod <- nls(paste0(y_term, ' ~ L  / (1 + exp( -k*(', time_term, ' - x0) ))'),
                  start = c(L = 5, k = 1, x0 = 3), data = x)
    time_df <- data.frame(seq(time_l, time_u, eps))
    names(time_df) <- time_term
    return(time_df[min(which(predict(nl_mod, newdata = time_df) > y_cut)),])
}

set.seed(92103)
b0i <- rnorm(30, 0, 1.5)
b1i <- rnorm(30, 2, .6)
age <- seq(6, 19, .25)
age_center <- 10
age_c <- age - age_center
age_c_logistic <- age - (min(age) + diff(range(age))/2)

L <- 5
x0 <- rnorm(30, 4.5, .75) 
k <- rnorm(30, 1, .4)

d <- rbindlist(lapply(1:length(b0i), function(i){
    p <- b0i[[i]] + b1i[[i]]*age_c + rnorm(length(age_c))
    p_logistic_true <- L  / (1 + exp( -k[[i]]*(age_c - x0[[i]]) ))
    p_logistic <- p_logistic_true + rnorm(length(age_c), 0, .3)
    return(data.table(id = i, p = p, age = age, age_c = age_c, p_logistic = p_logistic, p_logistic_true,
                      k = k[[i]], x0 = x0[[i]]))
}))

d[, started := p_logistic_true > .25]
d[, age_started := min(age[started]), by = 'id']
d[, start_group := fifelse(age_started == max(age_started), 'max',
                           fifelse(age_started == min(age_started), 'min',
                                   ''))]

library(patchwork)
d[, logi_intercept := get_intercept(.SD, age ~ p_logistic), by = 'id']
d[, nl_age_started := 
      get_logistic_pub_start(.SD, y_term = 'p_logistic', time_term = 'age_c', 
                             time_l = -5, time_u = 15, eps = .1, y_cut = .25) + age_center,
  by = 'id']

ggplot(d[start_group == ''], aes(x = age, y = p_logistic_true, group = id)) + 
    geom_hline(yintercept = .25) + 
    geom_line(alpha = .15) +
    geom_point(color = '#555555', size = 1) +
    geom_line(data = d[start_group != ''], aes(color = start_group)) +
    geom_line(data = d[start_group != ''], aes(color = start_group),
              stat = 'smooth', method = 'lm', linetype = 2) +
    geom_point(data = d[start_group != ''], aes(color = start_group), size = 1) +
    scale_color_manual(breaks = c('max', 'min'), values = c('red', 'blue'),
                       name = 'Start Age', labels = c('Max', 'Min')) + 
    theme_dark() + 
    labs(y = 'Puberty', x = 'Age') + 
    coord_cartesian(y = c(0, 5)) +
ggplot(unique(d[, c('age_started', 'logi_intercept')]), aes(x = age_started, y = logi_intercept)) + 
    geom_point(alpha = .8) + 
    labs(x = 'Age where true puberty score > 0.25',
         y = 'Linear estimate of age where puberty = 0\n(Intercept of age ~ measured puberty)') + 
    ggplot2::coord_flip(y = c(7,14), x = c(7,14))+ 
    geom_label(label = sprintf('r = %0.2f', (cor(unique(d[, c('age_started', 'logi_intercept')]))[[2]])), 
               y = 11, x = 8) + 
    geom_abline(intercept = 0, slope = 1 ) + 
    theme_minimal() +
ggplot(unique(d[, c('age_started', 'nl_age_started')]), aes(x = age_started, y = nl_age_started)) + 
    geom_point(alpha = .8) + 
    labs(x = 'Age where true puberty score > 0.25',
         y = 'Non-linear model estimation of puberty score > 0.25') + 
    ggplot2::coord_flip(y = c(7,14), x = c(7,14))+ 
    geom_label(label = sprintf('r = %0.2f', (cor(unique(d[, c('age_started', 'nl_age_started')]))[[2]])), 
               y = 11, x = 8) + 
    geom_abline(intercept = 0, slope = 1 ) + 
    theme_minimal() + 
patchwork::plot_layout(design = "
AAB
AAC")

d[, intercept := get_intercept(.SD, p ~ age_c), by = 'id']
d[, slope := get_slope(.SD, p ~ age_c, 'age_c'), by = 'id']
d[, max_int := fifelse(intercept == max(intercept), 'max',
                       fifelse(intercept == min(intercept), 'min',
                               ''))]

ggplot(d[max_int == ''], aes(x = age, y = p, group = id)) + 
    geom_line(alpha = .15) +
    geom_point(color = '#555555') +
    geom_line(stat = 'smooth', method = 'lm', color = 'gray') + 
    geom_line(data = d[max_int != ''], aes(color = max_int)) +
    geom_point(data = d[max_int != ''], aes(color = max_int), size = 3) +
    geom_line(data = d[max_int != ''], aes(color = max_int), stat = 'smooth', method = 'lm', size = 1) + 
    scale_color_manual(breaks = c('max', 'min'), values = c('red', 'blue'),
                       name = 'Intercept', labels = c('Max', 'Min')) + 
    theme_dark() + 
    labs(y = 'Puberty', x = 'Age')

