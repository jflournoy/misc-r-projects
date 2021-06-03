library(tidyverse)
library(data.table)
adf <- read_csv('/data/jflournoy/hariri/S4 social_behaviors_wide.csv')
call_cols <- cols(X1 = col_integer(), 
                  userid = col_character(),
                  date = col_date(format = "%Y%m%d"),
                  sums_24hours = col_double())
calls_in_dur <- read_csv('/data/jflournoy/hariri/S4 calls_in_duration_long.csv',
                col_types = call_cols) %>%
  rename(cindur = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
calls_in_freq <- read_csv('/data/jflournoy/hariri/S4 calls_in_frequency_long.csv',
                       col_types = call_cols) %>%
  rename(cinfreq = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
calls_out_dur <- read_csv('/data/jflournoy/hariri/S4 calls_out_duration_long.csv',
                                         col_types = call_cols) %>%
  rename(coutdur = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
calls_out_freq <- read_csv('/data/jflournoy/hariri/S4 calls_out_frequency_long.csv',
                                         col_types = call_cols) %>%
  rename(coutfreq = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
convo_dur <- read_csv('/data/jflournoy/hariri/S4 convo_duration_long.csv',
                                         col_types = call_cols) %>%
  rename(cvdur = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
convo_freq <- read_csv('/data/jflournoy/hariri/S4 convo_frequency_long.csv',
                           col_types = call_cols) %>%
  rename(cvfreq = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
sms_in_freq <- read_csv('/data/jflournoy/hariri/S4 sms_in_frequency_long.csv',
                         col_types = call_cols) %>%
  rename(sinfreq = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
sms_in_len <- read_csv('/data/jflournoy/hariri/S4 sms_in_length_long.csv',
                          col_types = call_cols) %>%
  rename(sinlen = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
sms_out_freq <- read_csv('/data/jflournoy/hariri/S4 sms_out_frequency_long.csv',
                          col_types = call_cols) %>%
  rename(soutfreq = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()
sms_out_len <- read_csv('/data/jflournoy/hariri/S4 sms_out_length_long.csv',
                           col_types = call_cols) %>%
  rename(soutlen = sums_24hours) %>%
  select(-X1) %>%
  as.data.table()

lapply(list(calls_in_dur,
            calls_in_freq,
            calls_out_dur,
            calls_out_freq,
            convo_dur,
            convo_freq,
            sms_in_freq,
            sms_in_len,
            sms_out_freq,
            sms_out_len),
       function(x) length(unique(x$userid)))

long_data <- list(calls_in_dur,
     calls_in_freq,
     calls_out_dur,
     calls_out_freq,
     convo_dur,
     convo_freq,
     sms_in_freq,
     sms_in_len,
     sms_out_freq,
     sms_out_len) %>%
  reduce(function(...) merge(..., by = c('userid', 'date'), all = TRUE)) %>%
  rename(id_ = userid) %>%
  mutate(id = as.numeric(factor(id_))) %>%
  select(-id_) %>%
  mutate_at(.vars = vars(-id, -date), .funs = scale)

dim(distinct(long_data, id))

cat(paste(names(long_data), collapse = '\n'))

long_data %>%
  select(-date) %>%
  write_tsv(path = '/data/jflournoy/hariri/harari.tsv', col_names = FALSE, na = '-9999')
         