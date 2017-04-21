library(dplyr)
library(zoo)
aDF <- data.frame(blurp=rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0), 2)) %>%
   mutate(index=1:n()) 
aDF

#use 'last observation carried forward', na.locf, from zoo.

aDF %>% mutate(censored = ifelse(blurp, 1, NA)) %>% #cuz this function operates on NAs
    na.locf(maxgap=3) #change this to 4, 5, or whatever

#better not to operate on the whole data frame -- we can use this inside of mutate, on
#just the column we want, ensuring we put na.rm=F so it doesn't throw out rows it can't
#fill
aDF %>% mutate(censored_raw = ifelse(blurp, 1, NA),
               censored = na.locf(censored_raw, maxgap=3, na.rm=F)) 


#showing how lag and lead work real quick:
lag(1:5, c(1,2))
lead(1:5)

#We're looking for rows with a blurp on either side of them such that the total distance is 5 
# or less.
#So a row with a blurp 1 row in front (lead 1), and 4 rows behind (lag 4) is one we'd want
# to censor. I think we actually have to explicitly state each combo:
aDF.ManyLeads <- aDF %>% arrange(index) %>% #arrange to make sure lag is operating correctly
    mutate(blrup.l1 = lead(blurp, 1, default=0) & lag(blurp, 4, default=0),
           blrup.l2 = lead(blurp, 2, default=0) & lag(blurp, 3, default=0),
           blrup.l3 = lead(blurp, 3, default=0) & lag(blurp, 2, default=0), 
           blrup.l4 = lead(blurp, 4, default=0) & lag(blurp, 1, default=0)) 
aDF.ManyLeads                                      

#We can do this in one call tho
aDF.ManyLeads <- aDF %>% arrange(index) %>% #arrange to make sure lag is operating correctly
    mutate(TwoBlurpWithin5 = (lead(blurp, 1, default=0) & lag(blurp, 4, default=0)) |
                      (lead(blurp, 2, default=0) & lag(blurp, 3, default=0)) |
                      (lead(blurp, 3, default=0) & lag(blurp, 2, default=0)) | 
                      (lead(blurp, 4, default=0) & lag(blurp, 1, default=0)), 
           censor = blurp | TwoBlurpWithin5) 
aDF.ManyLeads                                      

aDF %>% mutate(thing=index < (index[blurp==1]-lag(index[blurp==1])) & 
               index > (index[blurp==1]-lag(index[blurp==1]) - 5))

aDF %>% mutate(thing1 = any(c(lead(blurp, 1, default=0),
                              lead(blurp, 2, default=0),
                              lead(blurp, 3, default=0),
                              lead(blurp, 4, default=0))),
               thing2 = any(c(lag(blurp, 1, default=0),
                              lag(blurp, 2, default=0),
                              lag(blurp, 3, default=0),
                              lag(blurp, 4, default=0))))
library(zoo)
aDF %>% mutate(censored = ifelse(blurp, 1, NA))

aDF %>% mutate(censored = ifelse(blurp, 1, NA)) %>%
    na.locf(maxgap=3)
