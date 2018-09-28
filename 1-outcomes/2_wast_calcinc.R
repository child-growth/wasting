


rm(list=ls())
library(tidyverse)
library(zoo)


load("U:/Data/Wasting/rf_wasting_data.RData")

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")

#temp test
#unique(d$studyid)
# df <- d %>% filter(studyid=="ki1000108-IRC" | studyid=="ki1000108-CMC-V-BCS-2002")
# test <- df %>% group_by(studyid, country) %>% do(WastIncCalc(., dropBornWasted=T))
# table(test$wast_inc)
# table(test$studyid, test$wast_inc)
# table(test$studyid, test$wast)
# table(test$studyid, test$wast_rec60d)
# table(test$studyid, test$wasting_episode)
dfull<-d
df <- d
d <- df %>% group_by(studyid, country) %>% do(WastIncCalc(.))
d_noBW <- df %>% group_by(studyid, country) %>% do(WastIncCalc(., dropBornWasted=T))

save(d, d_noBW, file="U:/Data/Wasting/Wasting_inc_data.RData")





d <- d %>% group_by(studyid, country, subjid) %>% slice(1) 

d %>% group_by(measurefreq) %>% summarise(n()) 







