

rm(list=ls())
library(tidyverse)
library(metafor)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Wasting_inc_data.RData")

#Subset to monthly
d <- d %>% filter(measurefreq=="monthly")
d_noBW <- d_noBW %>% filter(measurefreq=="monthly")

#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)

#mean whz
whz.data <- summary.whz(d)

#Cumulative inc
d <- calc.ci.agecat(d)
ci.data <- summary.ci(d)

#Cumulative inc, no birth
d_noBW <- calc.ci.agecat(d_noBW)
ci.data.nobirth <- summary.ci(d_noBW)

#Recovery cumulative inc
#NOTE: need to make sure to only include those wasted in the denominator
#rec.data <- summary.ci(d, recovery = T)

#Incidence rate
ir.data <- summary.ir(d)

#Incidence rate - no birth wasting
ir.data.nobirth <- summary.ir(d_noBW)

#Recovery incidence rate
rec.ir.data <- summary.ir(d, recovery = T)

#Recovery incidence rate - no birth wasting
#rec.ir.data.d_noBW <- summary.ir(d_noBW, recovery = T)

#Duration
#dur.data <- summary.dur(d) 

#Recovery within 30, 60, 90 days
d <- calc.ci.agecat(d, range=6)
rec.data30 <- summary.rec60(d, length=30,  agelist=c("0-6 months","6-12 months","12-18 months","18-24 months"))
rec.data60 <- summary.rec60(d, length=60,  agelist=c("0-6 months","6-12 months","12-18 months","18-24 months"))
rec.data90 <- summary.rec60(d, length=90,  agelist=c("0-6 months","6-12 months","12-18 months","18-24 months"))


#Duration
perswast.data <- summary.perswast(d,  agelist=c("0-6 months","6-12 months","12-18 months","18-24 months")) 

#Need to add in the median durations
save(prev.data, whz.data, ci.data, ci.data.nobirth, 
     ir.data, ir.data.nobirth, rec.ir.data, 
     rec.data30, rec.data60, rec.data90, perswast.data, file="U:/Data/Wasting/Wasting_descriptive_epi_results.Rdata")





