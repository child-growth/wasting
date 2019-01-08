

#Descriptive epi- Indian studies


rm(list=ls())
library(tidyverse)
library(metafor)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Wasting_inc_data.RData")

#Subset to monthly or quarterly studies
# d <- d %>% filter(measurefreq!="yearly" & region=="Asia" & country!="PHILIPPINES")
# d_noBW <- d_noBW %>% filter(measurefreq!="yearly" & region=="Asia" & country!="PHILIPPINES")
d <- d %>% filter(measurefreq!="yearly" & region=="Asia" & country=="INDIA")
d_noBW <- d_noBW %>% filter(measurefreq!="yearly" & region=="Asia" & country=="INDIA")

#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)
#prev.region <- d %>% group_by(country) %>% do(summary.prev(.)$prev.res)
prev.cohort <- prev.data$prev.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename( est=prev,  lb=ci.lb,  ub=ci.ub)

prev<-bind_rows(
  data.frame(cohort="pooled", region="Overall", prev.data$prev.res),
  #data.frame(cohort="pooled", prev.region),
  prev.cohort)

#Severe wasting prevalence
sev.prev.data <- summary.prev(d, severe.wasted=T)
#sev.prev.region <- d %>% group_by(country) %>% do(summary.prev(.)$prev.res)
sev.prev.cohort <- sev.prev.data$prev.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename( est=prev,  lb=ci.lb,  ub=ci.ub)

sev.prev<-bind_rows(
  data.frame(cohort="pooled", region="Overall", sev.prev.data$prev.res),
  #data.frame(cohort="pooled", sev.prev.region),
  sev.prev.cohort)

#mean whz
whz.data <- summary.whz(d)
whz.region <- d %>% group_by(country) %>% do(summary.whz(.)$whz.res)
whz.cohort <- whz.data$whz.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename( est=meanwhz,  lb=ci.lb,  ub=ci.ub)

whz<-bind_rows(
  data.frame(cohort="pooled", region="Overall", whz.data$whz.res),
  data.frame(cohort="pooled", whz.region),
  whz.cohort)

#Cumulative inc
d <- calc.ci.agecat(d, range=6)
agelst=list("0-6 months","6-12 months","12-18 months","18-24 months")
ci.data <- summary.ci(d, agelist=agelst)
ci.region <- d %>% group_by(country) %>% do(summary.ci(.)$ci.res)
ci.cohort <- ci.data$ci.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  ci,  ci.lb,  ci.ub)) %>%
  rename( est=ci,  lb=ci.lb,  ub=ci.ub)

ci<-bind_rows(
  data.frame(cohort="pooled", region="Overall", ci.data$ci.res),
  data.frame(cohort="pooled", ci.region),
  ci.cohort)

#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d, range=3)
agelst3=list("0-3 months","3-6 months","6-9 months","9-12 months",
             "12-15 months","15-18 months","18-21 months","21-24 months")
ci.data3 <- summary.ci(d, agelist=agelst3)
ci.region3 <- d %>% group_by(country) %>% do(summary.ci(.)$ci.res)
ci.cohort3 <- ci.data3$ci.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  ci,  ci.lb,  ci.ub)) %>%
  rename( est=ci,  lb=ci.lb,  ub=ci.ub)

ci_3<-bind_rows(
  data.frame(cohort="pooled", region="Overall", ci.data3$ci.res),
  data.frame(cohort="pooled", ci.region3),
  ci.cohort3)



#Cumulative inc, no birth
d_noBW <- calc.ci.agecat(d_noBW, range=6)
ci.data.nobirth <- summary.ci(d_noBW, agelst)
ci.data.nobirth <- summary.ci(d, agelist=agelst)
ci.region.nobirth <- d %>% group_by(country) %>% do(summary.ci(.)$ci.res)
ci.cohort.nobirth <- ci.data.nobirth$ci.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  ci,  ci.lb,  ci.ub)) %>%
  rename( est=ci,  lb=ci.lb,  ub=ci.ub)

ci_nobw<-bind_rows(
  data.frame(cohort="pooled", region="Overall", ci.data.nobirth$ci.res),
  data.frame(cohort="pooled", ci.regio.nobirthn),
  ci.cohort.nobirth)


#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d_noBW, range=3)
agelst3=list("0-3 months","3-6 months","6-9 months","9-12 months",
             "12-15 months","15-18 months","18-21 months","21-24 months")
ci.data.nobirth3 <- summary.ci(d, agelist=agelst3)
ci.region.nobirth3 <- d %>% group_by(country) %>% do(summary.ci(.)$ci.res)
ci.cohort.nobirth3 <- ci.data.nobirth3$ci.cohort %>% subset(., select=c(cohort,region,agecat, nmeas,  ci,  ci.lb,  ci.ub)) %>%
  rename( est=ci,  lb=ci.lb,  ub=ci.ub)

ci_nobw3<-bind_rows(
  data.frame(cohort="pooled", region="Overall", ci.data.nobirth3$ci.res),
  data.frame(cohort="pooled", ci.region.nobirth3),
  ci.cohort.nobirth3)




#Recovery cumulative inc
#NOTE: need to make sure to only include those wasted in the denominator
#rec.data <- summary.ci(d, recovery = T)

#Incidence rate
ir.data <- summary.ir(d, agelist=agelst)

#Incidence rate - no birth wasting
ir.data.nobirth <- summary.ir(d_noBW, agelist=agelst)

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





india_desc_data<- rbind(
  data.frame(outcome="wasting", measure="prevalence", prev),
  data.frame(outcome="severe wasting", measure="prevalence", sev.prev),
  data.frame(outcome="whz", measure="mean", whz)
)

#Need to add in the median durations
save(india_desc_data, file="U:/Data/Wasting/Wasting_descriptive_epi_results_India.Rdata")





