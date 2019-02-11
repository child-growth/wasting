



rm(list = ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Wast_int_inc_data.RData")
d<-d_int

#Prevalence - yearly studies
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)
prev.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(.)$prev.res)
prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",])$prev.res
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

prev.yearly <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", prev.data$prev.res),
  data.frame(cohort = "pooled", prev.region),
  prev.cohort
)

#Severe wasting prevalence- yearly studies
sev.prev.data <- summary.prev(d, severe.wasted = T)
sev.prev.region <-
  d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(., severe.wasted = T)$prev.res)
sev.prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",], severe.wasted = T)$prev.res
sev.prev.cohort <-
  sev.prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

sev.prev.yearly <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", sev.prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", sev.prev.data$prev.res),
  data.frame(cohort = "pooled", sev.prev.region),
  sev.prev.cohort
)

#mean whz- yearly studies
whz.data <- summary.whz(d)
whz.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.whz(.)$whz.res)
whz.Pakistan <- summary.whz(d[d$country=="PAKISTAN",])$whz.res
whz.cohort <-
  whz.data$whz.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwhz,  lb = ci.lb,  ub = ci.ub)

whz.yearly <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", whz.data$whz.res),
  data.frame(cohort = "pooled", region = "Pakistan", whz.Pakistan),
  data.frame(cohort = "pooled", whz.region),
  whz.cohort
)




#Subset to monthly
d <- d %>% filter(measurefreq == "monthly")
d_noBW <- d_int_noBW %>% filter(measurefreq == "monthly")


#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)
prev.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(.)$prev.res)
prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",])$prev.res
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

prev <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", prev.data$prev.res),
  data.frame(cohort = "pooled", prev.region),
  prev.cohort
)

#Severe wasting prevalence
sev.prev.data <- summary.prev(d, severe.wasted = T)
sev.prev.region <-
  d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(., severe.wasted = T)$prev.res)
sev.prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",], severe.wasted = T)$prev.res
sev.prev.cohort <-
  sev.prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

sev.prev <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", sev.prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", sev.prev.data$prev.res),
  data.frame(cohort = "pooled", sev.prev.region),
  sev.prev.cohort
)

#mean whz
whz.data <- summary.whz(d)
whz.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.whz(.)$whz.res)
whz.Pakistan <- summary.whz(d[d$country=="PAKISTAN",])$whz.res
whz.cohort <-
  whz.data$whz.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwhz,  lb = ci.lb,  ub = ci.ub)

whz <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", whz.data$whz.res),
  data.frame(cohort = "pooled", region = "Pakistan", whz.Pakistan),
  data.frame(cohort = "pooled", whz.region),
  whz.cohort
)




#Cumulative inc
d <- calc.ci.agecat(d, range = 6)
agelst = list("0-6 months", "6-12 months", "12-18 months", "18-24 months")
ci.data <- summary.ci(d, agelist = agelst)
ci.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst)$ci.res)
ci.Pakistan <- summary.ci(d[d$country=="PAKISTAN",], agelist = agelst)$ci.res
ci.cohort <-
  ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub, nchild)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)

ci <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan),
  data.frame(cohort = "pooled", ci.region),
  ci.cohort
)

#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d, range = 3)
agelst3 = list(
  "0-3 months",
  "3-6 months",
  "6-9 months",
  "9-12 months",
  "12-15 months",
  "15-18 months",
  "18-21 months",
  "21-24 months"
)
ci.data3 <- summary.ci(d3, agelist = agelst3)
ci.region3 <- d3 %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst3)$ci.res)
ci.Pakistan3 <- summary.ci(d3[d3$country=="PAKISTAN",], agelist = agelst3)$ci.res
ci.cohort3 <-
  ci.data3$ci.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub, nchild)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)

ci_3 <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data3$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan3),
  data.frame(cohort = "pooled", ci.region3),
  ci.cohort3
)



#Cumulative inc, no birth
d_noBW <- calc.ci.agecat(d_noBW, range = 6)
ci.data.nobirth <- summary.ci(d_noBW, agelist = agelst)
ci.region.nobirth <-
  d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst)$ci.res)
ci.Pakistan.nobirth <- summary.ci(d_noBW[d_noBW$country=="PAKISTAN",], agelist = agelst)$ci.res
ci.cohort.nobirth <-
  ci.data.nobirth$ci.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub, nchild)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)

ci_nobw <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data.nobirth$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan.nobirth),
  data.frame(cohort = "pooled", ci.region.nobirth),
  ci.cohort.nobirth
)


#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d_noBW, range = 3)
agelst3 = list(
  "0-3 months",
  "3-6 months",
  "6-9 months",
  "9-12 months",
  "12-15 months",
  "15-18 months",
  "18-21 months",
  "21-24 months"
)
ci.data.nobirth3 <- summary.ci(d3, agelist = agelst3)
ci.region.nobirth3 <-
  d3 %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst3)$ci.res)
ci.Pakistan.nobirth3 <- summary.ci(d3[d3$country=="PAKISTAN",], agelist = agelst3)$ci.res
ci.cohort.nobirth3 <-
  ci.data.nobirth3$ci.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub, nchild)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)

ci_nobw3 <- bind_rows(
  data.frame(
    cohort = "pooled",
    region = "Overall",
    ci.data.nobirth3$ci.res
  ),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan.nobirth3),
  data.frame(cohort = "pooled", ci.region.nobirth3),
  ci.cohort.nobirth3
)


#Cumulative inc of severe wasting
d <- calc.ci.agecat(d, range = 6)
agelst = list("0-6 months", "6-12 months", "12-18 months", "18-24 months")
sev.ci.data <- summary.ci(d, agelist = agelst, severe.wasted = T)
sev.ci.Pakistan <- summary.ci(d[d$country=="PAKISTAN",], agelist = agelst, severe.wasted = T)$ci.res
sev.ci.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst, severe.wasted = T)$ci.res)
sev.ci.cohort <-
  sev.ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub, nchild)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)

sev.ci <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", sev.ci.Pakistan),
  data.frame(cohort = "pooled", sev.ci.region),
  sev.ci.cohort
)




#Incidence rate
ir.data <- summary.ir(d, agelist = agelst)
ir.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
ir.Pakistan <- summary.ir(d[d$country=="PAKISTAN",], agelist = agelst)$ir.res
ir.cohort <-
  ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

ir <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
  data.frame(cohort = "pooled", region = "Pakistan", ir.Pakistan),
  data.frame(cohort = "pooled", ir.region),
  ir.cohort
)

#Incidence rate - no birth wasting
ir.data.nobirth <- summary.ir(d_noBW, agelist = agelst)
ir.region.nobirth <- d_noBW %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
ir.Pakistan.nobirth <- summary.ir(d_noBW[d_noBW$country=="PAKISTAN",], agelist = agelst)$ir.res
ir.cohort.nobirth <-
  ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

ir_noBW <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
  data.frame(cohort = "pooled", region = "Pakistan", ir.Pakistan.nobirth),
  data.frame(cohort = "pooled", ir.region),
  ci.cohort
)

#Incidence rate - severe wasting
sev.ir.data <- summary.ir(d, sev.wasting = T, agelist = agelst)
sev.ir.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ir(., agelist = agelst, sev.wasting = T)$ir.res)
sev.ir.Pakistan <- summary.ir(d[d$country=="PAKISTAN",], agelist = agelst, sev.wasting = T)$ir.res
sev.ir.cohort <-
  sev.ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub)

sev.ir <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ir.Pakistan.nobirth),
  data.frame(cohort = "pooled", sev.ci.region),
  sev.ci.cohort
)



d <- calc.ci.agecat(d, range = 6)

#Persistant wasting
perswast.data <- summary.perswast(d, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
perswast.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.perswast(., agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$pers.res)
perswast.Pakistan <- summary.perswast(d[d$country=="PAKISTAN",], agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$pers.res
perswast.cohort <-
  perswast.data$pers.cohort %>% subset(., select = c(cohort, region, agecat,  y,  ci.lb,  ci.ub, nchild)) %>%
  rename(est = y,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)

perswast <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", perswast.data$pers.res),
  data.frame(cohort = "pooled", region = "Pakistan", perswast.Pakistan),
  data.frame(cohort = "pooled", perswast.region),
  perswast.cohort
)







shiny_desc_data <- bind_rows(
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Prevalence - yearly", outcome= "Wasting prevalence - including yearly studies", prev.yearly),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="yes", measure= "Prevalence - yearly",outcome="Severe wasting prevalence - including yearly studies", sev.prev.yearly),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Mean WLZ - yearly", outcome="Mean WLZ - including yearly studies", whz.yearly),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Prevalence", outcome="Wasting prevalence", prev),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="yes", measure= "Prevalence", outcome="Severe wasting prevalence", sev.prev),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Mean WLZ", outcome="Mean WLZ", whz),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Cumulative incidence", outcome="Cumulative incidence - 3 month range", ci_3),
  data.frame(disease = "Wasting", age_range="6 months",   birth="no",  severe="no",   measure= "Cumulative incidence", outcome="Cumulative incidence - no birth wasting", ci_nobw),
  data.frame(disease = "Wasting", age_range="3 months",   birth="no",  severe="no",   measure= "Cumulative incidence", outcome=NA, ci_nobw3),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="yes", measure= "Cumulative incidence", outcome="Cumulative incidence - severe wasting", sev.ci),
  # data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="no", measure= "Incidence rate", outcome=NA, ir),
  # data.frame(disease = "Wasting", age_range="6 months",   birth="no", severe="no", measure= "Incidence rate", outcome=NA, ir_noBW),
  # data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="yes", measure= "Incidence rate", outcome=NA, sev.ir),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="no", measure= "Persistent wasting", outcome="Persistent wasting", perswast)#,
  # data.frame(disease = "Wasting", age_range="30 days",   birth="yes", severe="no", measure= "Recovery", rec30),
  # data.frame(disease = "Wasting", age_range="60 days",   birth="yes", severe="no", measure= "Recovery", rec60),
  # data.frame(disease = "Wasting", age_range="90 days",   birth="yes", severe="no", measure= "Recovery", rec90)
)














shiny_desc_data <- shiny_desc_data %>% subset(., select = -c(se, nmeas.f,  ptest.f, pt.f))

unique(shiny_desc_data$agecat)
shiny_desc_data$agecat <- factor(shiny_desc_data$agecat, levels=unique(shiny_desc_data$agecat))

unique(shiny_desc_data$region)
shiny_desc_data$region <- factor(shiny_desc_data$region, levels=c("Overall", "Africa", "Latin America", "Asia", "Pakistan"))


shiny_desc_data$agecat <- as.character(shiny_desc_data$agecat)
shiny_desc_data$agecat <- factor(shiny_desc_data$agecat, levels=
                                   c("Birth", "3 months",   "6 months",   "9 months",   "12 months",  "15 months",  "18 months",  "21 months", "24 months",  
                                     "0-3 months",  "3-6 months",  "6-9 months",  "9-12 months", "12-15 months", "15-18 months", "18-21 months", "21-24 months",
                                     "0-6 months",  "6-12 months", "12-18 months", "18-24 months"))

save(shiny_desc_data, file = "U:/Data/Wasting/pakistan_desc_data.Rdata")



