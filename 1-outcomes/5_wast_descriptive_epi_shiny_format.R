



rm(list = ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Wasting_inc_data.RData")
load("U:/Data/Wasting/Wasting_inc_noRec_data.RData")


#Subset to monthly
d <- d %>% filter(measurefreq == "monthly")
d_noBW <- d_noBW %>% filter(measurefreq == "monthly")
d_noRec <- d_noRec %>% filter(measurefreq == "monthly")
d_noBW_noRec <- d_noBW_noRec %>% filter(measurefreq == "monthly")


#Summary statistics
dfull <- d
d <- d %>% filter(agedays < 24*30.4167)
nrow(d)

# n children
length(unique(paste0(d$studyid, d$country, d$subjid)))

#range of sample sizes
d %>% group_by(studyid, country, subjid) %>% slice(1) %>%
  ungroup %>% group_by(studyid,country) %>% summarise(N=n()) %>% arrange(N)

#WHZ and prevalence
mean_sd(d$whz)
N_perc(d$whz < (-2))
N_perc(d$whz < (-3))

#Number and percent of kids ever wasted
df <- d %>% group_by(studyid, country, subjid) %>%
  summarize(minwhz=min(whz)) %>% mutate(wast=minwhz < (-2), sevwast=minwhz < (-3)) 
N_perc(df$wast)
N_perc(df$sevwast)


#Total number of unique episodes
N_perc(d$wast_inc)
N_perc(d$sevwast_inc)

#Incidence rate
d %>% group_by(studyid, subjid) %>% summarise(inc=sum(wast_inc), pt=sum(pt_wast)) %>% ungroup() %>%
  summarize(sum(inc)/sum(pt)*1000)

#Incidence without birth or enrollment
d_noBW %>% group_by(studyid, subjid) %>% summarise(inc=sum(wast_inc, na.rm=T), pt=sum(pt_wast, na.rm=T)) %>% ungroup() %>%
  summarize(sum(inc,  na.rm=T)/sum(pt,  na.rm=T)*1000)

#Severe Incidence rate
d %>% group_by(studyid, subjid) %>% summarise(inc=sum(sevwast_inc), pt=sum(pt_sevwast)) %>% ungroup() %>%
  summarize(sum(inc)/sum(pt)*1000)

# Total number of recoveries
sum(d$wast_rec)
sum(d$wast_rec)/sum(d$wast_inc) * 100


sum(d$sevwast_rec)
sum(d$sevwast_rec)/sum(d$sevwast_inc) * 100

# Proportion in 30, 60, 90 days
N_perc(d$wast_rec30d)
N_perc(d$wast_rec60d)
N_perc(d$wast_rec90d)

# Median Episode duration
median(d$wasting_duration[d$wast_inc==1], na.rm = T)

#	Overall longitudinal prevalence of persistent wasting
d %>% group_by(studyid, subjid) %>% summarise(N=n(), pers_wast=mean(whz < (-2))>=0.5) %>% filter(N>3) %>% ungroup() %>%
  summarize(n=sum(pers_wast), pers=n/n()*100)

d %>% group_by(studyid, subjid) %>% summarise(N=n(), pers_wast=mean(whz < (-3))>=0.5) %>% filter(N>3) %>% ungroup() %>%
  summarize(n=sum(pers_wast), pers=n/n()*100)


d <- dfull

#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)
prev.region <- d %>% group_by(region) %>% do(summary.prev(.)$prev.res)
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

prev <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", prev.data$prev.res),
  data.frame(cohort = "pooled", prev.region),
  prev.cohort
)

#Severe wasting prevalence
sev.prev.data <- summary.prev(d, severe.wasted = T)
sev.prev.region <-
  d %>% group_by(region) %>% do(summary.prev(., severe.wasted = T)$prev.res)
sev.prev.cohort <-
  sev.prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

sev.prev <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.prev.data$prev.res),
  data.frame(cohort = "pooled", sev.prev.region),
  sev.prev.cohort
)

#mean whz
whz.data <- summary.whz(d)
whz.region <- d %>% group_by(region) %>% do(summary.whz(.)$whz.res)
whz.cohort <-
  whz.data$whz.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwhz,  lb = ci.lb,  ub = ci.ub)

whz <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", whz.data$whz.res),
  data.frame(cohort = "pooled", whz.region),
  whz.cohort
)


#monthly mean whz
d <- calc.monthly.agecat(d)
monthly.data <- summary.whz(d)
monthly.region <- d %>% group_by(region) %>% do(summary.whz(.)$whz.res)
monthly.cohort <-
  monthly.data$whz.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwhz,  lb = ci.lb,  ub = ci.ub)

monthly.whz <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", monthly.data$whz.res),
  data.frame(cohort = "pooled", monthly.region),
  monthly.cohort
)




#Cumulative inc
d <- calc.ci.agecat(d, range = 6)
agelst = list("0-6 months", "6-12 months", "12-18 months", "18-24 months")
ci.data <- summary.incprop(d, agelist = agelst)
ci.region <- d %>% group_by(region) %>% do(summary.incprop(., agelist = agelst)$ci.res)
ci.cohort <-
  ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ci <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data$ci.res),
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
ci.data3 <- summary.incprop(d3, agelist = agelst3)
ci.region3 <- d3 %>% group_by(region) %>% do(summary.incprop(., agelist = agelst3)$ci.res)
ci.cohort3 <-
  ci.data3$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ci_3 <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data3$ci.res),
  data.frame(cohort = "pooled", ci.region3),
  ci.cohort3
)



#Cumulative inc, no birth
d_noBW <- calc.ci.agecat(d_noBW, range = 6)
ci.data.nobirth <- summary.incprop(d_noBW, agelist = agelst)
ci.region.nobirth <-
  d %>% group_by(region) %>% do(summary.incprop(., agelist = agelst)$ci.res)
ci.cohort.nobirth <-
  ci.data.nobirth$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ci_nobw <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data.nobirth$ci.res),
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
ci.data.nobirth3 <- summary.incprop(d3, agelist = agelst3)
ci.region.nobirth3 <-
  d3 %>% group_by(region) %>% do(summary.incprop(., agelist = agelst3)$ci.res)
ci.cohort.nobirth3 <-
  ci.data.nobirth3$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ci_nobw3 <- bind_rows(
  data.frame(
    cohort = "pooled",
    region = "Overall",
    ci.data.nobirth3$ci.res
  ),
  data.frame(cohort = "pooled", ci.region.nobirth3),
  ci.cohort.nobirth3
)


#Cumulative inc of severe wasting
d <- calc.ci.agecat(d, range = 6)
agelst = list("0-6 months", "6-12 months", "12-18 months", "18-24 months")
sev.ci.data <- summary.incprop(d, agelist = agelst, severe.wasted = T)
sev.ci.region <- d %>% group_by(region) %>% do(summary.incprop(., agelist = agelst, severe.wasted = T)$ci.res)
sev.ci.cohort <-
  sev.ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

sev.ci <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
  data.frame(cohort = "pooled", sev.ci.region),
  sev.ci.cohort
)



#Recovery cumulative inc
#NOTE: need to make sure to only include those wasted in the denominator
#rec.data <- summary.incprop(d, recovery = T)

#Incidence rate
ir.data <- summary.ir(d, agelist = agelst)
ir.region <- d %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
ir.cohort <-
  ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ir <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
  data.frame(cohort = "pooled", ir.region),
  ir.cohort
)

#Incidence rate - no birth wasting
ir.data.nobirth <- summary.ir(d_noBW, agelist = agelst)
ir.region.nobirth <- d_noBW %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
ir.cohort.nobirth <-
  ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ir_noBW <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
  data.frame(cohort = "pooled", ir.region),
  ci.cohort
)

#Incidence rate - severe wasting
sev.ir.data <- summary.ir(d, sev.wasting = T, agelist = agelst)
sev.ir.region <- d %>% group_by(region) %>% do(summary.ir(., agelist = agelst, sev.wasting = T)$ir.res)
sev.ir.cohort <-
  sev.ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

sev.ir <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
  data.frame(cohort = "pooled", sev.ci.region),
  sev.ci.cohort
)


#Recovery incidence rate
#rec.ir.data <- summary.ir(d, recovery = T)

#Recovery incidence rate - no birth wasting
#rec.ir.data.d_noBW <- summary.ir(d_noBW, recovery = T)

#Duration
dur.data <- summary.dur(d, agelist=list("0-6 months",  "6-12 months","12-18 months","18-24 months"))
dur.region <- d %>% group_by(region) %>% do(summary.dur(., agelist=list("0-6 months",  "6-12 months","12-18 months","18-24 months")))

#Recovery within 30, 60, 90 days
d <- calc.ci.agecat(d, range = 6)
rec.data30 <- summary.rec60( d, length = 30, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
rec.data60 <- summary.rec60( d, length = 60, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
rec.data90 <- summary.rec60( d, length = 90, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
rec30.region <- d %>% group_by(region) %>% do(summary.rec60( ., length = 30, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res)
rec60.region <- d %>% group_by(region) %>% do(summary.rec60( ., length = 60, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res)
rec90.region <- d %>% group_by(region) %>% do(summary.rec60( ., length = 90, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res)

rec30.cohort <-
  rec.data30$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)
rec60.cohort <-
  rec.data60$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)
rec90.cohort <-
  rec.data90$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

rec30<- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", rec.data30$ci.res),
  data.frame(cohort = "pooled", rec30.region),
  rec30.cohort
)
rec60<- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", rec.data60$ci.res),
  data.frame(cohort = "pooled", rec60.region),
  rec60.cohort
)
rec90<- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", rec.data90$ci.res),
  data.frame(cohort = "pooled", rec90.region),
  rec90.cohort
)

#Persistant wasting
perswast.data <- summary.perswast(d, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
perswast.region <- d %>% group_by(region) %>% do(summary.perswast(., agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$pers.res)
perswast.cohort <-
  perswast.data$pers.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

perswast <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", perswast.data$pers.res),
  data.frame(cohort = "pooled", perswast.region),
  perswast.cohort
)


#------------------------------------
# Calculate outcomes with no recovery
#------------------------------------


#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d_noRec, range = 3)
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
ci.data3 <- summary.incprop(d3, agelist = agelst3)
ci.region3 <- d3 %>% group_by(region) %>% do(summary.incprop(., agelist = agelst3)$ci.res)
ci.cohort3 <-
  ci.data3$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ci_3_noRec <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data3$ci.res),
  data.frame(cohort = "pooled", ci.region3),
  ci.cohort3
)


#Cumulative inc of severe wasting
sev.ci.data <- summary.incprop(d3, agelist = agelst3, severe.wasted = T)
sev.ci.region <- d3 %>% group_by(region) %>% do(summary.incprop(., agelist = agelst3, severe.wasted = T)$ci.res)
sev.ci.cohort <-
  sev.ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

sev.ci_noRec <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
  data.frame(cohort = "pooled", sev.ci.region),
  sev.ci.cohort
)



#Incidence rate
d_noRec <- calc.ci.agecat(d_noRec, range = 6)
ir.data <- summary.ir(d_noRec, agelist = agelst)
ir.region <- d_noRec %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
ir.cohort <-
  ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub)

ir_noRec <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
  data.frame(cohort = "pooled", ci.region),
  ir.cohort
)



# Cumulative Incidence 
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
ci.region3 <- d3  %>% group_by(region) %>% do(summary.ci(., agelist = agelst3)$ci.res)
ci.cohort3 <-
  ci.data3$ci.cohort %>% subset(., select = c(cohort, region, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)


cuminc <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data3$ci.res),
  data.frame(cohort = "pooled", ci.region3),
  ci.cohort3
)






shiny_desc_data <- bind_rows(
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Prevalence", prev),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="yes", measure= "Prevalence", sev.prev),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Mean WLZ",  whz),
  data.frame(disease = "Wasting", age_range="1 month",   birth="yes", severe="no", measure= "Mean WLZ",  monthly.whz),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="no", measure= "Incidence proportion", ci),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Incidence proportion", ci_3),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Cumulative incidence", cuminc),
  data.frame(disease = "Wasting", age_range="6 months",   birth="no",  severe="no",   measure= "Incidence proportion",  ci_nobw),
  data.frame(disease = "Wasting", age_range="3 months",   birth="no",  severe="no",   measure= "Incidence proportion",  ci_nobw3),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="yes", measure= "Incidence proportion",  sev.ci),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="no", measure= "Incidence rate",  ir),
  data.frame(disease = "Wasting", age_range="6 months",   birth="no", severe="no", measure= "Incidence rate",  ir_noBW),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="yes", measure= "Incidence rate",  sev.ir),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="no", measure= "Persistent wasting", perswast),
  data.frame(disease = "Wasting", age_range="30 days",   birth="yes", severe="no", measure= "Recovery", rec30),
  data.frame(disease = "Wasting", age_range="60 days",   birth="yes", severe="no", measure= "Recovery", rec60),
  data.frame(disease = "Wasting", age_range="90 days",   birth="yes", severe="no", measure= "Recovery", rec90),
  data.frame(disease = "Wasting", age_range="3 months",   birth="yes", severe="no", measure= "Incidence proportion - no recovery", ci_3_noRec),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="yes", measure= "Incidence proportion - no recovery", sev.ci_noRec),
  data.frame(disease = "Wasting", age_range="6 months",   birth="yes", severe="no", measure= "Incidence rate - no recovery", ir_noRec)
)




shiny_desc_data <- shiny_desc_data %>% subset(., select = -c(se,  ptest.f, pt.f))

unique(shiny_desc_data$agecat)
shiny_desc_data$agecat <- factor(shiny_desc_data$agecat, levels=unique(shiny_desc_data$agecat))

unique(shiny_desc_data$region)
shiny_desc_data$region <- factor(shiny_desc_data$region, levels=c("Overall", "Africa", "Latin America", "Asia", "Pakistan"))



save(shiny_desc_data, file = "U:/Data/Wasting/shiny_desc_data.Rdata")



