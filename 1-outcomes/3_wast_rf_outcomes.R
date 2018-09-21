
#-----------------------------------
# Wasting Outcomes - Risk factor analysis
# Repeat sections of descriptive epi
# scripts to calculate the outcomes on
# the risk factor dataset (monthly and
# quarterly, all arms of RCTs)
#-----------------------------------
rm(list=ls())
library(tidyverse)



load("U:/Data/Wasting/Wasting_inc_data.RData")


#--------------------------------------
# Calculate prevalence of
# Wasting in certain age ranges for the
# risk factor analysis
#--------------------------------------

# define age windows
d = d %>% 
  arrange(studyid,subjid,agedays) %>%
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>2*30.4167 & agedays<4*30.4167,"3 months",
                              ifelse(agedays>5*30.4167 & agedays<7*30.4167,"6 months",
                                     ifelse(agedays>8*30.4167 & agedays<10*30.4167,"9 months",
                                            ifelse(agedays>11*30.4167 & agedays<13*30.4167,"12 months",
                                                   ifelse(agedays>17*30.4167 & agedays<19*30.4167,"18 months",
                                                          ifelse(agedays>23*30.4167& agedays<25*30.4167,"24 months","")))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","9 months",
                                       "12 months","18 months","24 months"))) 


# take mean of multiple measurements within age window
dmn <- d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid,agecat) %>%
  summarise(whz=mean(whz)) %>%
  mutate(wasted=ifelse(whz< -2, 1,0),swasted=ifelse(whz< -3, 1,0))


# export
prev = dmn %>% 
  filter(agecat=="Birth" | agecat=="6 months" | agecat=="24 months") %>% 
  select(studyid,subjid,country,agecat, wasted, swasted)



#--------------------------------------
# Calculate cumulative incidence of
# wasting in certain age ranges for the
# risk factor analysis
#--------------------------------------


# define age windows
d = d %>% 
  mutate(agecat=ifelse(agedays<=6*30.4167,"6 months",
                       ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                              ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                     ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))) %>%
  mutate(agecat=factor(agecat,levels=c("6 months","12 months","18 months","24 months")))



#calculate any wasting from 0-6
wast_ci_0_6 = d %>% ungroup() %>%
  filter(agecat=="6 months") %>%
  group_by(studyid,country,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(agecat="0-6 months", ever_wasted= 1*(sum(wast_inc, na.rm=T)>0), Nobs=n()) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup() 

# #calculate any wasting from 6-24
wast_ci_6_24 = d %>% ungroup() %>% 
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  filter(agecat!="6 months") %>%
  mutate(agecat="6-24 months",  ever_wasted=1*(sum(wast_inc, na.rm=T)>0), Nobs=n()) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup() 

#calculate any wasting from 0-24
wast_ci_0_24 = d %>% ungroup() %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(agecat="0-24 months", ever_wasted=1*(sum(wast_inc, na.rm=T)>0), Nobs=n()) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup() 

cuminc <- bind_rows(wast_ci_0_6, wast_ci_6_24, wast_ci_0_24)


#--------------------------------------
# Calculate cumulative incidence, excluding
# wasting at birth
#--------------------------------------

# define age windows
d_noBW = d_noBW %>% 
  mutate(agecat=ifelse(agedays<=6*30.4167,"6 months",
                       ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                              ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                     ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))) %>%
  mutate(agecat=factor(agecat,levels=c("6 months","12 months","18 months","24 months")))


wast_ci_0_6_no_birth = d_noBW %>% ungroup() %>% 
  arrange(studyid,country,subjid, agedays) %>% 
  filter(agecat=="6 months" & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  filter(wasting_episode!="Born Wasted") %>% #drop children born wasted
  mutate(agecat="0-6 months (no birth wast)", ever_wasted=1*(sum(wast_inc, na.rm=T)>0), Nobs=n()) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup() 

wast_ci_0_24_no_birth = d_noBW %>% ungroup() %>% 
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  filter(wasting_episode!="Born Wasted") %>% #drop children born wasted
  mutate(agecat="0-24 months (no birth wast)", ever_wasted=1*(sum(wast_inc, na.rm=T)>0), Nobs=n()) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup()

cuminc_nobirth <- rbind(wast_ci_0_6_no_birth, wast_ci_0_24_no_birth)


table(cuminc$ever_wasted[cuminc$agecat=="0-6 months"])
table(cuminc_nobirth$ever_wasted[cuminc_nobirth$agecat=="0-6 months (no birth wast)"])

table(cuminc$ever_wasted[cuminc$agecat=="0-24 months"])
table(cuminc_nobirth$ever_wasted[cuminc_nobirth$agecat=="0-24 months (no birth wast)"])


#--------------------------------------
# Calculate persistant wasting
#--------------------------------------

pers_wast_0_6 <- d %>% 
  filter(agecat=="6 months" & measurefreq=="monthly") %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  mutate(N=n()) %>% filter(N>=4) %>%
  mutate(perc_wasting=mean(whz < (-2))) %>% slice(1) %>%
  mutate(pers_wast = 1*(perc_wasting >= 0.5))

summary(pers_wast_0_6$perc_wasting)
table(pers_wast_0_6$pers_wast)

pers_wast_0_24 <- d %>% 
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  mutate(N=n()) %>% filter(N>=4) %>%
  mutate(perc_wasting=mean(whz < (-2))) %>% slice(1) %>%
  mutate(pers_wast = 1*(perc_wasting >= 0.5))

pers_wast_6_24 <- d %>% 
  filter(agecat!="6 months" & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  mutate(N=n()) %>% filter(N>=4) %>%
  mutate(perc_wasting=mean(whz < (-2))) %>% slice(1) %>%
  mutate(pers_wast = 1*(perc_wasting >= 0.5))

table(pers_wast_0_6$pers_wast)
table(pers_wast_6_24$pers_wast)
table(pers_wast_0_24$pers_wast)

mean(pers_wast_0_6$pers_wast)
mean(pers_wast_6_24$pers_wast)
mean(pers_wast_0_24$pers_wast)

summary(pers_wast_0_6$perc_wasting)
summary(pers_wast_6_24$perc_wasting)
summary(pers_wast_0_24$perc_wasting)

pers_wast <- bind_rows(pers_wast_0_6, pers_wast_6_24, pers_wast_0_24)


#--------------------------------------
# Calculate wasting recovery
#--------------------------------------

#calculate any wasting from 0-6
wast_rec_0_6 = d %>% ungroup() %>%
  filter(agecat=="6 months" & !is.na(wast_rec90d)) %>%
  group_by(studyid,country,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(agecat="0-6 months") %>% ungroup() 

# #calculate any wasting from 6-24
wast_rec_6_24 = d %>% ungroup() %>% 
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  filter(agecat!="6 months" & !is.na(wast_rec90d)) %>%
  mutate(agecat="6-24 months") %>% ungroup() 

#calculate any wasting from 0-24
wast_rec_0_24 = d %>% ungroup() %>%
  filter(!is.na(agecat) & !is.na(wast_rec90d)) %>%
  group_by(studyid,country,subjid) %>%
  mutate(agecat="0-24 months") %>% ungroup() 

rec <- bind_rows(wast_rec_0_6, wast_rec_6_24, wast_rec_0_24)



#--------------------------------------
# save datasets
#--------------------------------------


save(prev, file="U:/ucb-superlearner/Wasting rallies/wast_prev.RData")
save(cuminc, file="U:/ucb-superlearner/Wasting rallies/wast_cuminc.rdata")
save(cuminc_nobirth, file="U:/ucb-superlearner/Wasting rallies/wast_cuminc_nobirth.rdata")
save(pers_wast, file="U:/ucb-superlearner/Wasting rallies/pers_wast.rdata")
save(rec, file="U:/ucb-superlearner/Wasting rallies/wast_rec.rdata")









