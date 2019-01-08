

#-----------------------------------
# Wasting Outcomes - Intervention 
# and mortality analysis
# Repeat sections of descriptive epi
# scripts to calculate the outcomes on
#-----------------------------------
rm(list=ls())
library(tidyverse)

#Load WHZ data
load("U:/Data/Wasting/rf_wasting_data.RData")
d_waz <- d %>% filter(waz > (-5) & waz < 5)

load("U:/Data/Wasting/Wast_int_inc_data.RData")
d <- d_int
d_noBW <- d_int_noBW



#--------------------------------------
# Calculate cumulative incidence of
# wasting in certain age ranges for the
# intervention and mortality analysis
#--------------------------------------


#calculate any wasting from 0-6
wast_ci_0_6 = d %>% ungroup() %>%
  filter(agedays<=6*30.4167) %>%
  group_by(studyid,country,subjid) %>%
  mutate(minwhz=min(whz), ever_wasted06=ifelse(minwhz< -2,1,0), ever_swasted06=ifelse(minwhz< -3,1,0),
         minhaz=min(haz), ever_stunted06=ifelse(minhaz< -2,1,0), ever_sstunted06=ifelse(minhaz< -3,1,0),
         pers_wasted06=as.numeric(mean(whz < (-2)) >= 0.5)) %>% slice(1) %>%
  ungroup() 

#calculate any wasting from 0-24
wast_ci_0_24 = d %>% ungroup() %>%
  filter(agedays<=24*30.4167) %>%
  group_by(studyid,country,subjid) %>%
  mutate(minwhz=min(whz), ever_wasted024=ifelse(minwhz< -2,1,0), ever_swasted024=ifelse(minwhz< -3,1,0),
         minhaz=min(haz), ever_stunted024=ifelse(minhaz< -2,1,0), ever_sstunted024=ifelse(minhaz< -3,1,0),
         pers_wasted024=as.numeric(mean(whz < (-2)) >= 0.5)) %>% slice(1) %>%
  ungroup() 

#calculate persistent wasting from 6-24
pers_wast_6_24 = d %>% ungroup() %>%
  filter(agedays>6*30.4167 & agedays<=24*30.4167) %>%
  group_by(studyid,country,subjid) %>%
  mutate(pers_wasted624=as.numeric(mean(whz < (-2)) >= 0.5), N=n()) %>% slice(1) %>%
  filter(N>1) %>% # must have at least 2 obs
  ungroup() 
table(pers_wast_6_24$pers_wasted624)


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
  mutate(agecat="0-6 months (no birth wast)", ever_wasted06_noBW=1*(sum(wast_inc, na.rm=T)>0), ever_swasted06_noBW=1*(sum(sevwast_inc, na.rm=T)>0)) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup() 

wast_ci_0_24_no_birth = d_noBW %>% ungroup() %>% 
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid, agedays) %>% 
  filter(wasting_episode!="Born Wasted") %>% #drop children born wasted
  mutate(agecat="0-24 months (no birth wast)", ever_wasted024_noBW=1*(sum(wast_inc, na.rm=T)>0), ever_swasted024_noBW=1*(sum(sevwast_inc, na.rm=T)>0)) %>% slice(1) %>%
  mutate(N=n()) %>%
  ungroup()


#--------------------------------------
# Calculate cumulative incidence of
# underweight in certain age ranges for the
# intervention and mortality analysis
#--------------------------------------


#calculate any underweight from 0-6
underweight_ci_0_6 = d_waz %>% ungroup() %>%
  filter(agedays<=6*30.4167) %>%
  group_by(studyid,country,subjid) %>%
  mutate(minwaz=min(waz), ever_underweight06=ifelse(minwaz< -2,1,0), ever_sunderweight06=ifelse(minwaz< -3,1,0)) %>% 
  slice(1) %>% ungroup() 

#calculate any underweight from 0-24
underweight_ci_0_24 = d_waz %>% ungroup() %>%
  filter(agedays<=24*30.4167) %>%
  group_by(studyid,country,subjid) %>%
  mutate(minwaz=min(waz), ever_underweight024=ifelse(minwaz< -2,1,0), ever_sunderweight024=ifelse(minwaz< -3,1,0)) %>% 
  slice(1) %>% ungroup() 



#--------------------------------------
# save datasets
#--------------------------------------

save(wast_ci_0_6, wast_ci_0_24, wast_ci_0_6_no_birth, wast_ci_0_24_no_birth, pers_wast_6_24,
     underweight_ci_0_6, underweight_ci_0_24, file="U:/ucb-superlearner/wasting rallies/wast_int_outcomes.rdata")
