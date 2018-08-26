
#-----------------------------------
# Stunting Outcomes - Risk factor analysis
# Repeat sections of descriptive epi
# scripts to calculate the outcomes on
# the risk factor dataset (monthly and
# quarterly, all arms of RCTs)
#-----------------------------------
rm(list=ls())
library(tidyverse)



load("U:/Data/Stunting/int_stunting_data.RData")


#Subset to intervention studies
d <- d %>% filter(tr!="")


#--------------------------------------
# Calculate cumulative incidence of
# Stunting in certain age ranges for the
# risk factor analysis
#--------------------------------------

# define age windows
d = d %>% 
  #filter(agedays>1) %>%
  mutate(agecat=ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
                       ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                              ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                     ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))) %>%
  mutate(agecat=factor(agecat,levels=c("6 months","12 months","18 months","24 months")))

d <- d %>% ungroup() %>% arrange(studyid,country,subjid, agedays) %>% 
  group_by(studyid,country,subjid) %>% 
  mutate(stunt= as.numeric(haz < -2), numstunt=cumsum(stunt)) %>%
  group_by(studyid,country,subjid, agecat) %>% 
  mutate(minhaz=min(haz), ever_stunted=ifelse(minhaz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() 


#calculate any stunting from 0-6
stunt_ci_0_6 = d %>% ungroup() %>%
  filter(agecat=="6 months") %>%
  group_by(studyid,country,subjid) %>%
  mutate(agecat="0-6 months", minhaz=min(haz), ever_stunted=ifelse(minhaz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() 

#If the child was stunted at the start of the cumulative incidence age range, the child is not in the risk set and for 
#stunted and there cannot be an incident of stunting unless the child recovers and then drops back below -2 during the age 
#range.

#calculate any stunting from 6-24
stunt_ci_6_24 = d %>% ungroup() %>% 
  arrange(studyid,country,subjid, agedays) %>% 
  filter(agecat!="6 months" & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  #mark if children started stunted. They need to recover to be included in the at-risk pool
  mutate(start_stunt= as.numeric(first(haz) < -2), cumsum_notwasted=cumsum(as.numeric(haz >= -2)), anyrecovery=max(cumsum_notwasted)>0) %>%
  filter((anyrecovery & cumsum_notwasted!=0 & start_stunt==1) | start_stunt==0) %>% #drop children never at risk (start stunted and never recovered) and drop obs prior to recovery
  mutate(agecat="6-24 months", minhaz=min(haz), ever_stunted=ifelse(minhaz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() %>%
  select(studyid,subjid, country,tr,agedays,haz, measurefreq, measid, agecat,minhaz, ever_stunted,N)

#calculate any stunting from 0-24
stunt_ci_0_24 = d %>% ungroup() %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  mutate(agecat="0-24 months", minhaz=min(haz), ever_stunted=ifelse(minhaz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() 

stunt_ci_0_6 <- stunt_ci_0_6 %>% subset(., select = -c(stunt, numstunt))
stunt_ci_0_24 <- stunt_ci_0_24 %>% subset(., select = -c(stunt, numstunt))

cuminc <- rbind(stunt_ci_0_6, stunt_ci_6_24, stunt_ci_0_24)

table(cuminc$agecat)
table(cuminc$agecat, cuminc$ever_stunted)

#--------------------------------------
# Calculate prevalence of
# Stunting in certain age ranges for the
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
  summarise(haz=mean(haz)) %>%
  mutate(stunted=ifelse(haz< -2, 1,0),sstunted=ifelse(haz< -3, 1,0))


# export
prev = dmn %>% 
  filter(agecat=="Birth" | agecat=="6 months" | agecat=="24 months") %>%
select(studyid,subjid,country,agecat,
       stunted, sstunted)



#--------------------------------------
# save datasets
#--------------------------------------

int_cuminc <- cuminc
int_prev <- prev

save(int_cuminc, file="U:/ucb-superlearner/Stunting rallies/st_int_prev.RData")
save(int_prev, file="U:/ucb-superlearner/Stunting rallies/st_int_cuminc.rdata")
