
#-----------------------------------
# Wasting Outcomes - Risk factor analysis
# Repeat sections of descriptive epi
# scripts to calculate the outcomes on
# the risk factor dataset (monthly and
# quarterly, all arms of RCTs)
#-----------------------------------
rm(list=ls())
library(tidyverse)



load("U:/Data/Wasting/rf_wasting_data.RData")

#--------------------------------------
# Calculate cumulative incidence of
# Wasting in certain age ranges for the
# risk factor analysis
#--------------------------------------

# define age windows
d = d %>% 
  #filter(agedays>1) %>%
  mutate(agecat=ifelse(agedays<=6*30.4167,"6 months",
                              ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                                     ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                            ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))) %>%
  mutate(agecat=factor(agecat,levels=c("6 months","12 months","18 months","24 months")))

d <- d %>% ungroup() %>% arrange(studyid,country,subjid, agedays) %>% 
  group_by(studyid,country,subjid) %>% 
  mutate(stunt= as.numeric(whz < -2), numstunt=cumsum(stunt)) %>%
  group_by(studyid,country,subjid, agecat) %>% 
  mutate(minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() 


#calculate any Wasting from 0-6
stunt_ci_0_6 = d %>% ungroup() %>%
  filter(agecat=="6 months") %>%
  group_by(studyid,country,subjid) %>%
  mutate(agecat="0-6 months", minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() %>% subset(., select=-c(stunt, numstunt))

#If the child was stunted at the start of the cumulative incidence age range, the child is not in the risk set and for 
#stunted and there cannot be an incident of Wasting unless the child recovers and then drops back below -2 during the age 
#range.

#calculate any Wasting from 6-24
stunt_ci_6_24 = d %>% ungroup() %>% 
  arrange(studyid,country,subjid, agedays) %>% 
  filter(agecat!="6 months" & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  #mark if children started stunted. They need to recover to be included in the at-risk pool
  mutate(start_stunt= as.numeric(first(whz) < -2), cumsum_notwasted=cumsum(as.numeric(whz >= -2)), anyrecovery=max(cumsum_notwasted)>0) %>%
  filter((anyrecovery & cumsum_notwasted!=0) | start_stunt==0) %>% #drop children never at risk (start stunted and never recovered) and drop obs prior to recovery
  mutate(agecat="6-24 months", minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() %>%
  select(studyid,subjid, country,tr,agedays,whz, measurefreq, measid, agecat,minwhz, ever_stunted,N)

#calculate any Wasting from 0-24
stunt_ci_0_24 = d %>% ungroup() %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  mutate(agecat="0-24 months", minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup()  %>% subset(., select=-c(stunt, numstunt))


cuminc <- rbind(stunt_ci_0_6, stunt_ci_6_24, stunt_ci_0_24)

table(cuminc$agecat)
table(cuminc$agecat, cuminc$ever_stunted)

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
  mutate(stunted=ifelse(whz< -2, 1,0),sstunted=ifelse(whz< -3, 1,0))


# export
prev = dmn %>% 
  filter(agecat=="Birth" | agecat=="6 months" | agecat=="24 months")
select(studyid,subjid,country,agecat,
       stunted, sstunted)



#--------------------------------------
# Calculate recovery from
# Wasting in certain age ranges for the
# risk factor analysis
#--------------------------------------

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

# define age windows with 2 week buffer around age point
# (ie, for 6 months, consider recovery in the window  up to 7 months)
d = d %>% 
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>1 & agedays<=3.5*30.4167,"3 months",
                              ifelse(agedays>3.5*30.4167 & agedays<=6.5*30.4167,"6 months",
                                     ifelse(agedays>6.5*30.4167 & agedays<=9.5*30.4167,"9 months",
                                            ifelse(agedays>9.5*30.4167 & agedays<=12.5*30.4167,"12 months",
                                                   ifelse(agedays>12.5*30.4167 & agedays<=18.5*30.4167,"18 months",
                                                          ifelse(agedays>18.5*30.4167& agedays<=24.5*30.4167,"24 months","")))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# subset to stunted between birth and 3 months
stunt.03 <- d %>%
  filter(agecat=="Birth" | agecat=="3 months") %>%
  group_by(studyid,country,subjid) %>%
  mutate(measid=seq_along(subjid))  %>%
  mutate(stunted=ifelse(whz< -2,1,0),
         lagstunted=lag(stunted),
         leadstunted=lead(stunted))  %>%
  # unique Wasting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0)) %>%
  # identify whether child had Wasting episode between 0 and 3 months 
  summarise(stunted03=max(sepisode,na.rm=TRUE))

rec.24 <- d %>%
  filter(agecat=="24 months") %>%
  # identify last two measurements prior to 24 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(whz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec24=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

rev <- full_join(stunt.03, rec.24,by=c("studyid","country","subjid")) %>%
  mutate(s03rec24=ifelse(stunted03==1 & rec24==1,1,0)) %>%
  select(studyid, country,subjid, s03rec24)

# prepare data for pooling 
rev.data <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn=mean(s03rec24,na.rm=TRUE),
            n=sum(s03rec24,na.rm=TRUE),
            N=sum(!is.na(s03rec24)))

# estimate random effects, format results
rev.fit=rma(ni=rev.data$N, xi=rev.data$n, 
            method="REML", measure="PR")
rev.res=data.frame(est=rev.fit$beta, se=rev.fit$se, lb=rev.fit$ci.lb, ub=rev.fit$ci.ub)

# number of cohorts
nrow(rev.data)

# number of children
sum(rev.data$N)



#--------------------------------------
# save datasets
#--------------------------------------


save(cuminc, file="U:/ucb-superlearner/Wasting rallies/st_prev.RData")
save(prev, file="U:/ucb-superlearner/Wasting rallies/st_cuminc.rdata")
save(rev, file="U:/ucb-superlearner/Wasting rallies/st_rec.RData")

#-----------------------------------
# Wasting Outcomes - Risk factor analysis
# Repeat sections of descriptive epi
# scripts to calculate the outcomes on
# the risk factor dataset (monthly and
# quarterly, all arms of RCTs)
#-----------------------------------
library(tidyverse)



load("U:/Data/Wasting/rf_Wasting_data.RData")

#--------------------------------------
# Calculate cumulative incidence of
# Wasting in certain age ranges for the
# risk factor analysis
#--------------------------------------


#calculate any Wasting from 0-6
stunt_ci_0_6 = d %>% ungroup() %>%
  filter(agecat=="6 months") %>%
  group_by(studyid,country,subjid) %>%
  #create variable with minwhz by age category, cumulatively
  mutate(agecat="0-6 months", minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() 

#calculate any Wasting from 6-24
stunt_ci_6_24 = d %>% ungroup() %>% 
  arrange(studyid,country,subjid, agedays) %>% 
  filter(agecat!="6 months" & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  #mark if children started stunted. They need to recover to be included in the at-risk pool
  mutate(start_stunt= as.numeric(first(whz) < -2), cumsum_notwasted=cumsum(as.numeric(whz >= -2)), anyrecovery=max(cumsum_notwasted)>0) %>%
  filter((anyrecovery & cumsum_notwasted!=0 & start_stunt==1) | start_stunt==0) %>% #drop children never at risk (start stunted and never recovered) and drop obs prior to recovery
  mutate(agecat="6-24 months", minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() %>%
  select(studyid,subjid, country,tr,agedays,whz, measurefreq, measid, agecat,minwhz, ever_stunted,N)

#calculate any Wasting from 0-24
stunt_ci_0_24 = d %>% ungroup() %>%
  filter(agecat!="6 months" & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  #create variable with minwhz by age category, cumulatively
  mutate(agecat="0-24 months", minwhz=min(whz), ever_stunted=ifelse(minwhz< -2,1,0), N=n()) %>% slice(1) %>%
  ungroup() 

stunt_ci_0_6 <- stunt_ci_0_6 %>% subset(., select = -c(stunt, numstunt))
stunt_ci_0_24 <- stunt_ci_0_24 %>% subset(., select = -c(stunt, numstunt))

cuminc <- rbind(stunt_ci_0_6, stunt_ci_6_24, stunt_ci_0_24)


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
  mutate(stunted=ifelse(whz< -2, 1,0),sstunted=ifelse(whz< -3, 1,0))


# export
prev = dmn %>% 
  filter(agecat=="Birth" | agecat=="6 months" | agecat=="24 months") %>%
select(studyid,subjid,country,agecat,
       stunted, sstunted)



#--------------------------------------
# Calculate recovery from
# Wasting in certain age ranges for the
# risk factor analysis
#--------------------------------------

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

# define age windows with 2 week buffer around age point
# (ie, for 6 months, consider recovery in the window  up to 7 months)
d = d %>% 
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>1 & agedays<=3.5*30.4167,"3 months",
                              ifelse(agedays>3.5*30.4167 & agedays<=6.5*30.4167,"6 months",
                                     ifelse(agedays>6.5*30.4167 & agedays<=9.5*30.4167,"9 months",
                                            ifelse(agedays>9.5*30.4167 & agedays<=12.5*30.4167,"12 months",
                                                   ifelse(agedays>12.5*30.4167 & agedays<=18.5*30.4167,"18 months",
                                                          ifelse(agedays>18.5*30.4167& agedays<=24.5*30.4167,"24 months","")))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# subset to stunted between birth and 3 months
stunt.03 <- d %>%
  filter(agecat=="Birth" | agecat=="3 months") %>%
  group_by(studyid,country,subjid) %>%
  mutate(measid=seq_along(subjid))  %>%
  mutate(stunted=ifelse(whz< -2,1,0),
         lagstunted=lag(stunted),
         leadstunted=lead(stunted))  %>%
  # unique Wasting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0)) %>%
  # identify whether child had Wasting episode between 0 and 3 months 
  summarise(stunted03=max(sepisode,na.rm=TRUE))

rec.24 <- d %>%
  filter(agecat=="24 months") %>%
  # identify last two measurements prior to 24 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(whz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec24=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

rev <- full_join(stunt.03, rec.24,by=c("studyid","country","subjid")) %>%
  mutate(s03rec24=ifelse(stunted03==1 & rec24==1,1,0)) %>%
  select(studyid, country,subjid, s03rec24)

#--------------------------------------
# Format and subset the growth velocity dataset
#--------------------------------------
vel <- readRDS(file="U:/UCB-SuperLearner/Wasting rallies/velocity_longfmt.rds")

#Drop yearly studies
vel <- vel[!vel$studyid %in% c(
  "ki1000110-WASH-Bangladesh",       
  "ki1000111-WASH-Kenya",  
  "ki1148112-iLiNS-DOSE",     
  "ki1148112-iLiNS-DYAD-M", 
  "ki1033518-iLiNS-DYAD-G",
  "ki1000125-AgaKhanUniv",           
  "ki1112895-Burkina Faso Zn",    
  "ki1000304-VITAMIN-A",  
  "ki1000304-Vitamin-B12",
  "ki1000107-Serrinha-VitA",   
  "ki1000304-EU",        
  "ki1000304-ZnMort"),]

vel <- vel[!(vel$studyid=="ki1135781-COHORTS" & vel$country=="BRAZIL"),] #Drop because yearly 
vel <- vel[!(vel$studyid=="ki1135781-COHORTS" & vel$country=="SOUTH AFRICA"),] #Drop because yearly 

class(vel$subjid)

#Get only whz change from growth velocity dataset, and format names
vel_whz <- vel %>% filter(ycat=="whz") %>% subset(., select=c(studyid, country, subjid, y_rate, diffcat)) %>%
  rename(agecat = diffcat)




#--------------------------------------
# save datasets
#--------------------------------------


save(prev, file="U:/ucb-superlearner/Wasting rallies/st_prev_rf_outcomes.RData")
save(cuminc, file="U:/ucb-superlearner/Wasting rallies/st_cuminc_rf_outcomes.rdata")
save(rev, file="U:/ucb-superlearner/Wasting rallies/st_rec_rf_outcomes.RData")
save(vel_whz, vel_lencm, file="U:/ucb-superlearner/Wasting rallies/st_vel_rf_outcomes.RData")
