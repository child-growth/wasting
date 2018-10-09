


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




dfstunt <- df
dfstunt$whz <- dfstunt$haz
dstunt_noBW <- dfstunt %>% group_by(studyid, country) %>% do(WastIncCalc(., dropBornWasted=T, washout = 60)) 
dstunt <- dfstunt %>% group_by(studyid, country) %>% do(WastIncCalc(., washout=1000)) 



#Rename outcomes
colnames(dstunt)
dstunt <- dstunt %>% subset(., select=-c(haz)) %>% rename(haz=whz)
colnames(dstunt)<-gsub("wast","stunt",colnames(dstunt))

dstunt_noBW <- dstunt_noBW %>% subset(., select=-c(haz)) %>% rename(haz=whz)
colnames(dstunt_noBW)<-gsub("wast","stunt",colnames(dstunt_noBW))

#Drop incidence for children born or enrolled stunted
dstunt_noBW <- dstunt_noBW %>% group_by(studyid, subjid) %>% arrange(studyid, subjid, agedays) %>%
  mutate(cuminc=cumsum(stunt_inc))
dstunt_noBW$stunt_inc[dstunt_noBW$cuminc>1 & dstunt_noBW$stunt_inc==1] <- 0 

# dstunt <- dstunt %>% group_by(studyid, subjid) %>% arrange(studyid, subjid, agedays) %>%
#   filter(firstmeasure==F | stunt_inc==0)
#   #mutate(bornstunt=first(stunt_inc)==0)
# IS THIS RIGHT?

save(dstunt, dstunt_noBW, file="U:/Data/Wasting/Stunting_inc_data.RData")






