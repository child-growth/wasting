

rm(list=ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Wasting_inc_data.RData")



#load covariates
cov<-readRDS("U:/ucb-superlearner/stunting rallies/FINAL_clean_covariates.rds")


d <- left_join(d, cov, by=c("studyid", "subjid", "country"))


d<-d[d$measurefreq=="monthly",]


dfull <- d %>% filter(agedays<24*30.41)


#d <- dfull %>% filter(studyid=="ki1017093-NIH-Birth")
#d <- dfull %>% filter(!is.na(birthwt))
d <- dfull %>% filter(studyid!="ki1114097-CONTENT" & studyid!="ki1017093b-PROVIDE")


ggplot(d, aes(x=agedays, y=whz, group=birthwt)) + geom_smooth()




ggplot(d, aes(x=agedays, y=whz, group=pers_wast)) + geom_smooth()


df <- d %>% group_by(subjid) %>% mutate(numwast=sum(whz < (-2)))
table(df$numwast)

df$wastcat <- cut(df$numwast, breaks=c(-1, .5, 1.5, 100))
table(df$wastcat)

ggplot(df, aes(x=agedays, y=whz, group=wastcat, color=wastcat)) + geom_smooth()



# df <- dfull %>% group_by(studyid, subjid) %>% mutate(numwast=sum(whz < (-2)))
# df$wastcat <- cut(df$numwast, breaks=c(-1, .5, 1.5, 100))
# ggplot(df, aes(x=agedays, y=whz, group=wastcat, color=wastcat)) + geom_smooth()


ggplot(df, aes(x=agedays, y=whz, color=wastcat)) + geom_line(aes(group=subjid), alpha=0.1) + geom_smooth()+ facet_wrap(~wastcat)


df <- d %>% group_by(subjid) %>% mutate(numwast=sum(whz < (-2)))
df$wastcat <- cut(df$numwast, breaks=c(-1, .5, 1.5, 100))
table(df$wastcat)
df <- df %>% group_by(subjid) %>% arrange(agedays) %>% mutate(bornwast=first(whz) < (-2))
#ggplot(df, aes(x=agedays, y=whz, color=wastcat)) + geom_line(aes(group=subjid), alpha=0.1) + geom_smooth()+ facet_grid(bornwast~wastcat) + geom_hline(yintercept= -2)


#ggplot(df, aes(x=agedays, y=whz, color=wastcat)) + geom_line(aes(group=subjid), alpha=0.1) + geom_smooth()+ facet_grid(bornwast~pers_wast) + geom_hline(yintercept= -2)


df <- df %>% group_by(subjid) %>% arrange(agedays) %>% mutate(num_inc=sum(wast_inc))
df$wastinccat <- cut(df$num_inc, breaks=c(-1, .5, 1.5, 100))

ggplot(df, aes(x=agedays, y=whz, color=wastcat)) + geom_line(aes(group=subjid), alpha=0.1) + geom_smooth()+ facet_grid(bornwast~wastinccat) + geom_hline(yintercept= -2)






#d <- dfull

d$agelowwhz <- NA
d$agelowwhz[d$whz < (-2)] <- d$agedays[d$whz < (-2)] 
df <- d  %>% arrange(studyid, subjid,agedays) %>% group_by(studyid, subjid) %>% mutate(agelowwhz=first(agelowwhz))
df <- df %>% arrange(studyid, subjid,agedays) %>% group_by(studyid, subjid) %>% mutate(bornwast= (firstmeasure & whz < (-2)))
df$agewastcat <- cut(df$agelowwhz, breaks=c(-1, 3*30.41, 6*30.41, 12*30.41, 1000))
table(df$agewastcat)
table(df$agewastcat, df$bornwast)
df$agewastcat <- as.character(df$agewastcat)
df$agewastcat[df$bornwast==1] <- "born wast"
df$agewastcat[is.na(df$agewastcat)] <- "never wast"
df$agewastcat <- factor(df$agewastcat)
table(df$agewastcat)



ggplot(df, aes(x=agedays, y=whz)) + geom_line(aes(group=subjid), alpha=0.1) + 
  geom_smooth()+ facet_wrap(~agewastcat) + geom_hline(yintercept= -2)








table(d$birthwt, d$wast, d$firstmeasure)


#Make state sequences
d2 <- d
d <- d2




calc.monthly.agecat2<-function(d){
  d$agecat <- cut(d$agedays, breaks=c(0:24)*30.4167, include.lowest = F,
                  labels =paste0(1:24, " months"))
  levels(d$agecat)[1] <- "One month"
  table(d$agecat)
  return(d)
}
calc.monthly.agecat3 <- function(d){
  d$agecat <- cut(d$agedays, breaks=c(0:12)*30.4167*2, include.lowest = F,
                  labels =paste0((1:12*2)-1, " months"))
  levels(d$agecat)[1] <- "One month"
  table(d$agecat)
  return(d)
}


d <- calc.monthly.agecat3(d)

d <- d %>% filter(!is.na(agecat)) %>% group_by(studyid, subjid, agecat,birthwt) %>% summarise(whz=mean(whz), healthy=1*(whz >= (-0.5)), low=1*(whz < (-0.5) & whz >= (-2)), wast=1*(whz < (-2)), sevwast=1*(whz < (-3))) %>% 
  group_by(studyid, subjid) %>% mutate(N=n()) %>% filter(first(agecat)=="One month")
table(d$N)

d <- d %>% filter(N==12)

d$status <- NA
d$status[d$agecat=="One month" & d$wast==1] <- "Born wast"
#d$status[d$agecat=="One month" & d$wast==0] <- "Born not wast"
d$status[d$agecat!="One month" & d$healthy==1] <- "Healthy"
d$status[d$agecat!="One month" & d$low==1] <- "Low"

library(zoo)
d <- d %>% group_by(studyid, subjid) %>% na.locf %>% ungroup

d$status[d$agecat!="One month" & d$wast==1 & d$status!="Born wast"] <- "Wast"
d$status[d$agecat!="One month" & d$sevwast==1 & d$status!="Born wast"] <- "Sev wast"
table(d$status)
table(is.na(d$status))

d <- d %>% filter(!is.na(status))

d$id <- paste0(as.numeric(factor(d$studyid)),"_",d$subjid)

d <- d %>% subset(., select=c(id, agecat, status, birthwt))




data_wide <- spread(d, agecat, status)
data_wide

write.csv(data_wide, file="U:/data/trajectory_status.csv")








d2<-d
d<-d2



d <- calc.ci.agecat(d, range=6)
d$agecat <- as.character(d$agecat)
d$agecat[d$agecat=="12-18 months" | d$agecat=="18-24 months"] <- "12-24 months"
d$agecat <- factor(d$agecat)
table(d$agecat)


d$birthwt <- as.character(d$birthwt)
d$birthwt[is.na(d$birthwt)] <- "missing"

d <- d %>% group_by(studyid, subjid, agecat) %>% mutate(mean6mo=mean(whz)) %>% group_by(agecat) %>% mutate(mean6mo_cat=ntile(mean6mo, 3)) 
table(d$mean6mo_cat, d$birthwt)

d2 <- d %>%
  group_by(studyid, subjid, agecat) %>% slice(1) %>% group_by(studyid, subjid) %>%  
  summarise(N=n(),growthcat = toString(mean6mo_cat)) %>% filter(N==3) %>%
  ungroup()
head(d2)
table(d2$growthcat)

d <- left_join(d, d2, by=c("studyid", "subjid"))

d <- d %>% group_by(growthcat) %>% mutate(N=n()) %>% arrange(-N) %>% filter(N>1000)
d$growthcat <- factor(d$growthcat, levels=unique(d$growthcat))
d <- d %>% filter(!is.na(growthcat))

ggplot(d, aes(x=agedays, y=whz)) + geom_line(aes(group=subjid), alpha=0.01) + 
  geom_smooth(aes(group=birthwt, color=birthwt))+ facet_wrap(~growthcat) + geom_hline(yintercept= -2)



