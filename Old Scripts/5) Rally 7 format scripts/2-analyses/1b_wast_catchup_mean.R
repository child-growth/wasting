#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth 

# Cohort specific estimates & 
# Pooled estimates using random effects

# What is the mean duration of time for 
# reversing stunting (measured in cohorts 
# with at least monthly measurement)? 
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Stunting/stunting_data.RData")

# convert subjid to character
d <- d %>% 
  ungroup() %>%
  mutate(subjid=as.character(subjid))

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


# max measureid for each child
maxid = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  summarise(maxid=max(measid))

# create indicators for stunting
rev <- d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  # 
  mutate(stunted=ifelse(haz< -2,1,0),
         lagstunted=lag(stunted),
         leadstunted=lead(stunted))  %>%
  # unique stunting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0))

# make indicator for max measurement
rev2 <- full_join(rev, maxid, by=c("studyid","country","subjid"))  %>%
  # determine the start and end of each stunting episode
  mutate(start=ifelse(stunted==1 & lag(stunted)==0,1,0),
         end=case_when(
           stunted==1 & maxid==measid   ~ 1,
           stunted==0 & lag(stunted)==1 ~ 1,
           TRUE                         ~ 0
         )) %>%
  # keep starting and ending ages
  filter(start==1 | end==1) %>%
  # round age in months
  mutate(agem=round(agedays/30.4167)) %>%
  # calculate difference in age
  mutate(agemlag=lag(agem),
         agediff=agem-agemlag)  %>%
  filter(!is.na(agediff))

# plot distribution of stunting duration prior to 24 months
labs=paste0(sprintf("%0.1f",prop.table(table(rev2$agediff))*100),"%")[1:6]

pdf("U:/Figures/stunting-duration.pdf",width=8,height=4,onefile=TRUE)
ggplot(rev2, aes(x=agediff))+
  geom_histogram(binwidth=1,col="black",fill="gray")+
  scale_x_continuous(labels=seq(0,24),breaks=seq(0,24)) +
  annotate("text",x=seq(0,5),y=as.numeric(table(rev2$agediff)[1:6]),
           label=labs, vjust=-0.4,size=2.25)+
  xlab("Duration of stunting episodes in months")+
  ylab("Number of child-episodes")+
  ggtitle("Distribution of the duration of stunting episodes prior to 24 months")
dev.off()
