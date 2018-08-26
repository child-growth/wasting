#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate incidence at
# 6 mo and 12, and 24 mo of age

# Plot mean HAZ over age intervals
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
theme_set(theme_bw())

# load random effects function
source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")

load("U:/Data/Stunting/stunting_data.RData")


# define age windows
d = d %>% 
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays<=6*30.4167,"0-6 months",
                              ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"6-12 months",
                                     ifelse(agedays>12*30.4167& agedays<=18*30.4167,"12-18 months",
                                            ifelse(agedays>18*30.4167& agedays<=24*30.4167,"18-24 months","")))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","0-6 months","6-12 months","12-18 months","18-24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# average the haz over each bin within each study
avg.study = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,agecat) %>%
  summarise(mean=mean(haz)) %>%
  mutate(studyct=paste0(studyid,"-",country))

# average the haz over each bin within each child
avg.child = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid,agecat) %>%
  summarise(mean=mean(haz))

pdf("U:/Figures/stunting-meanhaz-age-study.pdf",width=16,height=8,onefile=TRUE)
ggplot(avg.study,aes(x=agecat,y=mean,group=studyct))+
  geom_point(aes(col=studyct), size=3)+
  geom_line(aes(col=studyct))+
  geom_hline(yintercept=-2,linetype="dashed",col="black")+
  theme(legend.position="bottom",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_color_discrete(name="Study,\ncountry")+
  xlab("Age category")+ylab("Mean HAZ")
dev.off()


