#-----------------------------------
# Stunting analysis
# Objective 1a
# Mean age of stunting onset
# Calculate in monthly cohorts only
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

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# ---------------------------------------
# identify incident cases
# ---------------------------------------
inc = d %>%
  # subset to children <=24 months
  filter(agedays<=360*2) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) %>%

  # create indicator for whether haz at t < haz at t-1
  mutate(hazlag=lag(haz)) %>%
  mutate(newcase=ifelse(measid==1,ifelse(haz< -2, 1,0),
                        ifelse(hazlag>= -2 & haz< -2,1,0))) %>%
  mutate(newcaselag=lag(newcase))%>%
  mutate(newcaselag=ifelse(measid==1,0,newcaselag))%>%
  mutate(cnewcaselag=cumsum(newcaselag)) %>%
  mutate(inccase=ifelse(cnewcaselag>=1,0,newcase)) %>%
  filter(inccase==1) %>%
  select(studyid,country,subjid,agedays) %>%
  mutate(agem=agedays/30.4167)


# cohort specific mean
age.onset.study = inc %>%
  # make region
  mutate(region = ifelse(country=="BANGLADESH" | country=="INDIA"|
                  country=="NEPAL" | country=="PAKISTAN"|
                  country=="PHILIPPINES" ,"Asia",
                ifelse(country=="BURKINA FASO"|
                         country=="GUINEA-BISSAU"|
                         country=="MALAWI"|
                         country=="SOUTH AFRICA"|
                         country=="TANZANIA, UNITED REPUBLIC OF"|
                         country=="ZIMBABWE"|
                         country=="GAMBIA","Africa",
                       ifelse(country=="BELARUS","Europe",
                              "Latin America")))) %>%
  # concatenate country and study
  mutate(study_country=paste0(studyid,"-",country)) %>%
  group_by(region,study_country) %>%
  summarise(mn=mean(agedays),
            med=median(agedays),
            se=sem(agedays),
            Nmeas=n(),
            Nchild=sum(length(unique(subjid)))) %>%
  mutate(lb=mn-qnorm(0.975)*se,
         ub=mn+qnorm(0.975)*se)  %>%
  mutate(mn_m=mn/30.4167,med_m=med/30.4167)

# sort by mean age
age.onset.study$study_country=factor(age.onset.study$study_country, 
            levels = age.onset.study$study_country[order(age.onset.study$mn)])

# estimate random effects, format results
pool.fit=rma(yi=age.onset.study$mn, 
             sei=age.onset.study$se,
             method="REML")

# results:
c(est=pool.fit$beta, se=pool.fit$se, lb=pool.fit$ci.lb, ub=pool.fit$ci.ub)


res=paste0(sprintf("%0.1f",pool.fit$beta/30.4167)," (95% CI ",
           sprintf("%0.1f",pool.fit$ci.lb/30.4167),",",
           sprintf("%0.1f",pool.fit$ci.ub/30.4167),")")
med = sprintf("%0.1f",median(inc$agedays/30.4167))

pdf("U:/Figures/stunting-age-onset.pdf",width=10,height=4,onefile=TRUE)
ggplot(age.onset.study,aes(x=study_country,y=mn_m))+
  geom_point(aes(size=Nchild,col=region),shape=15)+coord_flip()+
  geom_hline(yintercept=pool.fit$ci.lb/30.4167,linetype="dashed")+
  geom_hline(yintercept=pool.fit$ci.ub/30.4167,linetype="dashed")+
  geom_hline(yintercept=pool.fit$beta/30.4167)+
  scale_y_continuous(breaks=seq(0,12,1),labels=seq(0,12,1))+
  xlab("Study & Country") + ylab("Age in months")+
  annotate("text",x=1, y=9.5, label=paste0("Mean ",res))
dev.off()

pdf("U:/Figures/stunting-age-onset-hist.pdf",width=8,height=4,onefile=TRUE)
ggplot(inc,aes(x=agem))+
  geom_histogram(col="black",fill="gray",binwidth=1)+
  scale_x_continuous(breaks=seq(0,24,3),labels=seq(0,24,3))+
  scale_y_continuous(breaks=seq(0,1000,100),labels=seq(0,1000,100))+
  xlab("Age in months") + ylab("Number of children")+
  ggtitle("Distribution of age in months at first incident of stunting")+
  annotate("text",x=22, y=900, label=paste0("Mean: ",res))+
  annotate("text",x=20.1, y=840, label=paste0("Median: ",med))
dev.off()

inc = inc %>% mutate(cohort=paste0(studyid,"-",country)) %>%
  mutate(cohort=gsub("ki[^-]*-","",cohort))

age.onset.study$mn.f=paste0("Mean: ",sprintf("%0.1f",
      age.onset.study$mn_m))
age.onset.study$med.f=paste0("Median: ",sprintf("%0.1f",
      age.onset.study$med_m))

pdf("U:/Figures/stunting-age-onset-hist-cohort.pdf",width=12,height=8,onefile=TRUE)
ggplot(inc,aes(x=agem))+
  geom_histogram(col="black",fill="gray",binwidth=1)+
  xlab("Age in months") + ylab("Number of children")+
  ggtitle("Distribution of age in months at first incident of stunting")+
  facet_wrap(~cohort)+
  annotate("text",y=190,x=19,label=paste(age.onset.study$mn.f))+
  annotate("text",y=160,x=18.1,label=paste(age.onset.study$med.f))
dev.off()


