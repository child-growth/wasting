#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth

# Cohort specific estimates & 
# Pooled estimates using random effects

# create dataset for risk factor analyses: 
# stunting from birth to 3 months, recover by 24 months

# Not used for descriptive analyses
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

# subset to stunted between birth and 3 months
stunt.24 <- d %>%
  filter(agem<=25) %>%
  # identify last two measurements prior to 24 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  # drop last 2 measurements prior to 24 m
  filter(rank> 2) %>%
  # create stunting indicator
  mutate(measid=seq_along(subjid))  %>%
  mutate(stunted=ifelse(haz< -2,1,0),
         lagstunted=lag(stunted),
         leadstunted=lead(stunted))  %>%
  # unique stunting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0)) %>%
  # identify whether child had stunting episode by 24 months 
  summarise(stunted24=max(sepisode,na.rm=TRUE))

rec.24 <- d %>%
  filter(agem<=25) %>%
  # identify last two measurements prior to 24 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  # keep last two measurements 
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(haz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec24=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

rev <- full_join(stunt.24, rec.24,by=c("studyid","country","subjid")) %>%
  # subset to kids who were stunted
  filter(stunted24==1) %>%
  mutate(srec24=ifelse(stunted24==1 & rec24==1,1,0)) %>%
  mutate(snorec24=ifelse(stunted24==1 & rec24==0,1,0)) 

rev = rev %>% mutate(stunted24s=ifelse(stunted24==1,"Stunted","Not stunted"))
rev = rev %>% mutate(rec24s=ifelse(rec24==1,"Recover","Not recover"))
prop.table(table(rev$stunted24s,rev$rec24s))

# prepare data for pooling 
rev.data <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn.r=mean(srec24,na.rm=TRUE),
            n.r=sum(srec24,na.rm=TRUE),
            N.r=sum(!is.na(srec24)),
            mn.s=mean(snorec24,na.rm=TRUE),
            n.s=sum(snorec24,na.rm=TRUE),
            N.s=sum(!is.na(snorec24))) %>%
  mutate(agecat=as.factor("24 months"))

# estimate random effects, format results
fit.r=rma(ni=rev.data$N.r, xi=rev.data$n.r, 
            method="REML", measure="PR")
rev.res = rev.data %>%
  ungroup() %>%
  summarise(nstudies=length(unique(studyid)),
            nmeas=sum(rev.data$N.r)) %>%
  mutate(agecat="24 months",est=fit.r$beta, se=fit.r$se, lb=fit.r$ci.lb, ub=fit$ci.ub,
         nmeas.f=paste0("N=",format(sum(rev.data$N.r),big.mark=",",scientific=FALSE),
                        " children"),
         nstudy.f=paste0("N=",nstudies," studies"))

fit.s=rma(ni=rev.data$N.s, xi=rev.data$n.s, 
          method="REML", measure="PR")
s.res = rev.data %>%
  ungroup() %>%
  summarise(nstudies=length(unique(studyid)),
            nmeas=sum(rev.data$N.s)) %>%
  mutate(agecat="24 months",est=fit.s$beta, se=fit.s$se, lb=fit.s$ci.lb, ub=fit$ci.ub,
         nmeas.f=paste0("N=",format(sum(rev.data$N.s),big.mark=",",scientific=FALSE),
                        " children"),
         nstudy.f=paste0("N=",nstudies," studies"))

# number of cohorts
nrow(rev.data)

# number of children
sum(rev.data$N.r)


rec.cohort=fit.escalc(data=rev.data,ni="N", xi="n",age="24 months",meas="PR")
rec.cohort=cohort.format(rec.cohort, y=rec.cohort$yi,
                         lab="24 months")

# add the pooled result to the cohort plot
pooled= rev.res %>% select(est,lb,ub) %>%
  rename(y=est, ci.lb=lb,ci.ub=ub) %>%
  mutate(cohort="Pooled") %>%
  select(cohort,y,ci.lb,ci.ub)
cohort=rec.cohort %>%
  select(cohort,y,ci.lb,ci.ub) %>%
  mutate(cohort=as.character(cohort))

plot.df=bind_rows(as.data.frame(pooled),cohort)

# sort by recovery %
plot.df$cohort=factor(plot.df$cohort, 
                      levels = plot.df$cohort[order(plot.df$y)])
plot.df$y[plot.df$cohort=="Pooled"]=plot.df$y[plot.df$cohort=="Pooled"]*100
plot.df$ci.lb[plot.df$cohort=="Pooled"]=plot.df$ci.lb[plot.df$cohort=="Pooled"]*100
plot.df$ci.ub[plot.df$cohort=="Pooled"]=plot.df$ci.ub[plot.df$cohort=="Pooled"]*100
plot.df$pooled=as.factor(ifelse(plot.df$cohort=="Pooled",1,0))

# plot recovery
pdf("U:/Figures/stunting-rec24.pdf",width=8,height=4,onefile=TRUE)
ggplot(plot.df,aes(y=y,x=cohort))+
  geom_point(aes(shape=pooled),size=2)+coord_flip()+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(0,25))+
  scale_shape_manual("",values=c(16,15),guide=FALSE)+
  xlab("Cohort")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted and\nrecovered within 24 months")
dev.off()