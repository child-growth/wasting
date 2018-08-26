#-----------------------------------
# Wasting analysis
# Objective 1a
# Calculate point prevalence at
# Birth, 3, 6, 12, 18, and 24 mo of age

# Prevalence pooled using random effects
#-----------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Wasting/wasting_data.RData")

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

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# take mean of multiple measurements within age window
dmn <- d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid,agecat) %>%
  summarise(whz=mean(whz)) %>%
  mutate(wasted=ifelse(whz< -2, 1,0),swasted=ifelse(whz< -3, 1,0))

# count measurements per study by age
# exclude time points if number of measurements per age
# in a study is <50
prev.data = dmn %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,agecat) %>%
  summarise(nmeas=sum(!is.na(whz)),
            prev=mean(wasted),
            nxprev=sum(wasted==1)) %>%
  filter(nmeas>=50) 

# cohort specific results
prev.cohort=lapply(list("Birth","3 months","6 months","9 months","12 months","18 months","24 months"),function(x) 
  fit.escalc(data=prev.data,ni="nmeas", xi="nxprev",age=x,meas="PR"))
prev.cohort=as.data.frame(do.call(rbind, prev.cohort))
prev.cohort=cohort.format(prev.cohort,y=prev.cohort$yi,
                          lab=  c("Birth","3m","6m","9m","12m","18m","24m"))

# estimate random effects, format results
prev.res=lapply(list("Birth","3 months","6 months","9 months","12 months","18 months","24 months"),function(x) 
  fit.rma(data=prev.data,ni="nmeas", xi="nxprev",age=x,measure="PR",nlab="children"))
prev.res=as.data.frame(do.call(rbind, prev.res))
prev.res[,4]=as.numeric(prev.res[,4])
prev.res = prev.res %>%
  mutate(est=est*100,lb=lb*100,ub=ub*100)
prev.res$agecat=factor(prev.res$agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months"))
prev.res$ptest.f=sprintf("%0.0f",prev.res$est)

# plot cohort prevalence
pdf("U:/Figures/wasting-ptprev-africa.pdf",width=11,height=5,onefile=TRUE)
ggplot(prev.cohort[prev.cohort$region=="Africa",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  # scale_y_continuous(limits=c(0,96))+
  xlab("Age in months")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific point prevalence of wasting - Africa")
dev.off()

pdf("U:/Figures/wasting-ptprev-latamer-eur.pdf",width=8,height=5,onefile=TRUE)
ggplot(prev.cohort[prev.cohort$region=="Latin America"|
                     prev.cohort$region=="Europe",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  # scale_y_continuous(limits=c(0,96))+
  xlab("Age in months")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific point prevalence of wasting - Latin America & Europe")
dev.off()

pdf("U:/Figures/wasting-ptprev-asia.pdf",width=15,height=7,onefile=TRUE)
ggplot(prev.cohort[prev.cohort$region=="Asia",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  # scale_y_continuous(limits=c(0,96))+
  xlab("Age in months")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific point prevalence of wasting - Asia")
dev.off()

# plot pooled prevalence
pdf("U:/Figures/wasting-ptprev-pool.pdf",width=10,height=4,onefile=TRUE)
ggplot(prev.res,aes(y=est,x=agecat))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(-4,20))+
  annotate("text",x=prev.res$agecat,y=0,label=prev.res$nmeas.f,size=3)+
  annotate("text",x=prev.res$agecat,y=-3,label=prev.res$nstudy.f,size=3)+
  annotate("text",label=prev.res$ptest.f,x=prev.res$agecat,
           y=prev.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled point prevalence of wasting")
dev.off()


# export
prev = dmn %>% 
  select(studyid,subjid,country,agecat,
         wasted, swasted)


save(prev,file="U:/Data/Wasting/wast_prev.RData")


