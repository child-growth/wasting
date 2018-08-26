#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate cumulative incidence (ever stunted) at
# 3, 6, 12, 18, and 24 mo of age

# Cumulative incidence pooled using random effects
#-----------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Stunting/stunting_data.RData")

# define age windows
d = d %>% 
  mutate(agecat=ifelse(agedays<=3*30.4167,"3 months",
       ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
           ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                       ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months","")))))) %>%
  mutate(agecat=factor(agecat,levels=c("3 months","6 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# identify ever stunted children
  evs = d %>%
    filter(!is.na(agecat)) %>%
    group_by(studyid,country,subjid) %>%
    arrange(studyid,subjid) %>%
    #create variable with minhaz by age category, cumulatively
    mutate(minhaz=ifelse(agecat=="3 months",min(haz[agecat=="3 months"]),
      ifelse(agecat=="6 months",min(haz[agecat=="3 months" | agecat=="6 months"]),
        ifelse(agecat=="12 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"]),
               ifelse(agecat=="18 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
          min(haz)))))) %>%
    # create indicator for whether the child was ever stunted
    # by age category
    group_by(studyid,country,agecat,subjid) %>%
    summarise(minhaz=min(minhaz)) %>%
    mutate(ever_stunted=ifelse(minhaz< -2,1,0))
    
# count incident cases per study by age
# exclude time points if number of measurements per age
# in a study is <50  
cuminc.data= evs%>%
  group_by(studyid,country,agecat) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(ever_stunted),
    N=sum(length(ever_stunted))) %>%
  filter(N>=50)
  
cuminc.data

# cohort specific results
ci.cohort=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x) 
  fit.escalc(data=cuminc.data,ni="N", xi="ncases",age=x,meas="PR"))
ci.cohort=as.data.frame(do.call(rbind, ci.cohort))
ci.cohort=cohort.format(ci.cohort,y=ci.cohort$yi,
   lab=  c("3 months","6 months","12 months","18 months","24 months"))
  
# estimate random effects, format results
ci.res=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
    fit.rma(data=cuminc.data,ni="N", xi="ncases",age=x,measure="PR",nlab=" measurements"))
ci.res=as.data.frame(do.call(rbind, ci.res))
ci.res[,4]=as.numeric(ci.res[,4])
ci.res = ci.res %>%
  mutate(est=est*100, lb=lb*100, ub=ub*100)
ci.res$agecat.f=as.factor(ifelse(ci.res$agecat=="3 months","0-3 months",
                          ifelse(ci.res$agecat=="6 months","4-6 months",
                                 ifelse(ci.res$agecat=="12 months","7-12 months",
                                        ifelse(ci.res$agecat=="18 months","13-18 months","19-24 months")))))
ci.res$agecat.f=factor(ci.res$agecat.f,levels=c("0-3 months","4-6 months",
      "7-12 months","13-18 months","19-24 months"))
ci.res$ptest.f=sprintf("%0.0f",ci.res$est)

ci.res


# plot cohort incidence

pdf("U:/Figures/stunting-cuminc-africa.pdf",width=11,height=5,onefile=TRUE)
ggplot(ci.cohort[ci.cohort$region=="Africa",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  ggtitle("Cohort-specific cumulative incidence of stunting - Africa")
dev.off()

pdf("U:/Figures/stunting-cuminc-latamer-eur.pdf",width=11,height=5,onefile=TRUE)
ggplot(ci.cohort[ci.cohort$region=="Latin America"|
                   ci.cohort$region=="Europe",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  ggtitle("Cohort-specific cumulative incidence of stunting - Latin America & Europe")
dev.off()

pdf("U:/Figures/stunting-cuminc-asia.pdf",width=17,height=7,onefile=TRUE)
ggplot(ci.cohort[ci.cohort$region=="Asia",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  ggtitle("Cohort-specific cumulative incidence of stunting - Asia")
dev.off()

# plot pooled cumulative incidence
pdf("U:/Figures/stunting-cuminc-pool.pdf",width=9,height=3.5,onefile=TRUE)
ggplot(ci.res,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0,80))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=5,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of stunting")
dev.off()

#--------------------------------------
# Sensitivity analysis excluding birth from CI
# only among birth cohort studies
#--------------------------------------
# birth cohorts
bc= d %>% group_by(studyid,country) %>% 
  summarise(minage=min(agedays)) %>%
  # filter(minage==1) %>%
  mutate(birthcohort=ifelse(minage==1,1,0)) %>%
  select(-minage)
  
# identify ever stunted children
# excluding birth in birth cohort studies
evs.sens.nobirth = left_join(d, bc, by=c("studyid","country")) %>%
  filter(birthcohort==1 & !is.na(agecat) & agedays>1) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(minhaz=ifelse(agecat=="3 months",min(haz[agecat=="3 months"]),
                       ifelse(agecat=="6 months",min(haz[agecat=="3 months" | agecat=="6 months"]),
                              ifelse(agecat=="12 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"]),
                                     ifelse(agecat=="18 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
                                            min(haz)))))) %>%
  # create indicator for whether the child was ever stunted
  # by age category
  group_by(studyid,country,agecat,subjid) %>%
  summarise(minhaz=min(minhaz)) %>%
  mutate(ever_stunted=ifelse(minhaz< -2,1,0))

# count incident cases per study by age
# exclude time points if number of measurements per age
# in a study is <50  
cuminc.data.nobirth= evs.sens.nobirth%>%
  group_by(studyid,country,agecat) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(ever_stunted),
    N=sum(length(ever_stunted))) %>%
  filter(N>=50)

cuminc.data.nobirth

# estimate random effects, format results
ci.res.nobirth=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
  fit.rma(data=cuminc.data.nobirth,ni="N", xi="ncases",age=x,measure="PR",nlab=" measurements"))
ci.res.nobirth=as.data.frame(do.call(rbind, ci.res.nobirth))
ci.res.nobirth[,4]=as.numeric(ci.res.nobirth[,4])
ci.res.nobirth = ci.res.nobirth %>%
  mutate(est=est*100, lb=lb*100, ub=ub*100)
ci.res.nobirth$agecat.f=as.factor(ifelse(ci.res.nobirth$agecat=="3 months","0-3 months",
                                         ifelse(ci.res.nobirth$agecat=="6 months","4-6 months",
                                                ifelse(ci.res.nobirth$agecat=="12 months","7-12 months",
                                                       ifelse(ci.res.nobirth$agecat=="18 months","13-18 months","19-24 months")))))
ci.res.nobirth$agecat.f=factor(ci.res.nobirth$agecat.f,levels=c("0-3 months","4-6 months",
                                                                "7-12 months","13-18 months","19-24 months"))
ci.res.nobirth$ptest.f=sprintf("%0.0f",ci.res.nobirth$est)

ci.res.nobirth


# plot pooled cumulative incidence
pdf("U:/Figures/stunting-cuminc-pool-bc-nobirth.pdf",width=9,height=3.5,onefile=TRUE)
ggplot(ci.res.nobirth,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0,80))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res.nobirth$agecat.f,y=5,label=ci.res.nobirth$nmeas.f,size=3)+
  annotate("text",x=ci.res.nobirth$agecat.f,y=1,label=ci.res.nobirth$nstudy.f,size=3)+
  annotate("text",label=ci.res.nobirth$ptest.f,x=ci.res.nobirth$agecat.f,
           y=ci.res.nobirth$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of stunting - birth cohorts only - CI excludes birth")
dev.off()


# identify ever stunted children
# including birth in birth cohort studies
evs.sens.birth = left_join(d, bc, by=c("studyid","country")) %>%
  filter(birthcohort==1 & !is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(minhaz=ifelse(agecat=="3 months",min(haz[agecat=="3 months"]),
                       ifelse(agecat=="6 months",min(haz[agecat=="3 months" | agecat=="6 months"]),
                              ifelse(agecat=="12 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"]),
                                     ifelse(agecat=="18 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
                                            min(haz)))))) %>%
  # create indicator for whether the child was ever stunted
  # by age category
  group_by(studyid,country,agecat,subjid) %>%
  summarise(minhaz=min(minhaz)) %>%
  mutate(ever_stunted=ifelse(minhaz< -2,1,0))

# count incident cases per study by age
# exclude time points if number of measurements per age
# in a study is <50  
cuminc.data.birth= evs.sens.birth%>%
  group_by(studyid,country,agecat) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(ever_stunted),
    N=sum(length(ever_stunted))) %>%
  filter(N>=50)

cuminc.data.birth

# estimate random effects, format results
ci.res.birth=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
  fit.rma(data=cuminc.data.birth,ni="N", xi="ncases",age=x,measure="PR",nlab=" measurements"))
ci.res.birth=as.data.frame(do.call(rbind, ci.res.birth))
ci.res.birth[,4]=as.numeric(ci.res.birth[,4])
ci.res.birth = ci.res.birth %>%
  mutate(est=est*100, lb=lb*100, ub=ub*100)
ci.res.birth$agecat.f=as.factor(ifelse(ci.res.birth$agecat=="3 months","0-3 months",
                                       ifelse(ci.res.birth$agecat=="6 months","4-6 months",
                                              ifelse(ci.res.birth$agecat=="12 months","7-12 months",
                                                     ifelse(ci.res.birth$agecat=="18 months","13-18 months","19-24 months")))))
ci.res.birth$agecat.f=factor(ci.res.birth$agecat.f,levels=c("0-3 months","4-6 months",
                                                            "7-12 months","13-18 months","19-24 months"))
ci.res.birth$ptest.f=sprintf("%0.0f",ci.res.birth$est)

ci.res.birth

# plot pooled cumulative incidence
pdf("U:/Figures/stunting-cuminc-pool-bc-birth.pdf",width=9,height=3.5,onefile=TRUE)
ggplot(ci.res.birth,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0,80))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res.birth$agecat.f,y=5,label=ci.res.birth$nmeas.f,size=3)+
  annotate("text",x=ci.res.birth$agecat.f,y=1,label=ci.res.birth$nstudy.f,size=3)+
  annotate("text",label=ci.res.birth$ptest.f,x=ci.res.birth$agecat.f,
           y=ci.res.birth$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of stunting - birth cohorts only - CI includes birth")
dev.off()

# export data 
cuminc=evs %>% select(studyid,country,subjid,agecat,ever_stunted) 

save(cuminc,file="U:/Data/Stunting/st_cuminc.RData")
save(cuminc,file="U:/ucb-superlearner/Stunting rallies/st_cuminc.RData")


