#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth at
# 3, 6, 9, 12, 18, and 24 mo of age

# Cohort specific estimates & 
# Pooled estimates using random effects

# What proportion of children stunted at birth were no longer stunted at 3 months? 
# What proportion of children stunted at 3 months were no longer stunted at 6 months?
# What proportion of children stunted at 6 months were no longer stunted at 9 months? 
# What proportion of children stunted at 9 months were no longer stunted at 12 months? 
# What proportion of children stunted at 12 months were no longer stunted at 18 months? 
# What proportion of children stunted at 18 months were no longer stunted at 24 months? 
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

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

# make subjid character
d <- d %>% ungroup() %>% mutate(subjid=as.character(subjid))

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

d$agem=round(d$agem)

# 
# # create indicators for stunting
# st <- d %>%
#   filter(!is.na(agecat)) %>%
#   mutate(measid=seq_along(subjid))  %>%
#   group_by(studyid,country,subjid) %>%
#   mutate(stunted=ifelse(haz< -2,1,0),
#          lagstunted=lag(stunted),
#          leadstunted=lead(stunted))  %>%
#   # unique stunting episode
#   mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
#                          stunted==1 & measid==1,1,0))
#   # if last obs, then lead stunted is na, so can't be onset
#   # because we don't know the subsequent measurement
# 
# 
# #------------------------------------------
# # create indicator recovery
# #------------------------------------------
# rev.ind <- st %>%
#   group_by(studyid,country,subjid) %>%
#   # indicator for whether haz in time t> haz in time t-1
#   mutate(hazinc=ifelse(haz>lag(haz),1,0)) %>%
#   # create recovery indicator
#   # NA means that it was the age cat of first measurement 
#   mutate(recrow=ifelse(stunted==0 & lagstunted==1 & lag(lagstunted)==1,
#                        1,0)) %>%
#   # cumulative sum of recovery indicator
#   group_by(studyid,country,subjid,agecat) %>%
#   mutate(notst=ifelse(stunted==0,1,0)) %>%
#   mutate(recsum=cumsum(notst)) %>%
#   # assess whether recsum is for contiguous rec indicators
#   mutate(contig = ifelse(lag(recrow)==1,1,0)) %>%
#   # count as recovery if at least two meas have haz>=-2
#   mutate(rec=ifelse(recsum>=2 & contig==1 &
#       notst==1,1,0))
# 
# # subset to stunted children in each age group
# stunt.03 <- rev.ind %>%
#   filter(agecat=="Birth"|agecat=="3 months") %>%
#   group_by(studyid,country,subjid) %>%
#   summarise(rec=max(rec),stunted=max(sepisode))

# # test code
# rev.ind[rev.ind$studyid=="ki0047075b-MAL-ED", c("agecat","agem",
#                                                      "haz","stunted","lagstunted","sepisode","hazinc","recrow",
#                       "notst","recsum","contig","rec")][1:10,]
# 
# rev.ind[rev.ind$studyid=="ki0047075b-MAL-ED" &
#         rev.ind$subjid=="9.88131291682493e-324", c("agecat","agem",
#     "haz","stunted","lagstunted","sepisode","hazinc","recrow",
#      "notst","recsum","contig","rec")][11:20,]
# rev.agecat[rev.agecat$studyid=="ki0047075b-MAL-ED" &
#              rev.agecat$subjid=="9.88131291682493e-324",]

# stunted by age x, recovered by age y
# children with recovery=NA didn't have 2 measurements
# in the age window, so recovery could not be assessed
rec.03=rec.age(data=d,s.agem=1,r.agem=3)
rec.36=rec.age(data=d,s.agem=3,r.agem=6)
rec.69=rec.age(data=d,s.agem=6,r.agem=9)
rec.912=rec.age(data=d,s.agem=9,r.agem=12)

# prepare data for pooling 
rec.03.sum=rec.03 %>%
  group_by(studyid,country) %>%
  summarise(mn=mean(recovered,na.rm=TRUE),
            n=sum(recovered,na.rm=TRUE),
            N=sum(!is.na(recovered))) %>%
  mutate(agecat="0-3 months")

rec.36.sum=rec.36 %>%
  group_by(studyid,country) %>%
  summarise(mn=mean(recovered,na.rm=TRUE),
            n=sum(recovered,na.rm=TRUE),
            N=sum(!is.na(recovered)))%>%
  mutate(agecat="3-6 months")

rec.69.sum=rec.69 %>%
  group_by(studyid,country) %>%
  summarise(mn=mean(recovered,na.rm=TRUE),
            n=sum(recovered,na.rm=TRUE),
            N=sum(!is.na(recovered)))%>%
  mutate(agecat="6-9 months")

rec.912.sum=rec.912 %>%
  group_by(studyid,country) %>%
  summarise(mn=mean(recovered,na.rm=TRUE),
            n=sum(recovered,na.rm=TRUE),
            N=sum(!is.na(recovered)))%>%
  mutate(agecat="9-12 months")

rev.data=bind_rows(rec.03.sum,rec.36.sum,rec.69.sum,
                   rec.912.sum) 
rev.data = rev.data %>%mutate(agecat=as.factor(agecat))

# cohort specific results
rec.cohort=lapply(list("0-3 months","3-6 months","6-9 months",
                       "9-12 months"),function(x) 
  fit.escalc(data=rev.data,ni="N", xi="n",age=x,meas="PR"))
rec.cohort=as.data.frame(do.call(rbind, rec.cohort))
rec.cohort=cohort.format(rec.cohort,y=rec.cohort$yi,
    lab=  c("0-3 months","3-6 months","6-9 months",
            "9-12 months"))


# estimate random effects, format results
rev.res=lapply(list("0-3 months","3-6 months","6-9 months",
                    "9-12 months"),function(x) 
      fit.rma(rev.data,ni="N", xi="n",age=x,measure="PR",
              nlab="children"))
rev.res=as.data.frame(do.call(rbind, rev.res))
rev.res[,4]=as.numeric(rev.res[,4])
rev.res = rev.res %>%
  mutate(est=est*100,lb=lb*100,ub=ub*100)
rev.res$agecat=factor(rev.res$agecat,levels=c("0-3 months","3-6 months","6-9 months",
                                              "9-12 months"))

rev.res$ptest.f=sprintf("%0.1f",rev.res$est)

rev.res

lab.af=rec.cohort[rec.cohort$region=="Africa",] %>% 
  group_by(cohort) %>% summarise(N=sum(N))
lab.af.f=paste0("N=",lab.af$N)

# plot cohort % recovered by age
pdf("U:/Figures/stunting-rec-africa.pdf",width=8,height=5,onefile=TRUE)
ggplot(rec.cohort[rec.cohort$region=="Africa",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-17,100))+
  xlab("Age category")+
  ylab("Percentage (95% CI)")+
  ggtitle("Cohort-specific percentage of children who were stunted and recovered\nwithin age intervals - Africa")+
  annotate("text", x=4,y=90,label=lab.af.f,size=4)
dev.off()

lab.lae=rec.cohort[rec.cohort$region=="Latin America"|  rec.cohort$region=="Europe",] %>% 
  group_by(cohort) %>% summarise(N=sum(N))
lab.lae.f=paste0("N=",lab.lae$N)

pdf("U:/Figures/stunting-rec-latamer-eur.pdf",width=10,height=5,onefile=TRUE)
ggplot(rec.cohort[rec.cohort$region=="Latin America"|
                    rec.cohort$region=="Europe",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-17,100))+
  xlab("Age category")+
  ylab("Percentage (95% CI)")+
  ggtitle("Cohort-specific percentage of children who were stunted and recovered within age intervals - Latin America")+
  annotate("text",x=4,y=90,label=lab.lae.f,size=4)
dev.off()

lab.asia=rec.cohort[rec.cohort$region=="Asia",] %>% 
  group_by(cohort) %>% summarise(N=sum(N))
lab.asia.f=paste0("N=",lab.asia$N)

pdf("U:/Figures/stunting-rec-asia.pdf",width=15,height=7,onefile=TRUE)
ggplot(rec.cohort[rec.cohort$region=="Asia",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-17,100))+
  xlab("Age category")+
  ylab("Percentage (95% CI)")+
  ggtitle("Cohort-specific percentage of children who were stunted and recovered within age intervals - Asia")+
  annotate("text", x=4,y=90,label=lab.asia.f,size=4)
dev.off()


# plot pooled % recovered by age
pdf("U:/Figures/stunting-rec-pool.pdf",width=10,height=4,onefile=TRUE)
ggplot(rev.res,aes(y=est,x=agecat))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,20))+
  annotate("text",x=rev.res$agecat,y=1.2,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat,y=0.1,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat,
           y=rev.res$est,hjust=-0.4,size=3)+
  ggtitle("Percentage of children who were stunted and recovered within age intervals")
dev.off()

save(rev, rev.ind, file="U:/Data/Stunting/st_rec_interim.RData")

