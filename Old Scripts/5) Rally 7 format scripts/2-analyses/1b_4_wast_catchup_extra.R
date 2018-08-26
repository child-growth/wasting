#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth 

# extra questions: 

# number of stunting episodes

# distribution of laz among 
# newly stunted and recovered
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

load("U:/Data/Stunting/st_rec_interim.RData")

#------------------------------------------
# number of stunting episodes per child
#------------------------------------------
episode.child <- rev %>%
  group_by(studyid,country,subjid) %>%
  filter(!is.na(sepisode)) %>%
  summarise(nrev=sum(sepisode))

labs=paste0(sprintf("%0.1f",prop.table(table(episode.child$nrev))*100),"%")

pdf("U:/Figures/stunting-n-episodes.pdf",width=8,height=4,onefile=TRUE)
ggplot(episode.child, aes(nrev))+geom_histogram(binwidth=0.5,
                        col="black",fill="gray")+
  scale_x_continuous(labels=seq(0,5),breaks=seq(0,5)) +
  annotate("text",x=seq(0,5),y=as.numeric(table(episode.child$nrev)),
           label=labs, vjust=-0.4)+
  scale_y_continuous(limits=c(0,7000),labels=seq(0,7000,500),
                     breaks=seq(0,7000,500))+
  xlab("Number of stunting episodes per child")+
  ylab("Number of children")
dev.off()


#------------------------------------------
# distribution of stunting among those who 
# have recovered and those who have not 
#------------------------------------------
# plot dist among stunted
pdf("U:/Figures/stunting-rec-onset-st-dist.pdf",width=8,height=4,onefile=TRUE)
ggplot(rev.ind %>% filter(sepisode==1), aes(x=haz))+
  geom_histogram(binwidth=0.05,col="black",fill="gray",
                 position="dodge")+
  xlab("LAZ at stunting onset")+ylab("Number of children")+
  scale_x_continuous(breaks=seq(-6,-2,1),labels=seq(-6,-2,1))
dev.off()

# plot dist among recovered
pdf("U:/Figures/stunting-rec-onset-rec-dist.pdf",width=8,height=4,onefile=TRUE)
ggplot(rev.ind %>% filter(recrow==1), aes(x=haz))+
  geom_histogram(binwidth=0.1,col="black",fill="gray")+
  xlab("LAZ at first measurement in which child is not stunted")+
  ylab("Number of children")+
  scale_x_continuous(breaks=seq(-2,6,1),labels=seq(-2,6,1))
dev.off()
