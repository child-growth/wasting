


rm(list=ls())
library(tidyverse)

#------------------------------------------
# Individual trajectory plot function
#------------------------------------------
ind_traj_plot <- function(d){
  
  #set up colors
  colors <-  c("green", "orange", "red", "grey80", "grey40")
  names(colors) = c("Not wasted", "Wasted", "Severe wasted", "Born wasted", "Born severe wasted")
  
  tj <- d %>% arrange(AGEDAYS) %>% subset(., select=c(AGEDAYS, WHZ, wasting_episode, sevwasting_episode, wast_inc, sevwast_inc, wast_rec, sevwast_rec, period_length, wasting_duration))
  
  wastinc<-paste0("Wasting episodes: ",sum(tj$wast_inc, na.rm=T))
  sevwastinc<-paste0("Severe wasting episodes: ",sum(tj$sevwast_inc, na.rm=T))
  wastrec<-paste0("Wasting recoveries: ",sum(tj$wast_rec, na.rm=T))
  sevwastrec<-paste0("Severe wasting recoveries: ",sum(tj$sevwast_rec, na.rm=T))
  
  episode_durations <- NA
  episode_durations <- tj$wasting_duration[!is.na(tj$wasting_duration)]
  dur<-paste0("First episode duration: ",episode_durations[1])
  
  
  tj$`Wasting status`<-NA
  tj$`Wasting status`[tj$wasting_episode=="Not Wasted"]<-"Not wasted"
  tj$`Wasting status`[tj$wasting_episode=="Wasted"]<-"Wasted"
  tj$`Wasting status`[tj$wasting_episode=="Born Wasted"]<-"Born wasted"
  tj$`Wasting status`[tj$sevwasting_episode=="Born Severe Wasted"]<-"Born severe wasted"
  tj$`Wasting status`[tj$sevwasting_episode=="Severe Wasted"]<-"Severe wasted"
  tj$`Wasting status` <- factor(tj$`Wasting status`)                      
  table(tj$`Wasting status`)
  #tj <- tj %>% mutate(`Wasting status` = na.locf(`Wasting status`, fromLast=F), `Wasting status`=factor(`Wasting status`, levels=c("Not wasted","Wasted","Severe wasted")))
  
  tj$wasting_episode <- factor(tj$wasting_episode)
  tj$w1 <- tj$AGEDAYS-(tj$AGEDAYS-lag(tj$AGEDAYS))/2
  tj$w2 <- tj$AGEDAYS+(lead(tj$AGEDAYS)-tj$AGEDAYS)/2
  tj$w1[is.na(tj$w1)] <- tj$AGEDAYS[is.na(tj$w1)]
  tj$w2[is.na(tj$w2)] <- tj$AGEDAYS[is.na(tj$w2)]
  p <- ggplot(tj, aes(x=AGEDAYS, y=WHZ)) + 
    geom_line(lwd=2) +
    scale_fill_manual(values = colors) +   
    geom_rect(aes(xmin = w1, xmax = w2 , ymin = -4, ymax =1.5, fill = `Wasting status`, color = NULL), alpha=0.3) +
    guides(colour=FALSE) + xlab("Child Age (Days)") + theme(strip.background = element_blank()) +
    geom_hline(yintercept= -2, color="grey20", linetype=2) +
    geom_hline(yintercept= -3, color="grey20", linetype=2) +
    geom_text(aes(x=AGEDAYS, label=period_length), color="white") +
    #geom_text_repel(aes(x=AGEDAYS, label=period_length), color="white") +  
    geom_text(aes(x=300,y=1.2,label=wastinc))+geom_text(aes(x=300,y=1,label=sevwastinc))+ 
    geom_text(aes(x=300,y=0.8,label=wastrec))+geom_text(aes(x=300,y=0.6,label=sevwastrec))+ 
    geom_text(aes(x=300,y=0.4,label=dur))+ 
    ylab("WHZ") #+ aes(alpha=alpha, group=factor(SUBJID)) + guides(alpha=FALSE)
  
  p
  
  return(p)  
}


#------------------------------------------
# Load incidence data and create a plot per individual
#------------------------------------------

setwd("U:/data/WastIncDatasets")


load("gmsn_inc_NoBirthInc.Rdata")
d_inc_noBornWast<-gmsn_inc
load("gmsn_inc.Rdata")
d_inc<-gmsn_inc

i<-27


setwd("U:/results/diagnostic figures")


pdf("GMSN_wast_inc_diagnostic_plots.pdf",width=10,height=8.5)    
for(i in 1:length(unique(d_inc$SUBJID))){   
  p <- ind_traj_plot(d_inc[d_inc$SUBJID==unique(d_inc$SUBJID)[i],])
  print(p)
  # p_noBornWast <- ind_traj_plot(d_inc_noBornWast[d_inc_noBornWast$SUBJID==unique(d_inc_noBornWast$SUBJID)[i],])
  # print(p_noBornWast)
}
dev.off()





setwd("U:/data/WastIncDatasets")

load("mled_inc.Rdata")
d_inc<-mled_inc_india


setwd("U:/results/diagnostic figures")

i<-27
d<-d_inc[d_inc$SUBJID==unique(d_inc$SUBJID)[i],]
d<-d %>% arrange(AGEDAYS)

pdf("MLED_India_wast_inc_diagnostic_plots.pdf",width=10,height=8.5)    
for(i in 1:length(unique(d_inc$SUBJID))){   
  p <- ind_traj_plot(d_inc[d_inc$SUBJID==unique(d_inc$SUBJID)[i],])
  print(p)
}
dev.off()

max(d_inc$AGEDAYS)


mean(d_inc$wasting_duration, na.rm=T)

mean(d_inc$wasting_duration[d_inc$wast_inc==1], na.rm=T)


d<-d_inc %>% subset(., select=c(SUBJID, AGEDAYS, wast, wast_inc, sevwast_inc, wast_rec, wasting_duration, period_length)) %>%
            arrange(SUBJID, AGEDAYS)



#Calculate average episode lengths
d <- d %>% group_by(SUBJID) %>%
  mutate(state_run = cumsum( wast_inc+sevwast_inc+wast_rec) + 1,
         wast_run = ifelse(wast==1, state_run, 0)) %>%
  ungroup() %>%  group_by(SUBJID, state_run) %>% 
  mutate(state_dur = sum(period_length),
         wast_dur = ifelse(wast==1, state_dur, 0)) %>%
  as.data.frame()


episode_duration <- d %>% 
  filter(wast_inc==1) %>% #drop non-wasting periods
  group_by(SUBJID, state_run ) %>% 
  slice(1) %>% ungroup() %>% 
  subset(., select=c(SUBJID,wast_dur)) %>% 
  as.data.frame()


# #Calculate mean, max, and total duration of wasting per child
duration <- episode_duration %>%
  group_by(SUBJID) %>%
  summarize(
    total_duration= sum(wast_dur, na.rm=T)
  )
#Average episode length
average_duration=mean(episode_duration$wast_dur)
#average total time wasted for each child
total_duration=mean(duration$total_duration)

average_duration
total_duration

mean(d$wasting_duration[d$wast_inc==1], na.rm=T)