


rm(list=ls())
library(tidyverse)

load("U:/data/WastIncDatasets/cmc_inc.Rdata")
load("U:/data/WastIncDatasets/irc_inc.Rdata")
load("U:/data/WastIncDatasets/gmsn_inc.Rdata")


cmc_inc <- cmc_inc %>% filter(AGEDAYS < 24 *30.25)
d <- cmc_inc

d2 <- irc_inc %>% filter(AGEDAYS < 24 *30.25)
d3 <- gmsn_inc %>% filter(AGEDAYS < 24 *30.25)



heatmap<-function(d, whzArrange=F, pers_cutoff=0.25, title=""){
  theme_set(theme_bw())
  
  d$persCutoff <- pers_cutoff
  
  #generate whz categories
  d$wastcat <- 0
  d$wastcat[d$WHZ<(-2)] <- -1
  d$wastcat[d$WHZ<(-3)] <- -2
  
  #Sum an ad-hoc total wasting score to rank children by  
  d <- d %>% group_by(SUBJID) %>% mutate(wastscore=sum(wastcat, na.rm=T),
                                         wastcount=sum(WHZ < -2),
                                         meanWHZ = mean(WHZ),
                                         persWast = as.numeric(mean(WHZ < -2) > persCutoff),
                                         persWast75 = as.numeric(mean(WHZ < -2) > .75),
                                         persWast50 = as.numeric(mean(WHZ < -2) > .50),
                                         persWast25 = as.numeric(mean(WHZ < -2) > .25),
                                         persWast10 = as.numeric(mean(WHZ < -2) > .10),
                                         label_x=25) %>% ungroup()
  table(d$wastscore)
  
  #Add level for missingness
  d$wastcat[is.na(d$WHZ)] <- 1
  d$wastcat <- as.factor(d$wastcat)
  d$SUBJID <- as.factor(d$SUBJID)
  table(d$wastcat)
  
  #make ordered childnum by amount of wasting
  if(whzArrange==T){
    d <- d %>%
      arrange(meanWHZ) %>%  
      mutate(SUBJID = factor(SUBJID, unique(SUBJID))) 
  }else{
    d <- d %>%
      arrange(desc(wastcount), meanWHZ) %>%  
      mutate(SUBJID = factor(SUBJID, unique(SUBJID)))}
  
  d$childnum <- as.numeric(d$SUBJID)
  head(as.data.frame(d))
  
  #Create a child age in months variable
  d$agemonths<-floor(d$AGEDAYS/30)
  
  
  if(sum(d$wastcat== -2) > 0){
    levels(d$wastcat)<-c("Severely wasted", "Wasted", "Not wasted")
    cbPalette <- c("Severely wasted"="#56B4E9", 
                   "Wasted"="#E69F00", "Not wasted"="#c1c1c1", 
                   "0"="white","1"="red")
  }else{
    levels(d$wastcat)<-c( "Wasted", "Not wasted")
    cbPalette <- c( "#E69F00", "#c1c1c1")   
  }
  
  labs <- data.frame(x=c(26:29),y=c(-5,-20,-5, -20), label=c(">75%",">50%",">25%",">10%"))
  
  p <- ggplot(d,aes(x=agemonths,y=childnum)) + 
    geom_tile(aes(fill=wastcat)) + scale_fill_manual(values=cbPalette) +
    xlab("Child Age (Months)") + ylab("Child number") +
    ggtitle(title) +
    theme(legend.title=element_blank()) +#+ scale_y_discrete(aes(label=wastcount))
    #geom_tile(aes( x=26,fill=factor(persWast))) +
    geom_tile(aes( x=26,fill=factor(persWast75))) +
    geom_tile(aes( x=27,fill=factor(persWast50))) +
    geom_tile(aes( x=28,fill=factor(persWast25))) +
    geom_tile(aes( x=29,fill=factor(persWast10))) + geom_text(data=labs,aes(x=x,y=y,label=label))
  
  return(p)
}


# p1 <- heatmap(d)
# p1
# 
# 
# p2 <- heatmap(d, whzArrange=T)
# p2




# heatmap(d) + facet_wrap(~persWast, scales = "free_y")



p1 <- heatmap(d, pers_cutoff = 0.5, title="CMC: Red marks children with >75, 50, 25, and 10% wasted measurements, respectively")
p1


p2 <- heatmap(d, pers_cutoff = 0.5, whzArrange=T, title="CMC: Y axis sorted by mean WHZ across child measurements")
p2   


p3 <- heatmap(d2, pers_cutoff = 0.5, title="IRC: Red marks children with >75, 50, 25, and 10% wasted measurements, respectively")
p3


p4 <- heatmap(d2, pers_cutoff = 0.5, whzArrange=T, title="IRC: Y axis sorted by mean WHZ across child measurements")
p4   

p5 <- heatmap(d3, pers_cutoff = 0.5, title="GMSN: Red marks children with >75, 50, 25, and 10% wasted measurements, respectively")
p5


p6 <- heatmap(d3, pers_cutoff = 0.5, whzArrange=T, title="GMSN: Y axis sorted by mean WHZ across child measurements")
p6   




df <- d %>% group_by(SUBJID) %>% 
  mutate(persWast = as.numeric(mean(WHZ < -2) > 0.25),
         percWast = mean(WHZ < -2)*100) %>% ungroup() %>%
  filter(persWast==1) %>% arrange(desc(percWast))



ind_traj_plot <- function(d){
  
  #set up colors
  colors <-  c("green", "orange", "red", "grey80", "grey40")
  names(colors) = c("Not wasted", "Wasted", "Severe wasted", "Born wasted", "Born severe wasted")
  
  tj <- d %>% arrange(AGEDAYS) %>% subset(., select=c(AGEDAYS, WHZ, wasting_episode, sevwasting_episode, wast_inc, sevwast_inc, wast_rec, sevwast_rec, period_length, wasting_duration,percWast))
  
  wastinc<-paste0("Wasting episodes: ",sum(tj$wast_inc, na.rm=T))
  sevwastinc<-paste0("Severe wasting episodes: ",sum(tj$sevwast_inc, na.rm=T))
  wastrec<-paste0("Wasting recoveries: ",sum(tj$wast_rec, na.rm=T))
  sevwastrec<-paste0("Severe wasting recoveries: ",sum(tj$sevwast_rec, na.rm=T))
  
  PercWast<-paste0("Longitudinal prevalence of wasting: ",round(tj$percWast[1],1))
  
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
    geom_text(aes(x=300,y=0.8,label=wastrec))+geom_text(aes(x=300,y=0.6,label=sevwastrec))+geom_text(aes(x=300,y=0.2,label=PercWast))+ 
    geom_text(aes(x=300,y=0.4,label=dur))+ 
    ylab("WHZ") #+ aes(alpha=alpha, group=factor(SUBJID)) + guides(alpha=FALSE)
  
  return(p)  
}




#------------------------------------------
# Load incidence data and create a plot per individual
#------------------------------------------



setwd("U:/results/diagnostic figures")


pdf("Persistant_wasting_definition.pdf",width=10,height=8.5)    
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
for(i in 1:length(unique(df$SUBJID))){   
  p <- ind_traj_plot(df[df$SUBJID==unique(df$SUBJID)[i],])
  print(p)
}
dev.off()

