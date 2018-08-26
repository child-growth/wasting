


#-----------------------------------
# Stunting analysis
# Objective 1a
# Import data, subset to relevant variables
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(data.table)
library(washb)

#--------------------------------------------
# Read in .csv file and save as an .rds file
#--------------------------------------------

# setwd("U:/data/")
#  d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T)
d<-fread("U:/data/Stunting/Full-compiled-data/FINAL.csv", header = T)

#change names to lower case
colnames(d) <- tolower(colnames(d))

#--------------------------------------------
# Subset to  just identifying and haz data
#--------------------------------------------


d <- d %>% subset(., select=c(studyid, subjid, country, clustid, tr, arm, agedays, haz))


#---------------------------------------
#Create an ID variable
#---------------------------------------

d$id <- NA
d$id[d$studyid %in% c("ki1112895-iLiNS-Zinc",
                      "kiGH5241-JiVitA-3",    
                      "kiGH5241-JiVitA-4",
                      "ki1119695-PROBIT",
                      "ki1000304b-SAS-CompFeed")] <-d$clustid[d$studyid %in% c("ki1112895-iLiNS-Zinc",
                                                                               "kiGH5241-JiVitA-3",    
                                                                               "kiGH5241-JiVitA-4",
                                                                               "ki1119695-PROBIT",
                                                                               "ki1000304b-SAS-CompFeed")]
d$id[!(d$studyid %in% c("ki1112895-iLiNS-Zinc",
                        "kiGH5241-JiVitA-3",    
                        "kiGH5241-JiVitA-4",
                        "ki1119695-PROBIT",
                        "ki1000304b-SAS-CompFeed"))] <-d$subjid[!(d$studyid %in% c("ki1112895-iLiNS-Zinc",
                                                                                   "kiGH5241-JiVitA-3",    
                                                                                   "kiGH5241-JiVitA-4",
                                                                                   "ki1119695-PROBIT",
                                                                                   "ki1000304b-SAS-CompFeed"))]
d$id[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"] <-d$clustid[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"]



d$id[is.na(d$id)] <- d$subjid[is.na(d$id)]

#--------------------------------------------
#Check for duplicate agedays
#--------------------------------------------
dup_age <- d %>% group_by(studyid, subjid, agedays) %>%
  summarize(N=n())
mean(dup_age$N)


#--------------------------------------------
# Mark monthly, quarterly, and yearly-measured 
# studies
#--------------------------------------------
#Drop studies Vishak added to data product that don't meet inclusion criteria
d <- d %>% filter(studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP")

#mark measure frequencies
d$measurefreq <- NA

d$measurefreq[d$studyid %in% c(
  "ki0047075b-MAL-ED",   
  "ki1000108-CMC-V-BCS-2002",              
  "ki1000108-IRC",               
  "ki1000109-EE",           
  "ki1000109-ResPak",  
  "ki1017093b-PROVIDE",  
  "ki1066203-TanzaniaChild2",           
  "ki1101329-Keneba",  
  "ki1112895-Guatemala BSC",       
  "ki1113344-GMS-Nepal",             
  "ki1114097-CONTENT"
)] <- "monthly"

d$measurefreq[d$studyid %in% c(
  "ki1112895-iLiNS-Zinc",  
  "kiGH5241-JiVitA-3",          
  "kiGH5241-JiVitA-4", 
  "ki1148112-LCNI-5",          
  "ki1017093-NIH-Birth",
  "ki1017093c-NIH-Crypto",   
  "ki1119695-PROBIT",         
  "ki1000304b-SAS-CompFeed",   
  "ki1000304b-SAS-FoodSuppl",   
  "ki1126311-ZVITAMBO",   
  "ki1114097-CMIN",                 
  "ki1135781-COHORTS"
)] <- "quarterly"

d$measurefreq[d$studyid %in% c(
  "ki1000110-WASH-Bangladesh",       
  "ki1000111-WASH-Kenya",  
  "ki1148112-iLiNS-DOSE",     
  "ki1148112-iLiNS-DYAD-M", 
  "ki1033518-iLiNS-DYAD-G",
  "ki1000125-AgaKhanUniv",           
  "ki1112895-Burkina Faso Zn",    
  "ki1000304-VITAMIN-A",  
  "ki1000304-Vitamin-B12",
  "ki1000107-Serrinha-VitA",   
  "ki1000304-EU",        
  "ki1000304-ZnMort"
)] <- "yearly"



#Mark COHORTS and CMIN cohorts with different measurement frequency than quarterly
d$measurefreq[d$studyid=="ki1114097-CMIN" & d$country=="BANGLADESH"] <- "monthly"
d$measurefreq[d$studyid=="ki1114097-CMIN" & d$country=="PERU"] <- "monthly"
d<- d[!(d$studyid=="ki1135781-COHORTS" & d$country=="BRAZIL"),] #Drop because yearly but not an RCT
d<- d[!(d$studyid=="ki1135781-COHORTS" & d$country=="SOUTH AFRICA"),] #Drop because yearly but not an RCT



#--------------------------------------------
# drop yearly measured intervention trials
#--------------------------------------------
#Keep monthly and quarterly studies
d <- d %>% filter(measurefreq!="yearly")


#--------------------------------------------
# Keep intervention studies
#--------------------------------------------

d <- d %>% filter(!is.na(arm) & arm!="")

#--------------------------------------------
# drop unrealistic HAZ
#--------------------------------------------
nrow(d)
d = filter(d,haz >= -6 & haz <=6)
nrow(d)

#--------------------------------------------
# order data, create measurement id
#--------------------------------------------
d <- d %>% 
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) 

# count number of studies
length(names(table(d$studyid)))


#--------------------------------------------
# Calculate cumulative incidence
#--------------------------------------------


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


head(evs)
head(d)

dfull<-d

d<- left_join(evs, d, by=c('studyid', 'subjid'))


d$tr[d$arm=="Control"] <- "Control"

d$arm2 <- d$arm

d$arm2[d$tr=="Control"] <- d$tr[d$tr=="Control"]

d_c <- d %>% filter(arm2=="Control")
d_tr <- d %>% filter(arm2!="Control")

d_tr$arm2 <- paste0(d_tr$studyid," ", d_tr$arm2)

arms <- unique(d_tr$arm2)

res_df <- NULL
for(i in 1:length(arms)){
  
  tr<-arms[i]
  
  dsub_tr <- d_tr[d_tr$arm2==tr,]
  dsub_c <- d_c[d_c$studyid==unique(dsub_tr$studyid),]
  
  dsub<-rbind(dsub_c,dsub_tr)
  
      res <- washb_tmle(
        Y=dsub$ever_stunted, tr=dsub$arm2, 
                 W = NULL,  pair = NULL, 
                 family = "binomial", id = dsub$id,
                 contrast=c("Control", tr),
                 Q.SL.library = c("SL.mean", "SL.glm"),
                 g.SL.library = c("SL.mean", "SL.glm"), 
                 pval = 0.2, seed = 12345, print=F)
      
      res <- data.frame(arm=tr, psi=res$estimates$RR$psi, ci.lb=res$estimates$RR$CI[1], ci.ub=res$estimates$RR$CI[2], pval= res$estimates$RR$pval)
  
      res_df<-rbind(res_df, res)
  
}



#mark sig effects
res_df$sig <- as.numeric(res_df$pval < 0.05)


  


  #plot
  RRplot<-ggplot(data=res_df) + 
    labs( x = "Treatment arm", y = "Risk ratio") +
    geom_hline(yintercept = 1) +
    scale_y_continuous(breaks=c(0.125,0.25,0.5,0.75,1,1.5,2,4), trans='log10') +
    coord_flip(ylim = c(0.75, 1.5)) +
    geom_pointrange( mapping=aes(x=arm, y=psi, ymin=ci.lb, ymax=ci.ub , colour=sig)) +
    theme(panel.border = element_blank(), 
          strip.background = element_blank(), legend.position = "none") + 
    ggtitle("Intervention effects on any stunting by 24 months") +theme(legend.position="none")

  RRplot

  
  
  
  
  
  res_df2 <- NULL
  for(i in 1:length(arms)){
    
    tr<-arms[i]
    
    dsub_tr <- d_tr[d_tr$arm2==tr,]
    dsub_c <- d_c[d_c$studyid==unique(dsub_tr$studyid),]
    
    dsub<-rbind(dsub_c,dsub_tr)
    
    res <- washb_tmle(
      Y=dsub$ever_stunted, tr=dsub$arm2, 
      W = NULL,  pair = NULL, 
      family = "binomial", 
      contrast=c("Control", tr),
      Q.SL.library = c("SL.mean", "SL.glm"),
      g.SL.library = c("SL.mean", "SL.glm"), 
      pval = 0.2, seed = 12345, print=F)
    
    res <- data.frame(arm=tr, psi=res$estimates$RR$psi, ci.lb=res$estimates$RR$CI[1], ci.ub=res$estimates$RR$CI[2], pval= res$estimates$RR$pval)
    
    res_df2<-rbind(res_df2, res)
    
  }
  
  
  
  #mark sig effects
  res_df2$sig <- as.numeric(res_df2$pval < 0.05)
  
  
  
  
  
  #plot
  RRplot_nocluster<-ggplot(data=res_df2) + 
    labs( x = "Treatment arm", y = "Risk ratio") +
    geom_hline(yintercept = 1) +
    scale_y_continuous(breaks=c(0.125,0.25,0.5,0.75,1,1.5,2,4), trans='log10') +
    coord_flip(ylim = c(0.75, 1.5)) +
    geom_pointrange( mapping=aes(x=arm, y=psi, ymin=ci.lb, ymax=ci.ub , colour=sig)) +
    theme(panel.border = element_blank(), 
          strip.background = element_blank(), legend.position = "none") + 
    ggtitle("Intervention effects on any stunting by 24 months") +theme(legend.position="none")
  
  RRplot_nocluster