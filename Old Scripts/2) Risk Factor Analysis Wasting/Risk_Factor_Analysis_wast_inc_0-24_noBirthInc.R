



#----------------------------------
# Script packages and functions
#----------------------------------

rm(list=ls())
library(caret)
library(tmle)
library(washb)
library(tidyverse)
library(sl3)
library(tmle3)
library(data.table)


setwd("U:/GHAP-Data-Management")
source("HBGDki_functions.R")


drop_missing <- function(df, variable){
  if(sum(df[,paste0("miss_",variable)])==nrow(df)){
    df$STUDYID="DROP"
  }
  return(df)
}




#----------------------------------
# Load and prepare data
#----------------------------------


load("U:/data/Compiled Datasets/WastInc0-24_noBW.Rdata")
mean(d$wast_inc)

#subset intervention studies to the control arm
d <- d %>% filter(is.na(tr) | tr=="C" | miss_tr==1) %>% as.data.frame()
d$COUNTRY <- as.character(d$COUNTRY)
d$STUDYID <- as.character(d$STUDYID)


#Note: should nrooms and npersons be changes to categories?

wastinc_024_unadj <- wastinc_024_adj <-  list()
HAZ_unadj <- stunt_unadj <- HAZ_adj <- stunt_adj <- list()


#set superlearner libraries
unadjusted_lib <- "SL.glm"
#adjusted_lib <- c("SL.mean","SL.glm","SL.glmnet")
adjusted_lib <- c("SL.mean","SL.glm")


#Drop outcome variables not needed in the dataset
d <- subset(d, select= -c(WHZ,HAZ,wast,sevwast,period_length,           
                          sevwast_inc,sevwast_rec,wast_rec,                
                          wast_risk,sevwast_risk,wast_rec_risk,sevwast_rec_risk,        
                          wasting_episode,born_wast_inc,episode_ID,incident_age,            
                          maxage,duration,wasting_duration,born_sevwast_inc,        
                          sevwasting_episode,sev_episode_ID,sevwasting_duration,wast_rec90d,            
                          wast_rec60d,wast_rec30d,sevwast_inc90d,sevwast_inc60d,          
                          sevwast_inc30d,stunt,sevstunt))

#Set variables to exclude from adjustment set
exclude_vars <- c("STUDYID","COUNTRY","SUBJID","AGEDAYS",  "WHZ","HAZ","wast","sevwast","period_length",           
                  "sevwast_inc","wast_inc","sevwast_rec","wast_rec",                
                  "wast_risk","sevwast_risk","wast_rec_risk","sevwast_rec_risk",        
                  "wasting_episode","born_wast_inc","episode_ID","incident_age",            
                  "maxage","duration","wasting_duration","born_sevwast_inc",        
                  "sevwasting_episode","sev_episode_ID","sevwasting_duration","wast_rec90d",            
                  "wast_rec60d","wast_rec30d","sevwast_inc90d","sevwast_inc60d",          
                  "sevwast_inc30d","stunt","sevstunt")


#Set factor vars
factor_vars <- c("SEX", "birthorder", "birthmonth", "homedelivery",
                 "vagbirth", "single",
                 "breastfeeding",  
                 "nchild5", "ncomp", "nroom",
                 "chicken", "cow",
                 "improved.floor",
                 "improved.sanitation",
                 "safe.water",
                 "treat.water",
                 "SOAP",
                 "cleancook",
                 "DIARFL","month",
                 "enrolstunt",
                 "HHwealth_quart")


#Create a vector of reference levels for factor variables
factor_reflevels<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
#Check 
for(i in 1:length(factor_vars)){
  cat(factor_vars[i]," reflevel: ",factor_reflevels[i],"\n")
  d[,factor_vars[i]] <- as.factor(d[,factor_vars[i]])
  print(levels(d[,factor_vars[i]]))
}

#XXXXXXXXXXXXXXX
# Add code to check if the reference level is missing (I.E. for month)
#XXXXXXXXXXXXXXX

continious_vars <- c(
  "GAGEBRTH",
  "BIRTHLEN",
  "BIRTHWT",
  "MHTCM",
  "MWTKG", 
  "MBMI", 
  "MAGE", 
  "MEDUCYRS",
  "FHTCM", 
  "FAGE",
  "FEDUCYRS",
  "DURBRST")

# continious_vars <- c(
#   "BIRTHWT")



# Create vector indicating if quartiles should be based on the overall distribution
overall_dist_vec <- c(T,T,T,T,T,T,T,F,T,T,F,T,F,F)

#Check 
for(i in 1:length(continious_vars)){
  cat(continious_vars[i],"overall distribution? ",overall_dist_vec[i],"\n")
  d[,continious_vars[i]] <- as.numeric(d[,continious_vars[i]])
}



#----------------------------------
# Set reference levels
#----------------------------------

class(d$nchild5)
levels(d$nchild5)
d$nchild5 <- relevel(d$nchild5, ref="1")

levels(d$nroom)
d$nroom <- relevel(d$nroom, ref="2")

#----------------------------------
# Factor variables
#----------------------------------




for(i in 1:length(factor_vars)){
  print(factor_vars[i])
  Avar<-factor_vars[i]
  A <- (d[,Avar])
  n.cat <- length(levels(A))
  Alevels<-levels(A)
  Acuts <- c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5)[1:(length(Alevels)-1)]
  
  table(A)
  table(d[,paste0("miss_",Avar)])
  
  missing_var <- paste0("miss_",Avar)
  
  
  #drop observations with missing risk factor
  dsub <- d[d[,missing_var]==0,]
  
  #Make sure there is variation in the risk factor of interest in each study.
  sddf <- dsub
  colnames(sddf)[which(colnames(sddf) %in% Avar)] <- "A"
  sddf <-sddf %>%  group_by(STUDYID, COUNTRY) %>% mutate(A=as.numeric(as.factor(A))) %>% summarize(mean=mean(A, na.rm=T), sd=sd(A, na.rm=T)) %>% filter(!is.na(sd) & sd!=0)
  dsub <- dsub[paste0(dsub$STUDYID," ", dsub$COUNTRY) %in% paste0(sddf$STUDYID," ", sddf$COUNTRY),]
  dsub <- dsub %>% mutate(w1=1, w2=1) %>% as.data.frame()
  
  
  wastinc_024_unadj[[paste0(Avar)]] <- dsub %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
    do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                         Y="wast_inc",
                                         W=c("w1","w2"),
                                         n.cat=n.cat,
                                         A=Avar,
                                         Acuts=Acuts,
                                         Alevels=Alevels,
                                         reflevel=1,
                                         family="binomial",
                                         SLlibrary=unadjusted_lib,
                                         outputdf=NULL,
                                         overall.dist=F,
                                         sparseN=4,
                                         adjusted=F)))) %>% as.data.frame()
  
  # wastinc_024_adj[[paste0(Avar)]] <- dsub %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  #   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
  #                                        Y="wast_inc",
  #                                        W=colnames(dsub)[which(!(colnames(dsub) %in% c(exclude_vars, Avar, paste0("miss_",Avar))))],
  #                                        n.cat=n.cat,
  #                                        A=Avar,
  #                                        Acuts=Acuts,
  #                                        Alevels=Alevels,
  #                                        reflevel=1,
  #                                        family="binomial",
  #                                        SLlibrary=adjusted_lib,
  #                                        outputdf=NULL,
  #                                        overall.dist=F,
  #                                        sparseN=4,
  #                                        adjusted=T)))) %>% as.data.frame()
}





#----------------------------------
# Continious variables
#----------------------------------


for(i in 1:length(continious_vars)){
  print(continious_vars[i])
  Avar<-continious_vars[i]
  
  overall_dist<-overall_dist_vec[i]
  
  table(d[,paste0("miss_",Avar)])
  table(is.na(d[,paste0("miss_",Avar)]))
  
  table(d$STUDYID,d[,paste0("miss_",Avar)])
  
  missing_var <- paste0("miss_",Avar)
  
  #drop observations with missing risk factor
  dsub <- d[d[,missing_var]==0,]
  
  #Make sure there is variation in the risk factor of interest in each study.
  sddf <- dsub
  colnames(sddf)[which(colnames(sddf) %in% Avar)] <- "A"
  sddf <-sddf %>%  group_by(STUDYID, COUNTRY) %>% mutate(A=as.numeric(as.factor(A))) %>% summarize(mean=mean(A, na.rm=T), sd=sd(A, na.rm=T)) %>% filter(!is.na(sd) & sd!=0)
  dsub <- dsub[paste0(dsub$STUDYID," ", dsub$COUNTRY) %in% paste0(sddf$STUDYID," ", sddf$COUNTRY),]
  
  dsub <- dsub %>% mutate(w1=1, w2=1) %>% as.data.frame()
  dsub <- droplevels(dsub)
  
  if(overall_dist==T){
    Acuts=quantile(dsub[,Avar], probs = c(.25,.5,.75), na.rm=T)
    Alevels=c(paste0("<=",round(Acuts[1],3)), 
              paste0(round(Acuts[1],3),"-",round(Acuts[2],3)),
              paste0(round(Acuts[2],3),"-",round(Acuts[3],3)), 
              paste0(">",round(Acuts[3],3))) 
  }else{
    Acuts=c(1.5,2.5,3.5)
    Alevels=c("Q1","Q2","Q3","Q4")
  }
  
  
  wastinc_024_unadj[[paste0(Avar)]] <- dsub %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
    do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                         Y="wast_inc",
                                         W=c("w1","w2"),
                                         n.cat=4,
                                         A=Avar,
                                         Acuts=Acuts,
                                         Alevels=Alevels,
                                         reflevel=3,
                                         family="binomial",
                                         SLlibrary=unadjusted_lib,
                                         outputdf=NULL,
                                         overall.dist=overall_dist,
                                         sparseN=4,
                                         adjusted=F)))) %>% as.data.frame()
  
  # wastinc_024_adj[[paste0(Avar)]] <- dsub %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  #   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
  #                                        Y="wast_inc",
  #                                        W=colnames(dsub)[which(!(colnames(dsub) %in% c(exclude_vars, Avar, paste0("miss_",Avar))))],
  #                                        n.cat=4,
  #                                        A=Avar,
  #                                        Acuts=Acuts,
  #                                        Alevels=Alevels,
  #                                        reflevel=1,
  #                                        family="binomial",
  #                                        SLlibrary=adjusted_lib,
  #                                        outputdf=NULL,
  #                                        overall.dist=overall_dist,
  #                                        sparseN=4,
  #                                        adjusted=T)))) %>% as.data.frame()
}



#----------------------------------
# Save objects
#----------------------------------




save(wastinc_024_unadj, wastinc_024_adj, 
     file="U:/results/Risk_Factor_Results_wastinc0-24_noBW.Rdata")




