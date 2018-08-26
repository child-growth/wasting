



#----------------------------------
# Script packages and functions
#----------------------------------

rm(list=ls())
library(caret)
library(tmle)
library(washb)
library(tidyverse)

source("U:/R scripts/HBGDki_function.R")


# XXXXXXXXXXXX
# NOTE
# Need to add code to subset ilins dose from analysis
# and set lns instead of c as reference for zinc
# XXXXXXXXXXXX


drop_missing <- function(df, variable){
  if(sum(df[,paste0("miss_",variable)])==nrow(df)){
    df$STUDYID="DROP"
  }
  return(df)
}

#set superlearner libraries
unadjusted_lib <- "SL.glm"
adjusted_lib <- c("SL.mean","SL.glm","SL.glmnet")



#-------------------------------
# Prevalent stunting and wasting
#-------------------------------

load("U:/data/Compiled Datasets/PrevStunt24.Rdata")

#subset to intervention studies 
d <- d %>% filter(!is.na(tr)) %>% as.data.frame()
d$COUNTRY <- as.character(d$COUNTRY)
d$STUDYID <- as.character(d$STUDYID)

#mark prevalence stunting and wasting
d$stunt <- ifelse(d$HAZ < -2, 1, 0)
d$wast <- ifelse(d$WHZ < -2, 1, 0)

#null covariates for unadjusted analysis
d <- d %>% mutate(w1=1, w2=1) %>% as.data.frame()

heatmap_df <- d %>% group_by(STUDYID, tr) %>% 
                    summarize(meanWHZ=mean(WHZ), meanHAZ=mean(HAZ)) %>%
                    as.data.frame()

Avar <- "tr"
A <- (d[,Avar])
n.cat <- length(levels(A))
Alevels<-levels(A)
Acuts <- c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5)[1:(length(Alevels)-1)]

table(A)
table(d[,paste0("miss_",Avar)])



HAZ_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                       Y="HAZ",
                                       W=c("w1","w2"),
                                       n.cat=n.cat,
                                       A=Avar,
                                       Acuts=Acuts,
                                       Alevels=Alevels,
                                       reflevel=1,
                                       family="gaussian",
                                       SLlibrary=unadjusted_lib,
                                       outputdf=NULL,
                                       overall.dist=F,
                                       sparseN=4,
                                       adjusted=F)))) %>% as.data.frame()


stunt_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                       Y="stunt",
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
HAZ_adj <- NULL
# HAZ_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
#   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
#                                        Y="HAZ",
#                                        W=colnames(d)[which(!(colnames(d) %in% c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt","tr", Avar, paste0("miss_",Avar))))],
#                                        n.cat=n.cat,
#                                        A=Avar,
#                                        Acuts=Acuts,
#                                        Alevels=Alevels,
#                                        reflevel=1,
#                                        family="gaussian",
#                                        SLlibrary=adjusted_lib,
#                                        outputdf=NULL,
#                                        overall.dist=F,
#                                        sparseN=4,
#                                        adjusted=T)))) %>% as.data.frame()

stunt_adj <- NULL
# stunt_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
#   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
#                                        Y="stunt",
#                                        W=colnames(d)[which(!(colnames(d) %in% c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt","tr", Avar, paste0("miss_",Avar))))],
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


WHZ_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                       Y="WHZ",
                                       W=c("w1","w2"),
                                       n.cat=n.cat,
                                       A=Avar,
                                       Acuts=Acuts,
                                       Alevels=Alevels,
                                       reflevel=1,
                                       family="gaussian",
                                       SLlibrary=unadjusted_lib,
                                       outputdf=NULL,
                                       overall.dist=F,
                                       sparseN=4,
                                       adjusted=F)))) %>% as.data.frame()


wast_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                       Y="wast",
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
WHZ_adj <- NULL
# WHZ_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
#   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
#                                        Y="WHZ",
#                                        W=colnames(d)[which(!(colnames(d) %in% c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt","tr", Avar, paste0("miss_",Avar))))],
#                                        n.cat=n.cat,
#                                        A=Avar,
#                                        Acuts=Acuts,
#                                        Alevels=Alevels,
#                                        reflevel=1,
#                                        family="gaussian",
#                                        SLlibrary=adjusted_lib,
#                                        outputdf=NULL,
#                                        overall.dist=F,
#                                        sparseN=4,
#                                        adjusted=T)))) %>% as.data.frame()

wast_adj <- NULL
# wast_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
#   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
#                                        Y="wast",
#                                        W=colnames(d)[which(!(colnames(d) %in% c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt","tr", Avar, paste0("miss_",Avar))))],
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



#-------------------------------
# Cumulative incidence of wasting
#-------------------------------

load("U:/data/Compiled Datasets/WastInc0-24.Rdata")


d <- d %>% filter(!is.na(tr)) %>% as.data.frame()
d$COUNTRY <- as.character(d$COUNTRY)
d$STUDYID <- as.character(d$STUDYID)


#null covariates for unadjusted analysis
d <- d %>% mutate(w1=1, w2=1) %>% as.data.frame()


wast_inc_024_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
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
wast_inc_024_adj <- NULL
# wast_adj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
#   do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
#                                        Y="wast_inc",
#                                        W=colnames(d)[which(!(colnames(d) %in% c("STUDYID","COUNTRY","SUBJID","AGEDAYS","HAZ","stunt","sevstunt","tr", Avar, paste0("miss_",Avar))))],
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



load("U:/data/Compiled Datasets/WastInc0-6.Rdata")


d <- d %>% filter(!is.na(tr)) %>% as.data.frame()
d$COUNTRY <- as.character(d$COUNTRY)
d$STUDYID <- as.character(d$STUDYID)


#null covariates for unadjusted analysis
d <- d %>% mutate(w1=1, w2=1) %>% as.data.frame()


wast_inc_06_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
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
wast_inc_06_adj <- NULL


load("U:/data/Compiled Datasets/WastInc6-24.Rdata")


d <- d %>% filter(!is.na(tr)) %>% as.data.frame()
d$COUNTRY <- as.character(d$COUNTRY)
d$STUDYID <- as.character(d$STUDYID)


#null covariates for unadjusted analysis
d <- d %>% mutate(w1=1, w2=1) %>% as.data.frame()


wast_inc_624_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
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
wast_inc_624_adj <- NULL


#-------------------------------
# Wasting recovery
#-------------------------------

load("U:/data/Compiled Datasets/WastRec0-24.Rdata")


d <- d %>% filter(!is.na(tr)) %>% as.data.frame()
d$COUNTRY <- as.character(d$COUNTRY)
d$STUDYID <- as.character(d$STUDYID)


#null covariates for unadjusted analysis
d <- d %>% mutate(w1=1, w2=1) %>% as.data.frame()

wast_rec_024_unadj <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(  tmle_risk(dat=as.data.frame(.),
                                   Y="wast_rec60d",
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
wast_rec_024_adj <- NULL



#-------------------------------
# save objects
#-------------------------------

setwd("U:/results/Nutritional Intervention Results")
save(heatmap_df,
     HAZ_unadj, stunt_unadj, HAZ_adj, stunt_adj,
     WHZ_unadj, wast_unadj, WHZ_adj, wast_adj,
     wast_inc_024_unadj, #sevwast_inc_024_unadj,
     wast_inc_024_adj, #sevwast_inc_024_adj,
     wast_inc_06_unadj,  wast_inc_06_adj, 
     wast_inc_624_unadj,  wast_inc_624_adj, 
     wast_rec_024_unadj, #sevwast_rec_024_unadj,
     wast_rec_024_adj, #sevwast_rec_024_adj,
     file="Nutritional_Intervention_Results.Rdata")
