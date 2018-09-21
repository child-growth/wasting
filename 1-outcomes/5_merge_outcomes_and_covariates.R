




rm(list=ls())
library(tidyverse)
library(reshape2)

#merge outcomes with covariates

setwd("U:/ucb-superlearner/Wasting rallies/")

#load covariates
cov<-readRDS("U:/ucb-superlearner/stunting rallies/FINAL_clean_covariates.rds")

#load outcomes
load("wast_prev.RData")
load("wast_cuminc.rdata")
load("wast_cuminc_nobirth.rdata")
load("pers_wast.rdata")
load("wast_rec.rdata")


#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
cuminc_nobirth$subjid <- as.character(cuminc_nobirth$subjid)
rec$subjid <- as.character(rec$subjid)
pers_wast$subjid <- as.character(pers_wast$subjid)



#------------------------------------
# Create cumulative incidence dataset
#------------------------------------

#merge in covariates
cuminc <- cuminc %>% subset(., select = -c(tr))
d <- left_join(cuminc, cov, by=c("studyid", "subjid", "country"))
head(d)



#Vector of outcome names
Y<-c("ever_wasted")

#Vector of risk factor names
A<-c( "sex",               "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",       
      "nrooms",      "nchldlt5",    "nhh",              
      "hhwealth_quart", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec")  



#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id,  file="wast_cuminc_rf.Rdata")


#------------------------------------
# Create cumulative incidence dataset
# - no birth incidence
#------------------------------------

#merge in covariates
cuminc_nobirth <- cuminc_nobirth %>% subset(., select = -c(tr))
cuminc_nobirth <- bind_rows(cuminc_nobirth, cuminc[cuminc$agecat=="6-24 months",])

d <- left_join(cuminc_nobirth, cov, by=c("studyid", "subjid", "country"))
head(d)



#Vector of outcome names
Y<-c("ever_wasted")

#Vector of risk factor names
A<-c( "gagebrth", "birthwt",    
      "birthlen", "vagbrth", "hdlvry",    
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  
      "impsan", "safeh20",
      "perdiar6", "perdiar24", 
      "predfeed3", "exclfeed3", "predfeed6", "exclfeed6", "predfeed36", "exclfeed36",
      "predexfd6", "earlybf", "month")

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id,  file="wast_cuminc_nobirth_rf.Rdata")


#------------------------------------
# Create prevalence dataset
#------------------------------------


#merge in covariates
d <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("wasted","swasted")


A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", 
      "predfeed3", "exclfeed3", "predfeed6", "exclfeed6", "predfeed36", "exclfeed36",
      "earlybf")  



save(d, Y, A,V, id,  file="wast_prev_rf.Rdata")


#------------------------------------
# Create recovery dataset
#------------------------------------

#merge in covariates
d <- left_join(rec, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-"wast_rec90d"


A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", 
      "predfeed3", "exclfeed3", "predfeed6", "exclfeed6", "predfeed36", "exclfeed36",
      "earlybf")  

save(d, Y, A,V, id,  file="wast_rec_rf.Rdata")

#------------------------------------
# Create persistant wasting dataset
#------------------------------------

#merge in covariates
d <- left_join(pers_wast, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-"pers_wast"


A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", 
      "predfeed3", "exclfeed3", "predfeed6", "exclfeed6", "predfeed36", "exclfeed36",
      "earlybf")  

save(d, Y, A,V, id,  file="pers_wast_rf.Rdata")


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Create list of adjustment variables
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


bf_covariates = c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
                  "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
                  "vagbrth","hdlvry",
                  "W_gagebrth","W_birthwt","W_birthlen",
                  "single",
                  "W_nrooms","W_nhh","W_nchldlt5",
                  "month","brthmon","W_parity",
                  "trth2o","cleanck","impfloor","impsan","safeh20")


adjustment_sets <- list( 
  
  gagebrth=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             #"W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "brthmon","W_parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),         
  
  birthwt=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            #"W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
            "vagbrth","hdlvry",
            "single",
            "W_nrooms","W_nhh","W_nchldlt5",
            "brthmon","W_parity",
            "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  birthlen=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             #"W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "vagbrth","hdlvry",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "brthmon","W_parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  
  enwast=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           #"W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "vagbrth","hdlvry",
           "single",
           "W_nrooms","W_nhh","W_nchldlt5",
           "brthmon","W_parity",
           "trth2o","cleanck","impfloor","impsan","safeh20"),  
  
  vagbrth=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
            "hdlvry",
            "single",
            "W_nrooms","W_nhh","W_nchldlt5",
            "brthmon","W_parity",
            "trth2o","cleanck","impfloor","impsan","safeh20"),     
  
  hdlvry=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "single",
           "W_nrooms","W_nhh","W_nchldlt5",
           "brthmon","W_parity",
           "trth2o","cleanck","impfloor","impsan","safeh20"),      
  
  mage=c("arm","W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
         "single",
         "W_nrooms","W_nhh","W_nchldlt5",
         "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  fage=c("arm","W_mage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
         "single",
         "W_nrooms","W_nhh","W_nchldlt5",
         "brthmon",
         "trth2o","cleanck","impfloor","impsan","safeh20"),     
  
  mhtcm=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "W_fhtcm",
          "single",
          "W_nrooms",
          "trth2o","cleanck","impfloor","impsan","safeh20"),    
  
  mwtkg=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "W_fhtcm",
          "single",
          "W_nrooms","W_nhh","W_nchldlt5",
          "brthmon","W_parity",
          "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  mbmi=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "W_fhtcm",
         "single",
         "W_nrooms","W_nhh","W_nchldlt5",
         "brthmon","W_parity",
         "trth2o","cleanck","impfloor","impsan","safeh20"),      
  
  single=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "W_nrooms","W_nhh","W_nchldlt5",
           "trth2o","cleanck","impfloor","impsan","safeh20"),    
  
  fhtcm=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "W_mhtcm","W_mwtkg","W_bmi",
          "single",
          "W_nrooms",
          "trth2o","cleanck","impfloor","impsan","safeh20"),     
  
  nrooms=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "single",
           "W_nhh","W_nchldlt5",
           "W_parity",
           "trth2o","cleanck","impfloor","impsan","safeh20"),    
  
  nhh=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
        "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
        "single",
        "W_nrooms",
        "W_parity",
        "trth2o","cleanck","impfloor","impsan","safeh20"),    
  
  nchldlt5=c("arm", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "single",
             "W_nrooms",
             "W_parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  hhwealth_quart=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", 
                   "W_gagebrth","W_birthwt","W_birthlen",
                   "single","W_nhh","W_nchldlt5",
                   "W_parity"), 
  
  parity=c("arm","W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "vagbrth","hdlvry",
           "single",
           "W_nrooms",
           "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  meducyrs=c("arm", "W_mage", "W_fage", "feducyrs", "hhwealth_quart",
             "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "W_gagebrth","W_birthwt","W_birthlen",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  feducyrs=c("arm", "W_mage", "W_fage", "meducyrs",  "hhwealth_quart", 
             "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "W_gagebrth","W_birthwt","W_birthlen",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  hfoodsec=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart",
             "vagbrth","hdlvry",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "brthmon","W_parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  
  trth2o=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "single",
           "W_nrooms","W_nhh","W_nchldlt5",
           "brthmon","W_parity",
           "cleanck","impfloor","impsan","safeh20"), 
  
  cleanck=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
            "single",
            "W_nrooms","W_nhh","W_nchldlt5",
            "W_parity",
            "trth2o","impfloor","impsan","safeh20"), 
  
  impfloor=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "W_parity",
             "trth2o","cleanck","impsan","safeh20"),  
  
  impsan=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
           "single",
           "W_nrooms","W_nhh","W_nchldlt5",
           "W_parity",
           "trth2o","cleanck","impfloor","safeh20"), 
  
  safeh20=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
            "single",
            "W_nrooms","W_nhh","W_nchldlt5",
            "W_parity",
            "trth2o","cleanck","impfloor","impsan"),
  
  perdiar6=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
             "vagbrth","hdlvry",
             "W_gagebrth","W_birthwt","W_birthlen",
             "single",
             "W_nrooms","W_nhh","W_nchldlt5",
             "month","brthmon","W_parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  perdiar24=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
              "vagbrth","hdlvry",
              "W_gagebrth","W_birthwt","W_birthlen",
              "single",
              "W_nrooms","W_nhh","W_nchldlt5",
              "month","brthmon","W_parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  predfeed3=bf_covariates,
  predfeed6=bf_covariates,
  predfeed36=bf_covariates,
  exclfeed3=bf_covariates,
  exclfeed6=bf_covariates, 
  exclfeed36=bf_covariates,
  predexfd6=bf_covariates,
  
  earlybf=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
            "vagbrth","hdlvry",
            "W_gagebrth","W_birthwt","W_birthlen",
            "single",
            "W_nrooms","W_nhh","W_nchldlt5",
            "brthmon","W_parity",
            "trth2o","cleanck","impfloor","impsan","safeh20")
)
save(adjustment_sets, file="adjustment_sets_list.Rdata")

