
rm(list=ls())
library(tidyverse)
library(reshape2)

#merge outcomes with covariates

# setwd("U:/UCB-SuperLearner/Stunting rallies/")
setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_clean_covariates.rds")

#load outcomes
load("st_prev_rf_outcomes.rdata")
load("st_cuminc_rf_outcomes.rdata")
load("st_rec_rf_outcomes.rdata")
load("st_vel_rf_outcomes.rdata")


dim(prev)
dim(cuminc)
dim(rev)
dim(vel_haz)


colnames(prev)
colnames(cuminc)
colnames(rev)
colnames(vel_haz)


head(prev)
head(cuminc)
head(rev)
head(vel_haz)

#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
rev$subjid <- as.character(rev$subjid)
vel_haz$subjid <- as.character(vel_haz$subjid)
vel_lencm$subjid <- as.character(vel_lencm$subjid)


#------------------------------------
# Create cumulative incidence dataset
#------------------------------------

#merge in covariates
cuminc <- cuminc %>% subset(., select = -c(tr))
d <- left_join(cuminc, cov, by=c("studyid", "subjid", "country"))
head(d)



#Vector of outcome names
Y<-c("ever_stunted")

#Vector of risk factor names
A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", "earlybf")  

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id,  file="st_cuminc_rf.Rdata")


#------------------------------------
# Create prevalence dataset
#------------------------------------


#merge in covariates
d <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("stunted","sstunted")



save(d, Y, A,V, id,  file="st_prev_rf.Rdata")


#------------------------------------
# Create recovery dataset
#------------------------------------

#merge in covariates
d <- left_join(rev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("s03rec24")

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_rec_rf.Rdata")




#------------------------------------
# Create growth velocity dataset
#------------------------------------

#HAZ

#merge in covariates
d <- left_join(vel_haz, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("y_rate_haz")


#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_haz_vel_rf.Rdata")


# Height in cm

#merge in covariates
d <- left_join(vel_lencm, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("y_rate_len")


#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_len_vel_rf.Rdata")


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Create intervention effects datasets
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


load("st_int_prev.RData")
load("st_int_cuminc.RData")

int_cov <- readRDS("U:/data/intervention_cov_dataset.rds")

#create outcome dataframes for secondary contrasts
int_prev2 <- int_prev %>% filter(studyid=="ki1112895-iLiNS-Zinc" | studyid=="ki1148112-iLiNS-DYAD-M")
int_cuminc2 <- int_cuminc %>% filter(studyid=="ki1112895-iLiNS-Zinc" | studyid=="ki1148112-iLiNS-DYAD-M")

int_prev2$studyid[int_prev2$studyid=="ki1112895-iLiNS-Zinc"] <- "iLiNS-Zinc_ZvLNS"
int_prev2$studyid[int_prev2$studyid=="ki1148112-iLiNS-DYAD-M"] <- "iLiNS_DYADM_LNS"

int_cuminc2$studyid[int_cuminc2$studyid=="ki1112895-iLiNS-Zinc"] <- "iLiNS-Zinc_ZvLNS"
int_cuminc2$studyid[int_cuminc2$studyid=="ki1148112-iLiNS-DYAD-M"] <- "iLiNS_DYADM_LNS"



#-------------------------------------
# Prevalence
#-------------------------------------


#merge in covariates
d <- left_join(int_prev, int_cov, by=c("studyid", "subjid", "country"))
head(d)

#merge in secondary intervention contrasts
d2 <- left_join(int_prev2, int_cov, by=c("studyid", "subjid", "country"))
d2 <- d2 %>% filter(!is.na(stunted) & !is.na(tr))

d <- bind_rows(d, d2)
d <- droplevels(d)




#Vector of outcome names
# The age period over which cumulative incidence is calculated will depend on the timing of the delivery of the intervention. 
# Zinc, LNS, and other interventions:  6-12 months, 12-18 months, 18-24 months, and intervention delivery to 24 months.
# Pre-birth maternal interventions: Stunting incidence at birth, and post-birth stunting from 0-6 months in age.

Y<-c("stunted")

#Vector of risk factor names
A<-c("tr")

#Vector of covariate names
W<-c("")

#Subgroup variable
#Objective 3: infant sex, stunting status at enrollment, wasting status at enrollment, gestational age, 
#geographic region (Asia, Africa, Latin America), exclusive or predominant breastfeeding in the first 6 months, 
#maternal age, maternal education, maternal height, or maternal BMI, birth order, household food insecurity, 
#number children under 5 years of age in the household

#Create interactions to have single stratification variables
d$intXsex <- interaction( d$sex, d$agecat, drop = T, sep = "_")
d$intXenstunt <- interaction( d$enstunt, d$agecat, drop = T, sep = "_")
d$intXenwast <- interaction( d$enwast, d$agecat, drop = T, sep = "_")
d$intXgagebrth <- interaction( d$gagebrth, d$agecat, drop = T, sep = "_")
d$intXpredexfd6 <- interaction( d$predexfd6, d$agecat, drop = T, sep = "_")
d$intXmage <- interaction( d$mage, d$agecat, drop = T, sep = "_")
d$intXmhtcm <- interaction( d$mhtcm, d$agecat, drop = T, sep = "_")
d$intXmbmi <- interaction( d$mbmi, d$agecat, drop = T, sep = "_")
d$intXmeducyrs <- interaction( d$meducyrs, d$agecat, drop = T, sep = "_")
d$intXparity <- interaction( d$parity, d$agecat, drop = T, sep = "_")
d$intXhfoodsec <- interaction( d$hfoodsec, d$agecat, drop = T, sep = "_")
d$intXnchldlt5 <- interaction( d$nchldlt5, d$agecat, drop = T, sep = "_")
d$intXhhwealth_quart <- interaction( d$hhwealth_quart, d$agecat, drop = T, sep = "_")

V <- c("agecat", 
       "intXsex", 
       "intXenstunt", 
       "intXenwast", 
       "intXgagebrth", 
       "intXpredexfd6", 
       "intXmage", 
       "intXmhtcm", 
       "intXmbmi", 
       "intXmeducyrs", 
       "intXparity", 
       "intXhfoodsec", 
       "intXnchldlt5", 
       "intXhhwealth_quart")


#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_prev_int.Rdata")



#-------------------------------------
# Cumulative incidence
#-------------------------------------


int_cuminc <- int_cuminc %>% subset(., select = -c(tr))
int_cuminc2 <- int_cuminc2 %>% subset(., select = -c(tr))

#merge in covariates
d <- left_join(int_cuminc, int_cov, by=c("studyid", "subjid", "country"))
head(d)

#merge in secondary intervention contrasts
d2 <- left_join(int_cuminc2, int_cov, by=c("studyid", "subjid", "country"))
d2 <- d2 %>% filter(!is.na(ever_stunted) & !is.na(tr))

d <- bind_rows(d, d2)
d <- droplevels(d)


#Vector of outcome names
# The age period over which cumulative incidence is calculated will depend on the timing of the delivery of the intervention. 
# Zinc, LNS, and other interventions:  6-12 months, 12-18 months, 18-24 months, and intervention delivery to 24 months.
# Pre-birth maternal interventions: Stunting incidence at birth, and post-birth stunting from 0-6 months in age.

Y<-c("ever_stunted")

#Vector of risk factor names
A<-c("tr")

#Vector of covariate names
W<-c("")

#Subgroup variable
#Objective 3: infant sex, stunting status at enrollment, wasting status at enrollment, gestational age, 
#geographic region (Asia, Africa, Latin America), exclusive or predominant breastfeeding in the first 6 months, 
#maternal age, maternal education, maternal height, or maternal BMI, birth order, household food insecurity, 
#number children under 5 years of age in the household

#Create interactions to have single stratification variables
d$intXsex <- interaction( d$sex, d$agecat, drop = T, sep = "_")
d$intXenstunt <- interaction( d$enstunt, d$agecat, drop = T, sep = "_")
d$intXenwast <- interaction( d$enwast, d$agecat, drop = T, sep = "_")
d$intXgagebrth <- interaction( d$gagebrth, d$agecat, drop = T, sep = "_")
d$intXpredexfd6 <- interaction( d$predexfd6, d$agecat, drop = T, sep = "_")
d$intXmage <- interaction( d$mage, d$agecat, drop = T, sep = "_")
d$intXmhtcm <- interaction( d$mhtcm, d$agecat, drop = T, sep = "_")
d$intXmbmi <- interaction( d$mbmi, d$agecat, drop = T, sep = "_")
d$intXmeducyrs <- interaction( d$meducyrs, d$agecat, drop = T, sep = "_")
d$intXparity <- interaction( d$parity, d$agecat, drop = T, sep = "_")
d$intXhfoodsec <- interaction( d$hfoodsec, d$agecat, drop = T, sep = "_")
d$intXnchldlt5 <- interaction( d$nchldlt5, d$agecat, drop = T, sep = "_")
d$intXhhwealth_quart <- interaction( d$hhwealth_quart, d$agecat, drop = T, sep = "_")

V <- c("agecat", 
       "intXsex", 
       "intXenstunt", 
       "intXenwast", 
       "intXgagebrth", 
       "intXpredexfd6", 
       "intXmage", 
       "intXmhtcm", 
       "intXmbmi", 
       "intXmeducyrs", 
       "intXparity", 
       "intXhfoodsec", 
       "intXnchldlt5", 
       "intXhhwealth_quart")


#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_cuminc_int.Rdata")






#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Create list of adjustment variables
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

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
  
  enstunt=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
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
  
  anywast06=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              #"W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
              "vagbrth","hdlvry",
              "single",
              "W_nrooms","W_nhh","W_nchldlt5",
              "month","brthmon","W_parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  pers_wast=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              #"W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
              "vagbrth","hdlvry",
              "single",
              "W_nrooms","W_nhh","W_nchldlt5",
              "month","brthmon","W_parity",
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
  
  predexfd6=c("arm","sex", "W_mage", "W_fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              "W_mhtcm","W_mwtkg","W_bmi", "W_fhtcm",
              "vagbrth","hdlvry",
              "W_gagebrth","W_birthwt","W_birthlen",
              "single",
              "W_nrooms","W_nhh","W_nchldlt5",
              "month","brthmon","W_parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"),
  
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

