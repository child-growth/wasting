


rm(list=ls())
library(tidyverse)
library(reshape2)
library(data.table)

#merge outcomes with covariates

setwd("U:/UCB-SuperLearner/Wasting rallies/")

#load covariates
cov<-readRDS("U:/ucb-superlearner/stunting rallies/FINAL_clean_covariates.rds")

#load outcomes
#load outcomes
load("wast_prev.RData")
load("wast_cuminc.rdata")
load("wast_cuminc_nobirth.rdata")
load("pers_wast.rdata")
load("wast_rec.rdata")
load("monthly_whz.rdata")

dim(prev)
dim(cuminc)
dim(rec)
dim(vel_haz)


colnames(prev)
colnames(cuminc)
colnames(rec)
colnames(vel_haz)


head(prev)
head(cuminc)
head(rec)
head(vel_haz)

#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
rec$subjid <- as.character(rec$subjid)
pers_wast$subjid <- as.character(pers_wast$subjid)
monthly_whz$subjid <- as.character(monthly_whz$subjid)


#------------------------------------
# Create tabulation function
#------------------------------------


A<-c( "sex", "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", "earlybf")  

dfull <- d

#Create a risk factor tabulation function
RF_tab <- function(d, Yvar="ever_wasted", statistic="N", age=NULL, A=c( "sex",              "gagebrth",      "birthwt",      
                                  "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
                                  "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
                                  "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
                                  "feducyrs", "hfoodsec",  
                                  "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
                                  "perdiar6", "perdiar24", "predexfd6", "earlybf")){
  if(is.null(age)){
    agecat <- d$agecat[1]    
  }else{
    d <- d %>% filter(agecat==age)
    agecat <- d$agecat[1]    
  }

  
  d <- d %>% subset(., select=c("studyid",  "country", Yvar, A))
  colnames(d)[3] <-"Y"
  
  for(i in 4:ncol(d)){
      df <-d[,c(1:3,i)]
      Avariable <- colnames(df)[4] 
      colnames(df)[4] <- "Avar"
      res <- df %>% group_by(studyid, country, Avar) %>%
        filter(!is.na(Avar)) %>%
      summarize(N=n(), N_cases=sum(Y), Mean=mean(Y, na.rm=T))
      if(statistic=="N"){
        res <- subset(res, select = -c(N_cases, Mean))
      }
      if(statistic=="N_cases"){
        res <- subset(res, select = -c(N, Mean))
      }
      if(statistic=="mean"){
        res <- subset(res, select = -c(N, N_cases))
      }
      colnames(res)[4] <- Avariable
      
      res <- res %>%
        gather(key, value, -studyid, -country, -Avar) %>%
        unite(col, key, Avar, sep = "__") %>%
        spread(col, value)
      
      if(i==4){
        resdf<-res
      }else{
        resdf<-merge(resdf, res, by=c("studyid", "country"), all.x=T, all.y=T)
      }
  }
  
  
  resdf <- melt(resdf, id.vars = c("studyid","country"))
  resdf$agecat <- agecat
  resdf$intervention_variable <- sapply(strsplit(as.character(resdf$variable), "__", fixed=T), `[`, 1)
  resdf$intervention_level <- sapply(strsplit(as.character(resdf$variable), "__", fixed=T), `[`, 2)
  resdf <- resdf %>% subset(., select = -c(variable))
  
 
  if(statistic=="N_cases"){
    resdf <- resdf %>% rename(N_cases = value)
  }else{
    resdf <- resdf %>% rename(N = value)
  }
   return(resdf) 
}

#------------------------------------
# Create outcome dataframes
#------------------------------------


#merge in covariates
prev <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
cuminc <- left_join(cuminc, cov, by=c("studyid", "subjid", "country"))
rec <- left_join(rec, cov, by=c("studyid", "subjid", "country"))
pers_wast <- left_join(pers_wast, cov, by=c("studyid", "subjid", "country"))
monthly_whz <- left_join(monthly_whz, cov, by=c("studyid", "subjid", "country"))




#------------------------------------
# Tabulate N's and n cases
#------------------------------------

table(cuminc$agecat)
cumincN_024 <- RF_tab(cuminc[cuminc$agecat=="0-24 months",])
cumincCase_024 <- RF_tab(cuminc[cuminc$agecat=="0-24 months",], statistic="N_cases")
cumincN_624 <- RF_tab(cuminc[cuminc$agecat=="6-24 months",])
cumincCase_624 <- RF_tab(cuminc[cuminc$agecat=="6-24 months",], statistic="N_cases")
cumincN_06 <- RF_tab(cuminc[cuminc$agecat=="0-6 months",])
cumincCase_06 <- RF_tab(cuminc[cuminc$agecat=="0-6 months",], statistic="N_cases")


table(prev$agecat)
prevN_birth <- RF_tab(prev[prev$agecat=="Birth",], Yvar="wasted")
prevCase_birth <- RF_tab(prev[prev$agecat=="Birth",], Yvar="wasted", statistic="N_cases")
prevN_6 <- RF_tab(prev[prev$agecat=="6 months",], Yvar="wasted")
prevCase_6 <- RF_tab(prev[prev$agecat=="6 months",], Yvar="wasted", statistic="N_cases")
prevN_24 <- RF_tab(prev[prev$agecat=="24 months",], Yvar="wasted")
prevCase_24 <- RF_tab(prev[prev$agecat=="24 months",], Yvar="wasted", statistic="N_cases")


table(pers_wast$agecat)
pers_wastN_024 <- RF_tab(pers_wast[pers_wast$agecat=="0-24 months",], Yvar="pers_wast.x")
pers_wastCase_024 <- RF_tab(pers_wast[pers_wast$agecat=="0-24 months",], statistic="N_cases", Yvar="pers_wast.x")
pers_wastN_624 <- RF_tab(pers_wast[pers_wast$agecat=="6-24 months",], Yvar="pers_wast.x")
pers_wastCase_624 <- RF_tab(pers_wast[pers_wast$agecat=="6-24 months",], statistic="N_cases", Yvar="pers_wast.x")
pers_wastN_06 <- RF_tab(pers_wast[pers_wast$agecat=="0-6 months",], Yvar="pers_wast.x")
pers_wastCase_06 <- RF_tab(pers_wast[pers_wast$agecat=="0-6 months",], statistic="N_cases", Yvar="pers_wast.x")



#------------------------------------
# plot N's by month
#------------------------------------

#Monthly N's
whzN <- lapply(levels(monthly_whz$agecat), function(x) RF_tab(d=monthly_whz, Yvar="whz", statistic="N",age=x))
whzN=as.data.frame(rbindlist(whzN))
whzN <- whzN %>% filter(!is.na(N))
head(whzN)

p1 <- ggplot(monthly_whzN, aes(x=as.numeric(agecat), color=studyid, fill=studyid)) + geom_col(aes(y=N)) + facet_wrap(~intervention_variable) + theme(legend.position = "none")
p1


#Monthly N's
monthly_whzN <- lapply(levels(monthly_whz$agecat[monthly_whz$measurefreq=="monthly"]), function(x) RF_tab(d=monthly_whz[monthly_whz$measurefreq=="monthly",], Yvar="whz", statistic="N",age=x))
monthly_whzN=as.data.frame(rbindlist(monthly_whzN))
monthly_whzN <- monthly_whzN %>% filter(!is.na(N))
head(monthly_whzN)

p2 <- ggplot(monthly_whzN, aes(x=as.numeric(agecat), color=studyid, fill=studyid)) + geom_col(aes(y=N)) + facet_wrap(~intervention_variable) + theme(legend.position = "none")
p2

ggsave(p1, file="U:/Figures/Nobs_per_RF_quarterly.png")
ggsave(p2, file="U:/Figures/Nobs_per_RF_monthly.png")


#------------------------------------
# Save tabulated objects
#------------------------------------
ls()
save(cumincN_024, 
     cumincCase_024, 
     cumincN_624, 
     cumincCase_624, 
     cumincN_06, 
     cumincCase_06, 
     prevN_birth, 
     prevCase_birth, 
     prevN_6, 
     prevCase_6, 
     prevN_24, 
     prevCase_24,
     pers_wastN_024,
     pers_wastCase_024,
     pers_wastN_624,
     pers_wastCase_624,
     pers_wastN_06,
     pers_wastCase_06,
     whzN,
     monthly_whzN,
     file="U:/ucb-superlearner/Stunting rallies/wast_RiskFactor_Ns.Rdata")







