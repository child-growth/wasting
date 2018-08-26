
# load packages
rm(list=ls())
library(tidyverse)

setwd("U:/data/WastIncDatasets")


#-------------------------------------
# Counting birth incidence
#-------------------------------------
load("gmsn_inc.Rdata")
load("cntt_inc.Rdata")
load("gbsc_inc.Rdata")
load("cmin_inc.Rdata")
load("phua_inc.Rdata")
load("tzc2_inc.Rdata")
load("cmc_inc.Rdata")
load("ee_inc.Rdata")
load("irc_inc.Rdata")
load("tdc_inc.Rdata")
load("rspk_inc.Rdata")
load("mled_inc.Rdata")


mean(phua_inc$wast_rec90d, na.rm=T)
mean(phua_inc$wast_rec90d[phua_inc$wast_inc==1], na.rm=T)

#Define set of short ID's I care about
datasets <- list(
  gmsn_inc, 
  cntt_inc, 
  gbsc_inc, 
  cmin_inc_brazil, 
  cmin_inc_guinea_bissau, 
  cmin_inc_peru, 
  cmin_inc_bangladesh,   
  phua_inc, 
  tzc2_inc, 
  cmc_inc, 
  ee_inc, 
  irc_inc,
  tdc_inc,
  rspk_inc, 
  mled_inc_bangladesh,
  mled_inc_brazil,             
  mled_inc_india,
  mled_inc_nepal,
  mled_inc_peru,
  mled_inc_southafrica, 
  mled_inc_tanzania)


#Make list of summary tables
tablelist <- list(
  gmsn_inc_table, 
  cntt_inc_table, 
  gbsc_inc_table, 
  cmin_inc_table_brazil, 
  cmin_inc_table_guinea_bissau, 
  cmin_inc_table_peru, 
  cmin_inc_table_bangladesh,   
  phua_inc_table, 
  tzc2_inc_table, 
  cmc_inc_table, 
  ee_inc_table, 
  irc_inc_table,
  tdc_inc_table,
  rspk_inc_table, 
  mled_inc_table_bangladesh,
  mled_inc_table_brazil,             
  mled_inc_table_india,
  mled_inc_table_nepal,
  mled_inc_table_peru,
  mled_inc_table_southafrica,
  mled_inc_table_tanzania)




#Define the corresponding study names
studynames <- c("GMS-Nepal (Growth Monitoring Study, Nepal)",
                "CNTT (Evaluation and Control of Neglected Mucosal Enteric Infections in Childhood)",
                "GBSC (Longitudinal study of Bovine Serum Concentrate in Guatemala)",
                "CMIN (Child Malnutrition and Infection Network): Brazil",
                "CMIN (Child Malnutrition and Infection Network): Guinea Bissau",
                "CMIN (Child Malnutrition and Infection Network): Peru",
                "CMIN (Child Malnutrition and Infection Network): Bangladesh",
                "PHUA (Infant Growth in Peru)",
                "TZC2 (Tanzania Child 2)",
                "CMC Vellore Birth Cohort 2002",
                "EE (Pakistan Study of Biomarkers for Environmental Enteropathy)",
                "IRC (Vellore Crypto Study)",
                "TDC (Vellore Bottled Water BC)",
                "RSPK (Respiratory Pathogens Birth Cohort)",
                "MAL-ED Study: Bangladesh",
                "MAL-ED Study: Brazil",
                "MAL-ED Study: India",
                "MAL-ED Study: Nepal",
                "MAL-ED Study: Peru",
                "MAL-ED Study: South Africa",
                "MAL-ED Study: Tanzania"
)






d<-NULL
for(i in 1:length(datasets)){
  cat(datasets[[i]]$STUDYID[1], " ", datasets[[i]]$COUNTRY[1])
  temp<-cbind(rep(datasets[[i]]$STUDYID[1], nrow(tablelist[[i]]$means)), rep(datasets[[i]]$COUNTRY[1],nrow(tablelist[[i]]$means)), tablelist[[i]]$means)
  d<-rbind(d,temp)
}  
colnames(d)[1:2]<-c("STUDYID","COUNTRY")

#Create unique country/study combination
d$STUDYID<-gsub("^k.*?-","",d$STUDYID)
d$COUNTRY<-as.character(d$COUNTRY)
d$COUNTRY[d$COUNTRY=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
d$country_cohort<-paste0(d$STUDYID, " ", d$COUNTRY)

#Group by region
table(d$COUNTRY)

d$region <- NA
d$region[d$COUNTRY=="TANZANIA"|d$COUNTRY=="SOUTH AFRICA"|d$COUNTRY=="GUINEA-BISSAU"]<-2
d$region[d$COUNTRY=="BANGLADESH"|d$COUNTRY=="PAKISTAN"|d$COUNTRY=="INDIA"|d$COUNTRY=="NEPAL"]<-1
d$region[d$COUNTRY=="GUATEMALA"|d$COUNTRY=="PERU"|d$COUNTRY=="BRAZIL"]<-3

d <- d %>% arrange(region)
d$country_cohort<-factor(d$country_cohort, levels=unique(d$country_cohort))


#Drop out MLED PAkistan (bad anthropometry measures) and CMIN Brazil (not monthly)
d <- d[d$country_cohort!="MAL-ED PAKISTAN" & d$country_cohort!="CMIN BRAZIL",] 

#Save summary statistics list to create pooled estimates
setwd("U:/results/Wasting Descriptive Epi")
save(d, file="descriptive_epi_mean_monthly_cohorts.Rdata")





#-------------------------------------
# Not Counting birth incidence
#-------------------------------------
rm(list=ls())
setwd("U:/data/WastIncDatasets")
load("gmsn_inc_NoBirthInc.Rdata")
load("cntt_inc_NoBirthInc.Rdata")
load("gbsc_inc_NoBirthInc.Rdata")
load("cmin_inc_NoBirthInc.Rdata")
load("phua_inc_NoBirthInc.Rdata")
load("tzc2_inc_NoBirthInc.Rdata")
load("cmc_inc_NoBirthInc.Rdata")
load("ee_inc_NoBirthInc.Rdata")
load("irc_inc_NoBirthInc.Rdata")
load("tdc_inc_NoBirthInc.Rdata")
load("rspk_inc_NoBirthInc.Rdata")
load("mled_inc_NoBirthInc.Rdata")


mean(phua_inc$wast_rec90d, na.rm=T)
mean(phua_inc$wast_rec90d[phua_inc$wast_inc==1], na.rm=T)

#Define set of short ID's I care about
datasets <- list(
  gmsn_inc, 
  cntt_inc, 
  gbsc_inc, 
  cmin_inc_brazil, 
  cmin_inc_guinea_bissau, 
  cmin_inc_peru, 
  cmin_inc_bangladesh,   
  phua_inc, 
  tzc2_inc, 
  cmc_inc, 
  ee_inc, 
  irc_inc,
  tdc_inc,
  rspk_inc, 
  mled_inc_bangladesh,
  mled_inc_brazil,             
  mled_inc_india,
  mled_inc_nepal,
  mled_inc_peru,
  mled_inc_southafrica, 
  mled_inc_tanzania)


#Make list of summary tables
tablelist <- list(
  gmsn_inc_table, 
  cntt_inc_table, 
  gbsc_inc_table, 
  cmin_inc_table_brazil, 
  cmin_inc_table_guinea_bissau, 
  cmin_inc_table_peru, 
  cmin_inc_table_bangladesh,   
  phua_inc_table, 
  tzc2_inc_table, 
  cmc_inc_table, 
  ee_inc_table, 
  irc_inc_table,
  tdc_inc_table,
  rspk_inc_table, 
  mled_inc_table_bangladesh,
  mled_inc_table_brazil,             
  mled_inc_table_india,
  mled_inc_table_nepal,
  mled_inc_table_peru,
  mled_inc_table_southafrica,
  mled_inc_table_tanzania)




#Define the corresponding study names
studynames <- c("GMS-Nepal (Growth Monitoring Study, Nepal)",
                "CNTT (Evaluation and Control of Neglected Mucosal Enteric Infections in Childhood)",
                "GBSC (Longitudinal study of Bovine Serum Concentrate in Guatemala)",
                "CMIN (Child Malnutrition and Infection Network): Brazil",
                "CMIN (Child Malnutrition and Infection Network): Guinea Bissau",
                "CMIN (Child Malnutrition and Infection Network): Peru",
                "CMIN (Child Malnutrition and Infection Network): Bangladesh",
                "PHUA (Infant Growth in Peru)",
                "TZC2 (Tanzania Child 2)",
                "CMC Vellore Birth Cohort 2002",
                "EE (Pakistan Study of Biomarkers for Environmental Enteropathy)",
                "IRC (Vellore Crypto Study)",
                "TDC (Vellore Bottled Water BC)",
                "RSPK (Respiratory Pathogens Birth Cohort)",
                "MAL-ED Study: Bangladesh",
                "MAL-ED Study: Brazil",
                "MAL-ED Study: India",
                "MAL-ED Study: Nepal",
                "MAL-ED Study: Peru",
                "MAL-ED Study: South Africa",
                "MAL-ED Study: Tanzania"
)






d<-NULL
for(i in 1:length(datasets)){
  cat(datasets[[i]]$STUDYID[1], " ", datasets[[i]]$COUNTRY[1])
  temp<-cbind(rep(datasets[[i]]$STUDYID[1], nrow(tablelist[[i]]$means)), rep(datasets[[i]]$COUNTRY[1],nrow(tablelist[[i]]$means)), tablelist[[i]]$means)
  d<-rbind(d,temp)
}  
colnames(d)[1:2]<-c("STUDYID","COUNTRY")

#Create unique country/study combination
d$STUDYID<-gsub("^k.*?-","",d$STUDYID)
d$COUNTRY<-as.character(d$COUNTRY)
d$COUNTRY[d$COUNTRY=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
d$country_cohort<-paste0(d$STUDYID, " ", d$COUNTRY)

#Group by region
table(d$COUNTRY)

d$region <- NA
d$region[d$COUNTRY=="TANZANIA"|d$COUNTRY=="SOUTH AFRICA"|d$COUNTRY=="GUINEA-BISSAU"]<-2
d$region[d$COUNTRY=="BANGLADESH"|d$COUNTRY=="PAKISTAN"|d$COUNTRY=="INDIA"|d$COUNTRY=="NEPAL"]<-1
d$region[d$COUNTRY=="GUATEMALA"|d$COUNTRY=="PERU"|d$COUNTRY=="BRAZIL"]<-3

d <- d %>% arrange(region)
d$country_cohort<-factor(d$country_cohort, levels=unique(d$country_cohort))


#Drop out MLED PAkistan (bad anthropometry measures) and CMIN Brazil (not monthly)
d <- d[d$country_cohort!="MAL-ED PAKISTAN" & d$country_cohort!="CMIN BRAZIL",] 

#Save summary statistics list to create pooled estimates
setwd("U:/results/Wasting Descriptive Epi")
save(d, file="descriptive_epi_mean_monthly_cohorts_noBW.Rdata")


