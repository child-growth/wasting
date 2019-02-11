

rm(list=ls())
library(tidyverse)
library(scatterpie)
library(data.table)

#--------------------------------------------
# Read in .csv file and save as an .rds file
#--------------------------------------------

d<-fread("U:/data/Stunting/Full-compiled-data/FINAL.csv", header = T)



#change names to lower case
colnames(d) <- tolower(colnames(d))
d<-d %>% subset(., select=c(studyid, subjid, country,  agedays, whz, latitude, longitud))
gc()
d <- d %>% filter(agedays <= 24*30.4167)



#Drop studies Vishak added to data product that don't meet inclusion criteria
d <- d %>% filter(studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP")




#--------------------------------------------
# Mark monthly, quarterly, and yearly-measured 
# studies
#--------------------------------------------

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
# drop unrealistic HAZ
#--------------------------------------------
nrow(d)
d = filter(d, whz >= -5 & whz <=5)
nrow(d)

#--------------------------------------------
# order data, create measurement id
#--------------------------------------------
d <- d %>% 
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(measid=seq_along(subjid)) 

# count number of studies
length(names(table(d$studyid)))

# table of studies
table(d$studyid)
table(d$studyid,d$country)

#convert subjid to character for the merge with covariate dataset
d$subjid <- as.character(d$subjid)


#Add region variable
#Add regions
d <- d %>% mutate(region = case_when(
  country=="BANGLADESH" | country=="INDIA"|
    country=="NEPAL" | country=="PAKISTAN"|
    country=="PHILIPPINES"                   ~ "Asia", 
  country=="KENYA"|
    country=="GHANA"|
    country=="BURKINA FASO"|
    country=="GUINEA-BISSAU"|
    country=="MALAWI"|
    country=="SOUTH AFRICA"|
    country=="TANZANIA, UNITED REPUBLIC OF"|
    country=="ZIMBABWE"|
    country=="GAMBIA"                       ~ "Africa",
  country=="BELARUS"                      ~ "Europe",
  country=="BRAZIL" | country=="GUATEMALA" |
    country=="PERU"                         ~ "Latin America",
  TRUE                                    ~ "Other"
))



#Keep monthly and quarterly studies
d <- d %>% filter(measurefreq!="yearly")




#--------------------------------------------
# Subset to get summary data from map
#--------------------------------------------



d$cohort <- paste0(d$studyid,"-",d$country)
df <- d %>% filter(!is.na(whz)) %>% group_by(studyid,cohort) %>% 
  summarise(n=n(),
            `Not wasted`= mean(whz >=-2),
            `Wasted`= mean(whz < (-3)),
            `Moderately wasted`= mean(whz >=-3 & whz < (-2)),
            `Severely wasted`= mean(whz < (-3)),
            lat=mean(latitude),
            long=mean(longitud),
            country=first(country)) 

save(df, file="U:/data/wasting/wast_scattermap_data.Rdata")




