






rm(list=ls())
library(SuperLearner)
library(caret)
library(colorspace) 
library(RColorBrewer) 
library(fields) 
library(MASS)
library(cluster)
library(fields)
library(reshape2)
library(printr)
library(gridExtra)
library(ggthemes)
library(tidyverse)
library(zoo)
library(metafor)
library(washb)
library(tmle)

#Risk factor functions
setwd("U:/Perminant files/R scripts/")
source("HBGDki_function.R")
setwd("U:/Perminant files/R scripts/Risk factor analysis Rally 4b/")
source("4b_RiskFactor_functions.R")
setwd("U:/data/Rally4b_data")

load("nut_int_compiled_datasets.Rdata")

#-----------------------------------------------------
# Overall parameters
#-----------------------------------------------------
Wvars=NULL
SLlibrary="SL.glm"
agerange=c(0,24)
born_not_wast=F



#-----------------------------------------------------
# Subset data by agerange
#-----------------------------------------------------
dfull <- dfull[dfull$AGEDAYS>(agerange[1])*30 &
                 dfull$AGEDAYS<=(agerange[2])*30,]


table(dfull$STUDYID)


#-----------------------------------------------------
#Treatment arm 
#-----------------------------------------------------

table(dfull$ARM)



d <- dfull %>% filter(!is.na(ARM)) 
d<-droplevels(d)

table(d$STUDYID, d$ARM)

unique(d$STUDYID)
unique(d$ARM)

for(i in 1:length(unique(d$STUDYID))){
  temp <- d[d$STUDYID==unique(d$STUDYID)[i],]
  temp <- droplevels(temp)
  cat("\n\n",as.character(unique(temp$STUDYID)), ":\n")
  print(table(temp$ARM))
}



#Replace nondescript intervention names
d$ARM[d$STUDYID=="ki1000125-AgaKhanUniv" & d$ARM=="Intervention"] <-"Education"
d$ARM[d$STUDYID=="ki1000304b-SAS-CompFeed" & d$ARM=="Intervention"] <-"Comp. feeding education"


d$tr <- NA
d$tr[d$ARM=="Control" | d$ARM=="Control (no Zinc)" | d$ARM=="Standard(Control)" | d$ARM=="No intervention" | d$ARM=="Placebo" | d$ARM=="Passive Control" | d$ARM=="no zinc, no copper" 
     | d$ARM=="Iron and folic acid supplementation" | d$ARM=="e.Control" | d$ARM=="Likuni Phala" | d$ARM=="WPC" | d$ARM== "CFC"  | d$ARM=="Placebo nippled + Placebo Oval" | d$ARM=="Iron Folic Acid"] <- "C"

d$tr[d$ARM=="Therapeutic Zinc: 20 mg/day for 10 days" | d$ARM=="3 mg zinc, no copper" | d$ARM=="10 mg zinc, no copper" |  d$ARM=="10 mg zinc, with copper" |  d$ARM=="3 mg zinc, no copper" | 
       d$ARM=="Intermittent Zinc: 10 mg/d for 10 days" | d$ARM=="Preventive Zinc: 7 mg/day" | d$ARM=="Zinc Alone" | d$ARM=="7 mg zinc, no copper" ] <- "Zinc"

d$tr[d$ARM=="a.LNS-Zn0" | d$ARM=="b.LNS-Zn5" | d$ARM=="c.LNS-Zn10" | d$ARM=="d.LNS-TabZn5" | d$ARM=="LNS-20gNoM" | d$ARM=="LNS-20gM" | d$ARM=="LNS-10gM" | d$ARM=="LNS-40gM" | d$ARM=="LNS-40gNoM" | d$ARM=="Nutrition" | d$ARM=="Nutrition + WSH" | d$ARM=="Lipid-based nutrient supplementation"  |  d$ARM=="Plumpy Doz" |  d$ARM=="Milk FS" | d$ARM=="Soy FS"] <- "LNS"

d$tr[d$ARM=="WSH" | d$ARM=="Water" | d$ARM=="Handwashing" | d$ARM=="Sanitation" | d$ARM=="Education" | d$ARM=="Visitation" | d$ARM=="Nutritional counselling" | d$ARM=="Vitamin D"  | 
       d$ARM=="50,000 IU nippled + 400,000 IU Oval" | d$ARM=="Placebo nippled + 400,000 IU Oval" | d$ARM=="50,000 IU nippled + Placebo Oval" | d$ARM=="BSC" | d$ARM=="Comp. feeding education" | d$ARM=="Nutritional counselling" | d$ARM=="Visitation"] <- "Other"

d$tr[d$ARM=="Multiple micronutrient supplementation" | d$ARM=="Multivitamin Alone" | d$ARM=="Zinc + Multivitamin" |  d$ARM=="MNT + WPC" | d$ARM=="MNT + BSC" | d$ARM=="Multiple Micronutrients" ] <- "MMN"

d$tr[d$ARM=="Food supplementation" | d$ARM=="Chickpea" | d$ARM=="Rice Lentil" | d$ARM=="WSB++" ] <- "CF"

d$tr <- factor(d$tr)

table(d$STUDYID, d$tr)

#treatment codes
tr_codes <- d %>% group_by(STUDYID, ARM) %>% slice(1) %>% select(STUDYID, ARM, tr)
  

heatmap_df <- d %>% group_by(STUDYID, tr) %>% summarise(meanWHZ= mean(WHZ))

table(d$tr)
d$tr <- as.numeric(d$tr)
table(d$tr)
table(is.na(d$tr))
d <- d %>% filter(!is.na(WHZ) & !is.na(tr)) 

A="tr"
n.cat=6
reflevel=1
Acuts=c(1.5, 2.5, 3.5, 4.5, 5.5)
Alevels=c("Control","Comp. Feeding", "LNS", "MMN", "Other", "Zinc")
Anywast024mo <- NULL 
wastrec60d_024mo <- NULL 

setwd("U:/Perminant files/R scripts/")
source("HBGDki_function.R")

Anywast024mo <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(riskfactor4b(., 
                                    A=A, 
                                    Wvars=NULL, 
                                    n.cat=n.cat, 
                                    reflevel=reflevel, 
                                    SLlibrary="SL.glm", 
                                    Acuts=Acuts, 
                                    Alevels=Alevels, 
                                    agerange=agerange,
                                    born_not_wast=F,
                                    overall_dist=T, 
                                    run_anywast=T,
                                    run_recovery=F,
                                    run_irr=F) %>% `[[`("Anywast6mo"))))
as.data.frame(Anywast024mo)

sevwastfalt60d_024mo <- d %>% group_by(STUDYID, COUNTRY) %>% 
  mutate(wast_rec60d = sevwast_inc60d) %>% 
  select_groups() %>%
  do(try(as.data.frame(riskfactor4b(., 
                                    A=A, 
                                    Wvars=NULL, 
                                    n.cat=n.cat, 
                                    reflevel=reflevel, 
                                    SLlibrary="SL.glm", 
                                    Acuts=Acuts, 
                                    Alevels=Alevels, 
                                    agerange=agerange,
                                    born_not_wast=F,
                                    overall_dist=T, 
                                    run_anywast=F,
                                    run_recovery=T,
                                    run_irr=F) %>% `[[`("wastrec60d"))))
as.data.frame(sevwastfalt60d_024mo)


wastrec60d_024mo <- d %>% group_by(STUDYID, COUNTRY) %>% select_groups() %>%
  do(try(as.data.frame(riskfactor4b(., 
                                    A=A, 
                                    Wvars=NULL, 
                                    n.cat=n.cat, 
                                    reflevel=reflevel, 
                                    SLlibrary="SL.glm", 
                                    Acuts=Acuts, 
                                    Alevels=Alevels, 
                                    agerange=agerange,
                                    born_not_wast=F,
                                    overall_dist=T, 
                                    run_anywast=F,
                                    run_recovery=T,
                                    run_irr=F) %>% `[[`("wastrec60d"))))
as.data.frame(wastrec60d_024mo)


setwd("U:/Perminant files/Results/Rally4b_results/RiskFactorRes")
save(Anywast024mo, wastrec60d_024mo, sevwastfalt60d_024mo, heatmap_df, tr_codes, file="ARM_IRres.Rdata")





