



#http://www.reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
#https://stackoverflow.com/questions/30422008/r-knitr-pdf-is-there-a-posssibility-to-automatically-save-pdf-reports-generate

# References for automation 
# http://www.r-bloggers.com/how-to-source-an-r-script-automatically-on-a-mac-using-automator-and-ical/
# http://www.engadget.com/2013/03/18/triggering-applescripts-from-calendar-alerts-in-mountain-lion/

# File 1: Should be an R-Script 
# contains a loop that iteratively calls an Rmarkdown file (i.e. File 2)

# load packages
rm(list=ls())
library(knitr)
library(markdown)
library(rmarkdown)
library(ggplot2)


setwd("U:/Perminant files/R scripts/Risk factor analysis Rally 4b")
source("4b_RiskFactor_functions.R")

setwd("U:/data/Rally4b_data")


load("akup_inc.Rdata")
load("bfzn_inc.Rdata")
load("cmc_inc.Rdata")
load("cmin_inc.Rdata")
load("cort_inc.Rdata")
load("cntt_inc.Rdata")
load("ee_inc.Rdata")
load("eu_inc.Rdata")
load("gmsn_inc.Rdata")
load("gsto_inc.Rdata")
load("gbsc_inc.Rdata")
load("irc_inc.Rdata")
load("jvt3_inc.Rdata")
load("jvt4_inc.Rdata")
load("knba_inc.Rdata")
load("lcn5_inc.Rdata")
load("mled_inc.Rdata")
load("nbrt_inc.Rdata")
load("prbt_inc.Rdata")
load("phua_inc.Rdata")
load("rspk_inc.Rdata")
load("cmpf_inc.Rdata")
load("fspp_inc.Rdata")
load("tdc_inc.Rdata")
load("tzc2_inc.Rdata")
load("zvit_inc.Rdata")
load("lnsz_inc.Rdata")

load("ncry_inc.Rdata")
load("incp_inc.Rdata")
load("eczn_inc.Rdata")
load("prvd_inc.Rdata")
load("dvds_inc.Rdata")




#Define set of short ID's I care about
shortids <- c(
  "AKUP", "BFZN", "CMC",
  "CMIN-Peru",
  "CMIN-Brazil",
  "CMIN-GuineaBissau",
  "CMIN-Bangladesh",
  "CORT-Brazil", "CORT-Guatemala", "CORT-India", "CORT-Philippines", "CORT-SouthAfrica", 
  "CNTT", "EE", "EU", "GMSN", "GSTO", "GBSC", "IRC", "JVT3", "JVT4", "KNBA", "LCN5", 
  "MLED-Bangladesh",
  "MLED-Brazil",
  "MLED-India",
  "MLED-Nepal",
  "MLED-Peru",
  "MLED-Pakistan",
  "MLED-SouthAfrica",
  "MLED-Tanzania",
  "NBRT", "PRBT", "PHUA", "RSPK", "CMPF", "FSPP", "TDC", "TZC2", "ZVIT", "LNSZ",
  "NCRY", "INCP", "ECZN", "PRVD", "DVDS"
)



#List of datasets
datasets <- list(
  akup_inc, bfzn_inc, cmc_inc, 
  cmin_inc_peru, 
  cmin_inc_brazil,
  cmin_inc_guinea_bissau,
  cmin_inc_bangladesh,
  cort_inc_brazil,
  cort_inc_guatemala,
  cort_inc_india,
  cort_inc_philippines,
  cort_inc_southafrica,
  cntt_inc, ee_inc, eu_inc, gmsn_inc, gsto_inc, gbsc_inc, 
  irc_inc, jvt3_inc, jvt4_inc, knba_inc, lcn5_inc, 
  mled_inc_bangladesh,
  mled_inc_brazil,
  mled_inc_india,
  mled_inc_nepal,
  mled_inc_peru,
  mled_inc_pakistan,
  mled_inc_southafrica,
  mled_inc_tanzania,
  nbrt_inc, prbt_inc, phua_inc, 
  rspk_inc, cmpf_inc, fspp_inc, 
  tdc_inc, tzc2_inc, zvit_inc, lnsz_inc,
  ncry_inc,incp_inc,eczn_inc,prvd_inc,dvds_inc)




#Make list of summary tables
tablelist<- list(
  akup_inc_table, bfzn_inc_table, cmc_inc_table, 
  cmin_inc_table_peru,
  cmin_inc_table_brazil,
  cmin_inc_table_guinea_bissau,
  cmin_inc_table_bangladesh,
  cort_inc_table_brazil,
  cort_inc_table_guatemala,
  cort_inc_table_india,
  cort_inc_table_philippines,
  cort_inc_table_southafrica,
  cntt_inc_table, ee_inc_table, eu_inc_table, gmsn_inc_table, gsto_inc_table, 
  gbsc_inc_table, irc_inc_table, jvt3_inc_table, jvt4_inc_table, knba_inc_table, 
  lcn5_inc_table, 
  mled_inc_table_bangladesh,
  mled_inc_table_brazil,
  mled_inc_table_india,
  mled_inc_table_nepal,
  mled_inc_table_peru,
  mled_inc_table_pakistan,
  mled_inc_table_southafrica,
  mled_inc_table_tanzania,
  nbrt_inc_table, prbt_inc_table, phua_inc_table, rspk_inc_table, 
  cmpf_inc_table, fspp_inc_table, tdc_inc_table, tzc2_inc_table, 
  zvit_inc_table, lnsz_inc_table,
  ncry_inc_table,incp_inc_table,eczn_inc_table,prvd_inc_table,dvds_inc_table)




#Define the corresponding study names
studynames <- c(
  "akup  PAKISTAN  Aga Khan University Evidence Based Nutrition Intervention Study",       
  "bfzn  BURKINA FASO  Zn Trial in Burkina Faso",                                          
  "cmc  INDIA  CMC Vellore Birth Cohort 2002",                                             
  "cmin  PERU  Child Malnutrition and Infection Network",                                  
  "cmin  BRAZIL  Child Malnutrition and Infection Network",                                
  "cmin  GUINEA-BISSAU  Child Malnutrition and Infection Network",                         
  "cmin  BANGLADESH  Child Malnutrition and Infection Network",                            
  "cort  BRAZIL  COHORTS",                                                                 
  "cort  GUATEMALA  COHORTS",                                                              
  "cort  INDIA  COHORTS",                                                                  
  "cort  PHILIPPINES  COHORTS",                                                            
  "cort  SOUTH AFRICA  COHORTS",                                                           
  "cntt  PERU  Evaluation and Control of Neglected Mucosal Enteric Infections in Childhood",
  "ee  PAKISTAN  Study of Biomarkers for Environmental Enteropathy",                       
  "eu  INDIA  Zn Supp for Diarrheal Pneuomonia",                                           
  "gmsn  Growth Monitoring Study, Nepal",                                           
  "gsto  SINGAPORE  GUSTO",                                                                
  "gbsc  Longitudinal study of BSC in Guatemala",                               
  "irc  INDIA  Vellore Crypto Study",                                                      
  "jvt3  BANGLADESH  JiVitA-3 Study",                                                      
  "jvt4  BANGLADESH  JiVitA-4 Study",                                                      
  "knba  GAMBIA  MRC Kenaba",                                                              
  "lcn5  MALAWI  Lungwena Child Nutrition Intervention Study 5",                           
  "mled  BANGLADESH  MAL-ED Study (PMID25305287)",                                         
  "mled  BRAZIL  MAL-ED Study (PMID25305287)",                                             
  "mled  INDIA  MAL-ED Study (PMID25305287)",                                              
  "mled  NEPAL  MAL-ED Study (PMID25305287)",                                              
  "mled  PERU  MAL-ED Study (PMID25305287)",                                               
  "mled  PAKISTAN  MAL-ED Study (PMID25305287)",                                           
  "mled  SOUTH AFRICA  MAL-ED Study (PMID25305287)",                                       
  "mled  TANZANIA MAL-ED Study (PMID25305287)",                       
  "nbrt  BANGLADESH  NIH Birth Cohort Study",                                              
  "prbt  BELARUS  PROBIT Study",                                                           
  "phua  Infant Growth in Peru",                                                     
  "rspk  PAKISTAN  Respiratory Pathogens Birth Cohort",                                    
  "cmpf  INDIA  Optimal Infant Feeding Practices",                                         
  "fspp  INDIA  Randomized Food Supplementation Trial Ages 4-12 Months",                   
  "tdc  INDIA  Vellore Bottled Water BC",                                                  
  "tzc2 Tanzania Child 2",                                  
  "zvit  ZIMBABWE  ZVITAMBO",                                                              
  "lnsz  BURKINA FASO  iLiNS-Zinc Study",
  "ncry NIH Crypto", "incp INCAP", "eczn Ecuador Zinc Trial", "prvd Provide", "dvds DIVIDS")




# render allows the rmarkdown to still have access to the global environment, 
#so I just need to define the variables I want read in within the for-loop
for(i in 1:length(datasets)){
  study <- studynames[i]
  d <- datasets[[i]]
  tables <- tablelist[[i]]
  
  try(
    rmarkdown::render('U:/Perminant files/R scripts/Risk factor analysis Rally 4b/MarkdownTemplate.Rmd',  
                      output_file =  paste("report_",shortids[i],'_', Sys.Date(), ".html", sep=''), 
                      output_dir = 'U:/Perminant files/Results/Rally4b_results')
  )}



