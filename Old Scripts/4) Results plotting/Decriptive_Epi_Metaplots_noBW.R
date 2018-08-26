
# load packages
rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/Scripts/Figure and Table Scripts/HBGDki_plotting_functions.R")
source("C:/Users/andre/Documents/HBGDki/Scripts/Figure and Table Scripts/Meta-analysis functions.R")

setwd("C:/Users/andre/Documents/HBGDki/Results")
load("descriptive_epi_mean_monthly_cohorts_noBW.Rdata")


d<-prep_desc_data(d)


#Set theme and colors
theme_set(theme_bw())
d$stratacol <- "strata"
d$stratacol[d$strata=="Overall"] <- "overall"
d$stratacol[d$pooled==1] <- "pooled"
d$stratacol[d$strata=="Overall" & d$pooled==1] <- "pooled_unstrat"
cbPalette <- c( overall="#56B4E9", strata="#999999" , pooled="#f7a809", pooled_unstrat="#009E73")

#Set plot width and height
w <- 10
h <- 7

#----------------------
# Primary plots
#----------------------


#Set plot directory

setwd("C:/Users/andre/Documents/HBGDki/Figures/Descriptive Epi/Sensitivity-no birth incidence")

#Wasting prevalence
p1 <- desc_epi_metaplot(d, stat="Prevalence\nof\nwasting",
                     ylabel="Wasting longitudinal prevalence",
                     title="Wasting longitudinal prevalencee")
ggsave("WastPrev_metaplot.pdf", p1, width = w, height = h, units = "in")

#Severe wasting prevalence
p2 <- desc_epi_metaplot(d, stat="Prevalence\nof\nsevere\nwasting",
                     ylabel="Severe wasting longitudinal prevalence",
                     title="Severe wasting longitudinal prevalencee")
ggsave("SevWastPrev_metaplot.pdf", p2, width = w, height = h, units = "in")


#Wasting IR
p3 <- desc_epi_metaplot(d, stat="Wasting\nincidence\nrate",
                     ylabel="Wasting incidence rate per 1000 days",
                     title="Wasting incidence rate")
ggsave("WastInc_metaplot.pdf", p3, width = w, height = h, units = "in")

#Sev wasting IR

p4 <- desc_epi_metaplot(d, stat="Severe\nwasting\nrecovery\nincidence\nrate",
                     ylabel="Severe wasting incidence rate per 1000 days",
                     title="Severe wasting incidence rate")
ggsave("SevWastInc_metaplot.pdf", p4, width = w, height = h, units = "in")

#Duration
p5<- desc_epi_metaplot(d, stat="Average\nduration\nof\nwasting",
                     ylabel="Duration of wasting (days)",
                     title="Average duration of wasting")
ggsave("Durationc_metaplot.pdf", p5, width = w, height = h, units = "in")



# 60 day recovery
p6 <- desc_epi_metaplot(d, stat="Percent\nwasting\nrecovered\nin 60 days",
                     ylabel="Percent wasting recovered in 60 days",
                     title="Percent wasting recovered in 60 days")
ggsave("WastRec60_metaplot.pdf", p6, width = w, height = h, units = "in")


#Unstratified comparison of 30,60,90 day recovery
d_unstrat<-d[d$strata=="Overall",]
d_unstrat <- d_unstrat %>% filter(statistic=="Percent\nwasting\nrecovered\nin 30 days" |
                                  statistic=="Percent\nwasting\nrecovered\nin 60 days" |
                                  statistic=="Percent\nwasting\nrecovered\nin 90 days")
d_unstrat$statistic<-as.character(d_unstrat$statistic)
d_unstrat$statistic[d_unstrat$statistic=="Percent\nwasting\nrecovered\nin 30 days"]<-"30 days"
d_unstrat$statistic[d_unstrat$statistic=="Percent\nwasting\nrecovered\nin 60 days"]<-"60 days"
d_unstrat$statistic[d_unstrat$statistic=="Percent\nwasting\nrecovered\nin 90 days"]<-"90 days"
d_unstrat$statistic<-as.factor(d_unstrat$statistic)
d_unstrat$Lower.95.CI <- d_unstrat$Lower.95.CI  * 100
d_unstrat$Mean <- d_unstrat$Mean  * 100
d_unstrat$Upper.95.CI <- d_unstrat$Upper.95.CI  * 100

p7 <- desc_epi_metaplot(d_unstrat, stat=NULL,
                     ylabel="Percent wasting recovered",
                     title="Percent wasting recovered within 30, 60, and 90 days",
                     xlabel="Recovery time")
ggsave("WastRec_unstrat_metaplot.pdf", p7, width = w, height = h, units = "in")




#Unstratified comparison of faltering into severe wasting
d_unstrat<-d[d$strata=="Overall",]
d_unstrat <- d_unstrat %>% filter(statistic=="Percent\nfalter to\nsevere\nwasting\nin 30 days" |
                                  statistic=="Percent\nfalter to\nsevere\nwasting\nin 60 days" |
                                  statistic=="Percent\nfalter to\nsevere\nwasting\nin 90 days")
d_unstrat$statistic<-as.character(d_unstrat$statistic)
d_unstrat$statistic[d_unstrat$statistic=="Percent\nfalter to\nsevere\nwasting\nin 30 days"]<-"30 days"
d_unstrat$statistic[d_unstrat$statistic=="Percent\nfalter to\nsevere\nwasting\nin 60 days"]<-"60 days"
d_unstrat$statistic[d_unstrat$statistic=="Percent\nfalter to\nsevere\nwasting\nin 90 days"]<-"90 days"
d_unstrat$statistic<-as.factor(d_unstrat$statistic)
d_unstrat$Lower.95.CI <- d_unstrat$Lower.95.CI  * 100
d_unstrat$Mean <- d_unstrat$Mean  * 100
d_unstrat$Upper.95.CI <- d_unstrat$Upper.95.CI  * 100

d_unstrat$Mean[d_unstrat$country_cohort=="Content Peru" | d_unstrat$country_cohort=="Mal-ED Nepal" | d_unstrat$country_cohort=="Mal-ED Tanz." | d_unstrat$country_cohort=="Mal-ED Brazil" | d_unstrat$country_cohort=="Mal-ED Peru"]<-NA
d_unstrat$Lower.95.CI[d_unstrat$country_cohort=="Content Peru" | d_unstrat$country_cohort=="Mal-ED Nepal" | d_unstrat$country_cohort=="Mal-ED Tanz." | d_unstrat$country_cohort=="Mal-ED Brazil" | d_unstrat$country_cohort=="Mal-ED Peru"]<-NA
d_unstrat$Upper.95.CI[d_unstrat$country_cohort=="Content Peru" | d_unstrat$country_cohort=="Mal-ED Nepal" | d_unstrat$country_cohort=="Mal-ED Tanz." | d_unstrat$country_cohort=="Mal-ED Brazil" | d_unstrat$country_cohort=="Mal-ED Peru"]<-NA

p8 <- desc_epi_metaplot(d_unstrat, stat=NULL,
                     ylabel="Percent wasting faltered to severe wasting",
                     title="Percent wasting faltered to severe wasting within 30, 60, and 90 days",
                     xlabel="Faltering time")
ggsave("WastFalter_unstrat_metaplot.pdf", p8, width = w, height = h, units = "in")




#Print all descriptive plots together
pdf("Descriptive_epi_plots_noBW.pdf", width=w,height=h, paper="USr")
p1
p2
p3
p4
p5
p6
p7
p8
dev.off()

