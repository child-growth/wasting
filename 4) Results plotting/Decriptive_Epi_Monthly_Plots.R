


# load packages
rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/Scripts/Figure and Table Scripts/HBGDki_plotting_functions.R")
source("C:/Users/andre/Documents/HBGDki/Scripts/Figure and Table Scripts/Meta-analysis functions.R")


setwd("C:/Users/andre/Documents/HBGDki/Results")
load("WastInc_res_monthStrat.Rdata")


d <- means[means$strata=="Overall",]
rm(means)


  #remove grant identifier
  d$country_cohort<- gsub("^k.*?-" , "", d$country_cohort)
  

#subset to asian countries
d<-d[grepl("PAKISTAN",d$country_cohort) | grepl("INDIA",d$country_cohort) | grepl("NEPAL",d$country_cohort) | grepl("BANGLADESH",d$country_cohort),]


#Set theme and colors
theme_set(theme_bw())
d$stratacol <- "strata"
d$stratacol[d$strata=="Overall"] <- "overall"
d$stratacol[d$pooled==1] <- "pooled"
d$stratacol[d$strata=="Overall" & d$pooled==1] <- "pooled_unstrat"
cbPalette <- c( overall="#56B4E9", strata="#999999" , pooled="#f7a809", pooled_unstrat="#009E73")

#Set plot width and height
w <- 10
h <- 3.5


d$Mean[d$statistic=="Prevalence\nof\nwasting"] <- d$Mean[d$statistic=="Prevalence\nof\nwasting"] * 100
d$Lower.95.CI[d$statistic=="Prevalence\nof\nwasting"] <- d$Lower.95.CI[d$statistic=="Prevalence\nof\nwasting"] * 100
d$Upper.95.CI[d$statistic=="Prevalence\nof\nwasting"] <- d$Upper.95.CI[d$statistic=="Prevalence\nof\nwasting"] * 100


setwd("C:/Users/andre/Documents/HBGDki/Figures/Descriptive Epi/Seasonality figures")

pdf("Descriptive_epi_plots_monthly.pdf", width=w,height=h, paper="USr")

 for(i in 1:length(unique(d$statistic))){
                              
  stat <- unique(d$statistic)[i]
                              
  p <- ggplot(d[d$statistic==stat,]) + 
              geom_point(aes(x=month, y=Mean, fill=stratacol, color=stratacol), size = 4) +
              geom_linerange(aes(x=month, ymin = Lower.95.CI, ymax = Upper.95.CI, color=stratacol), 
                 alpha=0.5, size = 3) +
  facet_wrap(~country_cohort, nrow = 1)  + 
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=8),
        axis.text.x = element_text(size=8)) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12))+
  ylab(stat)+
  ggtitle(stat) + 
  xlab("Month") 
  
  if(i==4){
    p <- p + coord_cartesian(ylim=c(0,2))
  }
  
print(p)

}

dev.off()



