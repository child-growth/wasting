





#Get data into printable format and plot graphs



#----------------------------------------------------
# Setup
#----------------------------------------------------

rm(list=ls())
library(tidyverse)
library("ggplot2")
library("ggthemes")
library("metafor")
theme_set(theme_bw())
#setwd("C:/Users/andre/Documents/HBGDwasting/R/andrew")

setwd("C:/Users/andre/Documents/HBGDki/Figures/Risk Factor Analysis")
#load("birthweight_RF_res.Rdata")
load("C:/Users/andre/Documents/HBGDki/Results/Risk_Factor_Results_wastinc0-24.Rdata")




#Color palette
cbPalette <- c( "#56B4E9" , rep("#999999",100))



#----------------------------------------------------
# Function to create fixed or random effects pooled RRs
#----------------------------------------------------

#Fit RE and FE models
meta_fun <- function(res, method, reference=NULL){
  
  #ID reference level
  levels<-levels(res$level)
  res<-res[!is.na(res$logRR.var),]
  if(is.null(reference)){
    reference<- levels[!(levels %in% unique(res$level))]
  }
  require(metafor)
  RMAest<-data.frame(study="Pooled estimate", res$variable[1],reference ,b=NA, se=NA)
  colnames(RMAest)<-c("study","variable","level","logRR.psi","logSE")

  for(j in 1:length(unique(res$level))){
    temp<-res[res$level==unique(res$level)[j],]
  fit<-rma(yi=logRR.psi, vi=logRR.var, data=temp, method=method, measure="RR")
  est<-data.frame(study="Pooled estimate", temp$variable[1],temp$level[1] ,fit$b, fit$se)
  colnames(est)<-c("study","variable","level","logRR.psi","logSE")
  RMAest<-rbind(RMAest, est)
  }
  
RMAest$RR<-exp(RMAest$logRR)
RMAest$RR.CI1<-exp(RMAest$logRR - 1.96 * RMAest$logSE)
RMAest$RR.CI2<-exp(RMAest$logRR + 1.96 * RMAest$logSE)

#rename SE column to var just to allow binding to original results:
#not used again in calculations
colnames(RMAest)[5]<-"logRR.var"
    return(RMAest)
}





#----------------------------------------------------
# Clean results dfs function
#----------------------------------------------------

cleandf <- function(d, meta_method="REML", RF_levels=c("<=2600","2600-3000","3000-3400",">3400")){
  
  d$study <- d$STUDYID
  d$country <- d$COUNTRY
  
  #remove grant identifier
  d$study<- gsub("^k.*?-" , "", d$study)
  
  #create labels for country-specific cohorts
  d$country <- as.character(d$country)
  d$country[d$country=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
  d$study <- paste0(d$study, " ", d$country)
  
  #drop studies with <5 cases in either reference or comparison level
  d <- d %>% filter(is.na(b) | !(b<5 | d<5))
  
  d_meantab <- d %>% subset(.,select=c(study,meanLevel:meanY, mean.CI1, mean.CI2))
  colnames(d_meantab) <- c("Study","Level", "Number in quartile", "Cumulative incidence of wasting", "95% CI-lower", "95% CI-upper")

  d <- d %>% subset(.,select=c(study, variable, level, logRR.psi, logRR.var,
                                               RR, RR.CI1, RR.CI2))
  
  #Estimate and merge in pooled RR's
  d_RE<-meta_fun(d, method=meta_method)
  d<-rbind(d_RE,d) %>% subset(., select=-c(logRR.psi,  logRR.var))

  # Clean dataframes for plotting
  #Sort levels/studies for plot
  rankedRR <- d %>% group_by(study) %>% summarize(maxRR = max(RR, na.rm=T)) %>% arrange(maxRR)
  rankedRR$order <- 1:nrow(rankedRR)
  d <- left_join(d, rankedRR, by="study")
  d$order[d$study=="Pooled estimate"] <- 1
  
  #Drop studies with no estimated RR and order by size of RR and arrange the dataframe
  d$level <- factor(d$level, levels=RF_levels)
  d <- d %>% filter(!(maxRR=="-Inf")) %>% arrange(order, level)
  d$x<-1:nrow(d)
  d$study <- factor(d$study , as.character(unique(d$study )))

  return(d)
}




#----------------------------------------------------
# RR Plot function
#----------------------------------------------------
RRplot_fun <- function(d, reflevel, title, units="", levels=NULL, free_Y=T){
  
  #change names of risk factor levels
  if(!is.null(levels)){
    d$level <- factor(as.numeric(d$level))
    levels(d$level) <- levels
  }
  
  #plot
n <- nrow(d)

plot_df <- d %>% filter(level!=reflevel)
plot_df$size<-ifelse(plot_df$study=="Pooled estimate",1,0.5)

RRplot<-ggplot(data=plot_df) + 
  labs(title = d$variable, x = "Cohort", y = paste0("Risk ratio (reference = ",reflevel," ", units,")")) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.125,0.25,0.5,1,2,4), trans='log10') +
  coord_flip() +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
    scale_size_continuous(range = c(0.5, 1))+
  geom_pointrange( mapping=aes(x=study, y=RR, ymin=RR.CI1, ymax=RR.CI2, colour=study, size=size)) +
    theme(panel.border = element_blank(), 
    strip.background = element_blank())

if(free_Y){
  RRplot<-RRplot + facet_wrap(~level, ncol=1, scales = "free_y") 
}else{
  RRplot<-RRplot + facet_wrap(~level, ncol=1) 
}

RRplot<-RRplot + geom_vline(xintercept=1.5, color="grey20", linetype=2) +
   #geom_label(aes(label="Overall RR", x=1, y=6, size=.1)) + #Need to make smaller
    ggtitle(title) +theme(legend.position="none")#+ guides(colour = guide_legend(reverse=T)) 
return(RRplot)
}




#----------------------------------------------------
# Clean and plot risk factor results
#----------------------------------------------------

Anywast6mo <- cleandf(wastinc_024_unadj$SEX, RF_levels=c("Female","Male"))
sex_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="Female", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Female","Male"))
sex_plot


names(wastinc_024_unadj)
Anywast6mo <- cleandf(wastinc_024_unadj$birthorder, RF_levels=c("firstborn","secondborn","thirdborn+"))
risk_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="firstborn", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Firstborn","Secondborn","Thirdborn or later"))
risk_plot



names(wastinc_024_unadj)
Anywast6mo <- cleandf(wastinc_024_unadj$improved.sanitation, RF_levels=c("0","1"))
risk_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="0", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Unimproved sanitation","Improved sanitation"))
risk_plot




names(wastinc_024_unadj)
Anywast6mo <- cleandf(wastinc_024_unadj$improved.floor, RF_levels=c("0","1"))
risk_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="0", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Unimproved floor","Improved floor"))
risk_plot



names(wastinc_024_unadj)
Anywast6mo <- cleandf(wastinc_024_unadj$HHwealth_quart, RF_levels=c("Wealth Q1","Wealth Q2","Wealth Q3","Wealth Q4"))
wealth_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="Wealth Q1", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Wealth Q1","Wealth Q2","Wealth Q3","Wealth Q4"))
wealth_plot



names(wastinc_024_unadj)
Anywast6mo <- cleandf(wastinc_024_unadj$MBMI, RF_levels=c("<=19.287","19.287-21.453","21.453-23.754",">23.754"))
wealth_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="<=19.287", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Q1","Q2","Q3","Q4"))
wealth_plot


wastinc_024_unadj$MEDUCYRS$level<-rep(c("Q1","Q2","Q3","Q4"),25)
wastinc_024_unadj$MEDUCYRS$reference<-"Q1"
wastinc_024_unadj$MEDUCYRS$level<-as.factor(wastinc_024_unadj$MEDUCYRS$level)
wastinc_024_unadj$MEDUCYRS$reference<-as.factor(wastinc_024_unadj$MEDUCYRS$reference)
Anywast6mo <- cleandf(wastinc_024_unadj$MEDUCYRS, RF_levels=c("Q1","Q2","Q3","Q4"))
wealth_plot <-RRplot_fun(Anywast6mo, 
                             reflevel="<=0", 
                             title="Cumulative incidence ratios:\nAny wasting from 0-24 months age", 
                             units="",
                             levels = c("Q1","Q2","Q3","Q4"))
wealth_plot

