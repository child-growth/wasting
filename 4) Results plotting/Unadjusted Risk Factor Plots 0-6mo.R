




#----------------------------------------------------
# Setup
#----------------------------------------------------

rm(list=ls())
library(tidyverse)
library("ggplot2")
library("ggthemes")
library("metafor")

#Plot themes
theme_set(theme_bw())

#Forest plot Color palette
cbPalette <- c( "#56B4E9" , rep("#999999",40))

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


#Load data
setwd("C:/Users/andre/Documents/HBGDki/Figures/Risk Factor Analysis")
load("C:/Users/andre/Documents/HBGDki/Results/Risk_Factor_Results_wastinc0-6.Rdata")


#Drop trisls with only yearly measurements
for(i in 1:length(wastinc_06_unadj)){
  wastinc_06_unadj[[i]] <- wastinc_06_unadj[[i]] %>% filter(
    STUDYID != "ki1000110-WASH-Bangladesh" &
    STUDYID != "ki1000111-WASH-Kenya" &
    STUDYID != "kiGH5241-JiVitA-3" &
    STUDYID != "ki1148112-iLiNS-DOSE" &
    STUDYID != "ki1148112-iLiNS-DYAD-M" &
    STUDYID != "ki1000304-ZnMort" )
}





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
    
    fit<-NULL
    tryCatch({fit<-rma(yi=logRR.psi, vi=logRR.var, data=temp, method=method, measure="RR")},
           error=function(e){cat("ERROR : REML did not converge, trying ML \n")})
    if(is.null(fit)){fit<-rma(yi=logRR.psi, vi=logRR.var, data=temp, method="ML", measure="RR")}
    
  
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
  d$level <- factor(d$level)
  
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
# Clean and plot risk factor results, saving the pooled estimates
#----------------------------------------------------

REpooled_wastinc06_unadj <- NULL

setwd("C:/Users/andre/Documents/HBGDki/Figures/Risk Factor Analysis")

pdf("Unadjusted Risk Factor Plots Wasting CI 0-6.pdf", height=10, width=8)

for(i in 1:length(wastinc_06_unadj)){
  if(nrow(wastinc_06_unadj[[i]])>0 & 
     sum(is.na(wastinc_06_unadj[[i]]$RR),na.rm=T) != nrow(wastinc_06_unadj[[i]])
    ){
    
    if(length(unique(wastinc_06_unadj[[i]]$meanLevel))>13){
    plotlevels <- c("Q1","Q2","Q3","Q4")  
    wastinc_06_unadj[[i]]$meanLevel <- wastinc_06_unadj[[i]]$level <- rep(plotlevels, nrow(wastinc_06_unadj[[i]])/4)
    }else{
    plotlevels <- unique(wastinc_06_unadj[[i]]$meanLevel)  
    }
                 
  Anywast <- cleandf(wastinc_06_unadj[[i]], RF_levels=plotlevels)   #c("<=2700","2700-3000","3000-3400",">3400"))
  Anywast_plot <-RRplot_fun(Anywast, 
                               reflevel=wastinc_06_unadj[[i]]$level[1], 
                               title=paste0("Risk factor: ",wastinc_06_unadj[[i]]$variable[1],"\nCumulative incidence ratios:\nAny wasting from 0-6 months age"), 
                               units="",
                               levels = unique(wastinc_06_unadj[[i]]$meanLevel))
  print(Anywast_plot)
  REpooled_wastinc06_unadj<-rbind(REpooled_wastinc06_unadj, Anywast[Anywast$study=="Pooled estimate",])
  }
}

dev.off()



save(REpooled_wastinc06_unadj, file="C:/Users/andre/Documents/HBGDki/Results/REpooled_wastinc06_unadj.Rdata")

#Drop variables that don't make sense to pool
REpooled_wastinc06_unadj <- REpooled_wastinc06_unadj %>% filter(variable!="month" & variable!="birthmonth")

#filter out birthweight
REpooled_wastinc06_unadj <- REpooled_wastinc06_unadj %>% filter(variable!="BIRTHWT")


REpooled_wastinc06_unadj$reflabel <- ""
REpooled_wastinc06_unadj$reflabel[is.na(REpooled_wastinc06_unadj$RR)] <- "(ref.)"
REpooled_wastinc06_unadj$RR[is.na(REpooled_wastinc06_unadj$RR)] <- 1


#FIX to offset (ref.) by a relative amount
REpooled_wastinc06_unadj <- REpooled_wastinc06_unadj %>% group_by(variable) %>% mutate(RRrange=diff(range(RR), na.rm=T))


p<-ggplot(REpooled_wastinc06_unadj, aes(x=level)) + 
  geom_point(aes(y=RR, fill=variable, color=variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=variable),
                 alpha=0.5, size = 3) +
  geom_text(aes( y=0.75, label=reflabel, colour=variable)) +
  labs(x = "Risk factor level", y = "CIR") +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.125,0.25,0.5,1,2,4), trans='log10') +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  scale_size_continuous(range = c(0.5, 1))+
  theme(panel.border = element_blank(), 
    strip.background = element_blank(),
    legend.position="none",
    strip.text.x = element_text(size=8),
    axis.text.x = element_text(size=8)) +
  facet_wrap(~variable,  scales = "free_x") 

ggsave("Unadjusted Risk Factor Plots Wasting CI Pooled 0-6.pdf", p, height=10, width=10)
