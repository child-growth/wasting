
rm(list=ls())
library(tidyverse)
library(metafor)

load("C:/Users/andre/Downloads/sprint_7D_longbow-master (2)/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.rdata")


#load("C:/Users/andre/Downloads/sprint_7D_longbow-master (2)/sprint_7D_longbow-master/unadjusted_binary/unadjusted_binary_results.rdata")

d <- results


head(d)

#Subset to relative risks
d <- d %>% filter(type=="RR")


#Subset to primary outcomes
table(d$agecat)

d <- d %>% filter(agecat=="0-6 months"| agecat=="6 months"| agecat=="6-24 months"| agecat=="24 months")


#Drop enrolled stunted as a RF for stunting
d <- d %>% filter(intervention_variable!="enstunt")


#Drop diarrhea under 6 months
d <- d %>% filter(intervention_variable!="perdiar6")

#Drop water treatment
d <- d %>% filter(intervention_variable!="trth2o")

#Drop Mal-ED Tanzania HHwealth 6-24mo (only has 2 levels)
d <- d %>% filter(!(intervention_variable=="hhwealth_quart" & agecat=="6-24 months" & studyid=="ki0047075b-MAL-ED" & country=="TANZANIA, UNITED REPUBLIC OF"))


head(d)


#Pooled estimate function
poolRR <- function(d){
    #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(N=n())
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$N)
  }else{
  
  fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="REML", measure="RR")

  est<-data.frame(fit$b, fit$se)
  colnames(est)<-c("logRR.psi","logSE")
  
  est$RR<-exp(est$logRR)
  est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
  est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
  
  est$Nstudies <- nstudies$N
  }
  
  return(est)
}

RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
              do(poolRR(.)) %>% as.data.frame()







#Make sure Nstudies is constant across RF levels
RMAest <- RMAest %>% group_by(agecat, intervention_variable) %>%
                     mutate(Nstudies=paste0("N studies: ",max(Nstudies)),
                            Nstudies=ifelse(intervention_level==first(intervention_level),Nstudies,"")) %>% ungroup()

head(RMAest)




#Order factors for plotting
RMAest <- droplevels(RMAest)
RMAest$agecat <- as.character(RMAest$agecat)
RMAest$agecat[RMAest$agecat=="0-6 months"] <- "0-6 month\ncumulative incidence"
RMAest$agecat[RMAest$agecat=="6 months"] <- "6 month prevalence"
RMAest$agecat[RMAest$agecat=="6-24 months"] <- "6-24 month\ncumulative incidence"
RMAest$agecat[RMAest$agecat=="24 months"] <- "24 month prevalence"
RMAest$agecat <- factor(RMAest$agecat, levels = c("0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence"))

unique(RMAest$intervention_level)
RMAest$intervention_level <- factor(RMAest$intervention_level, 
  levels=c("0","1",
  "<259","[259-273)","[273-287)",">=287",
  "<-3","[-3--2)","[-2--1)","[-1-0)",">=0",
  "<18.5", "[18.5-25)",
  "<20","<25","[20-25)","[25-30)",">=30","[30-35)",">=35",
  "<145","[145-150)","[150-155)","[155-160)",">=160",
  "<42.5","[42.5-50)","[50-57.5)",">=57.5",
  "<160","[160-170)",">=170",
  "3 or less", "4-5","6-7", "8+",
  "2","3","4+",
  "3+",
  "Wealth Q1","Wealth Q2","Wealth Q3","Wealth Q4",
  "Q1","Q2","Q3","Q4",
  "Food Secure","Moderately Food Insecure","Mildly Food Insecure","Severely Food Insecure"))

unique(RMAest$intervention_variable)
RMAest$intervention_variable <- factor(RMAest$intervention_variable,
                                       levels=c("birthlen","birthwt", "gagebrth",
                                                "hdlvry","vagbrth",
                                                "enwast","anywast06","pers_wast",
                                                "earlybf","predexfd6","perdiar24",
                                                "mage","fage","mhtcm","fhtcm",
                                                "mwtkg","mbmi","single",
                                                "meducyrs","feducyrs",
                                                "parity",
                                                "nchldlt5","nhh","nrooms",
                                                "hhwealth_quart","hfoodsec",
                                                "impsan","safeh20",#"trth2o",
                                                "impfloor","cleanck"))


#Add variable labels
unique(RMAest$intervention_variable)

RMAest$RFlabel <- NA
RMAest$RFlabel[RMAest$intervention_variable=="enwast"] <-  "Enrolled wasted"
RMAest$RFlabel[RMAest$intervention_variable=="gagebrth"] <-  "Gestational age at birth"
RMAest$RFlabel[RMAest$intervention_variable=="predexfd6"] <-  "Exclusive or Predominant breastfeed under 6 months"
RMAest$RFlabel[RMAest$intervention_variable=="mage"] <- "Mother's age" 
RMAest$RFlabel[RMAest$intervention_variable=="mhtcm"] <- "Mother's height" 
RMAest$RFlabel[RMAest$intervention_variable=="mwtkg"] <- "Mother's weight" 
RMAest$RFlabel[RMAest$intervention_variable=="mbmi"] <- "Mother's BMI" 
RMAest$RFlabel[RMAest$intervention_variable=="meducyrs"] <- "Mother's education" 
RMAest$RFlabel[RMAest$intervention_variable=="parity"] <-  "Birth order" 
RMAest$RFlabel[RMAest$intervention_variable=="hfoodsec"] <- "Household food security" 
RMAest$RFlabel[RMAest$intervention_variable=="nchldlt5"] <-   "Number of children <5 in household"
RMAest$RFlabel[RMAest$intervention_variable=="hhwealth_quart"] <-  "Household wealth" 
RMAest$RFlabel[RMAest$intervention_variable=="fage"] <- "Father's age" 
RMAest$RFlabel[RMAest$intervention_variable=="fhtcm"] <- "Father's height" 
RMAest$RFlabel[RMAest$intervention_variable=="birthwt"] <- "Birthweight (Z-scored)" 
RMAest$RFlabel[RMAest$intervention_variable=="birthlen"] <- "Birth length (Z-scored)" 
RMAest$RFlabel[RMAest$intervention_variable=="vagbrth"] <- "Vaginal birth" 
RMAest$RFlabel[RMAest$intervention_variable=="hdlvry"] <- "Child delivered at home" 
RMAest$RFlabel[RMAest$intervention_variable=="single"] <- "Single parent" 
RMAest$RFlabel[RMAest$intervention_variable=="nrooms"] <- "Number of rooms in household" 
RMAest$RFlabel[RMAest$intervention_variable=="nhh"] <- "Number of people in household" 
RMAest$RFlabel[RMAest$intervention_variable=="meducyrs"] <- "Maternal education quartile" 
RMAest$RFlabel[RMAest$intervention_variable=="feducyrs"] <- "Paternal education quartile" 
RMAest$RFlabel[RMAest$intervention_variable=="anywast06"] <- "Any wasting before 6 months age" 
RMAest$RFlabel[RMAest$intervention_variable=="pers_wast"] <- "Persistent wasting before 6 months age" 
RMAest$RFlabel[RMAest$intervention_variable=="trth2o"] <- "Treats drinking water" 
RMAest$RFlabel[RMAest$intervention_variable=="cleanck"] <- "Clean cooking fuel usage" 
RMAest$RFlabel[RMAest$intervention_variable=="impfloor"] <- "Improved floor" 
RMAest$RFlabel[RMAest$intervention_variable=="impsan"] <- "Improved sanitation" 
RMAest$RFlabel[RMAest$intervention_variable=="safeh20"] <- "Safe water source" 
RMAest$RFlabel[RMAest$intervention_variable=="perdiar6"] <- "Quartile of diarrhea longitudinal\nprevalence under 6 months" 
RMAest$RFlabel[RMAest$intervention_variable=="perdiar24"] <- "Quartile of diarrhea longitudinal\nprevalence under 24 months" 
RMAest$RFlabel[RMAest$intervention_variable=="earlybf"] <- "Breastfeed within an hour of birth" 

#Print plots across all intervention arms
yticks <- c(0.125,0.25,0.5,1,2,4,8,16)
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/")
pdf("Risk Factor Plots Stunting.pdf", height=8, width=12)

j<-0
for(i in levels(RMAest$intervention_variable)){
  j<-j+1
  plotdf <- RMAest[RMAest$intervention_variable==i,]  
    
  p <-  ggplot(plotdf, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                       alpha=0.5, size = 3) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
          geom_text(aes(x=0.75, y=ceiling(max(plotdf$RR.CI2)), label=Nstudies), size=3,  hjust=0) +
        #coord_cartesian(ylim=range(yticks)) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~agecat, nrow = 1) +
        ggtitle(plotdf$RFlabel[1])
  ggsave(p, file=paste0(j,"_",i,"_RFplot.png"), height=8, width=12)
  ggsave(p, file=paste0("Powerpoint size/",j,"_",i,"_RFplot.png"), height=6.5, width=9.5)
  print(p)
}

dev.off()



# Create plots with just the CI outcomes

RMAestCI <- RMAest %>% filter(agecat!="6 month prevalence" & agecat!="24 month prevalence")

j<-0
for(i in levels(RMAestCI$intervention_variable)){
  j<-j+1
  plotdf <- RMAestCI[RMAestCI$intervention_variable==i,]  
    
  p <-  ggplot(plotdf, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                       alpha=0.5, size = 3) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
          geom_text(aes(x=0.75, y=ceiling(max(plotdf$RR.CI2)), label=Nstudies), size=3,  hjust=0) +
        #coord_cartesian(ylim=range(yticks)) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~agecat, nrow = 1) +
        ggtitle(plotdf$RFlabel[1])
  ggsave(p, file=paste0("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/CI only/",j,"_",i,"_RFplot.pdf"), height=8, width=12)
}






#Create 1 summary plot with the largest RR
sumdf <- RMAest %>% ungroup() %>% 
  group_by(intervention_variable) %>% 
  mutate(maxRR=max(abs(1-RR))) %>% 
  filter(maxRR==abs(1-RR)) %>% ungroup() %>%
  arrange(RR) %>%
  mutate(order=row_number())

sumdf$intervention_variable <- as.character(sumdf$intervention_variable)
sumdf$intervention_variable <- factor(sumdf$intervention_variable, levels=unique(sumdf$intervention_variable))

yticks <- c(0.125,0.25,0.5,1,2,4,8,16,32,64)


p <- ggplot(sumdf, aes(x=intervention_variable)) + 
        geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                       alpha=0.5, size = 3) +
        labs(x = "Risk factor", y = "Relative risk") +
        geom_hline(yintercept = 1) +
          scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        ggtitle("")

print(p) 

