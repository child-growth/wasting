
rm(list=ls())
library(tidyverse)
library(metafor)


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")




#load data
load("C:/Users/andre/Documents/HBGDki/Results/intervention_results.rdata")
head(results)

table(results$safeh20)

d <- results


#Drop covariates that are not pre-specified EM variables
d <- d %>% filter(is.na(birthwt) & is.na(birthlen) &                      
            is.na(vagbrth) & is.na(hdlvry) &  is.na(mwtkg) &                            
            is.na(single) & is.na(fage) & is.na(fhtcm) &                
            is.na(nrooms) & is.na(nhh) &                            
             is.na(month) & is.na(brthmon) &              
             is.na(feducyrs) &  is.na(anywast06) &            
            is.na(pers_wast) & is.na(cleanck) & is.na(impfloor) &             
            is.na(impsan) & is.na(safeh20) & is.na(perdiar24) &            
             is.na(trth2o) & is.na(perdiar6))


#Create effect modifier and effect modifier levels variables

EM <- d %>% 
  select("sex", 
       "enstunt", 
       "enwast", 
       "gagebrth", 
       "predexfd6", 
       "mage", 
       "mhtcm", 
       "mbmi", 
       "meducyrs", 
       "parity", 
       "hfoodsec", 
       "nchldlt5", 
       "hhwealth_quart") %>% 
  mutate_all(as.character) 
EM$EMvar <- NA
EM$EMvar[!is.na(EM$sex)] <- "sex" 
EM$EMvar[!is.na(EM$enstunt)] <- "enstunt" 
EM$EMvar[!is.na(EM$enwast)] <- "enwast" 
EM$EMvar[!is.na(EM$gagebrth)] <- "gagebrth" 
EM$EMvar[!is.na(EM$predexfd6)] <- "predexfd6" 
EM$EMvar[!is.na(EM$mage)] <- "mage" 
EM$EMvar[!is.na(EM$mhtcm)] <- "mhtcm" 
EM$EMvar[!is.na(EM$mbmi)] <- "mbmi" 
EM$EMvar[!is.na(EM$meducyrs)] <- "meducyrs" 
EM$EMvar[!is.na(EM$parity)] <- "parity" 
EM$EMvar[!is.na(EM$hfoodsec)] <- "hfoodsec" 
EM$EMvar[!is.na(EM$nchldlt5)] <- "nchldlt5" 
EM$EMvar[!is.na(EM$hhwealth_quart)] <- "hhwealth_quart" 

table(EM$EMvar)
table(is.na(EM$EMvar))
EM$EMvar[is.na(EM$EMvar)] <- "unstratified" 





EM[is.na(EM)] <-""

EM$EMlevel <- paste0(EM[,1],EM[,2],EM[,3],EM[,4],EM[,5],EM[,6],EM[,7],EM[,8],EM[,9],EM[,10],EM[,11],EM[,12],EM[,13])
table(EM$EMlevel)

EM <- EM %>% select(EMvar, EMlevel)

unique(paste0(EM$EMvar," ", EM$EMlevel))
unique(EM$EMlevel)

EM$EMlevel <- factor(EM$EMlevel, levels = 
c("Female", "Male","<259", "[259-273)", "[273-287)",">=287", "0",                       
"1", "<20","[20-25)", "<18.5", "[18.5-25)","[25-30)", ">=30",                    
"<145","[145-150)",               
"[150-155)","[155-160)", ">=160",
"2","3+",                      
"Wealth Q1","Wealth Q2","Wealth Q3", "Wealth Q4",
"Q1","Q2","Q3","Q4",
"Food Secure","Mildly Food Insecure",    
"Moderately Food Insecure", "Severely Food Insecure"  ))

d <- d %>% subset(., select= -c(sex, 
                                 enstunt, 
                                 enwast, 
                                 gagebrth, 
                                 predexfd6, 
                                 mage, 
                                 mhtcm, 
                                 mbmi, 
                                 meducyrs, 
                                 parity, 
                                 hfoodsec, 
                                 nchldlt5, 
                                 hhwealth_quart))
d <- data.frame(d, EM)

#subset to RR estimates
d <- d %>% filter(type=="RR" & intervention_level!="Control") %>% 
           filter(untransformed_se!=0)

#Drop "Other" arms
d <- d %>% filter(intervention_level!="Other")


#Add variable labels
d$EMvarlabel <- NA
d$EMvarlabel[d$EMvar=="unstratified"] <-  "Unstratified intervention effect"
d$EMvarlabel[d$EMvar=="sex"] <-  "Child gender"
d$EMvarlabel[d$EMvar=="enstunt"] <-  "Enrolled stunted"
d$EMvarlabel[d$EMvar=="enwast"] <-  "Enrolled wasted"
d$EMvarlabel[d$EMvar=="gagebrth"] <-  "Gestational age at birth"
d$EMvarlabel[d$EMvar=="predexfd6"] <-  "Exclusive or Predominant breastfeed under 6 months"
d$EMvarlabel[d$EMvar=="mage"] <- "Mother's age" 
d$EMvarlabel[d$EMvar=="mhtcm"] <- "Mother's height" 
d$EMvarlabel[d$EMvar=="mbmi"] <- "Mother's BMI" 
d$EMvarlabel[d$EMvar=="meducyrs"] <- "Mother's education" 
d$EMvarlabel[d$EMvar=="parity"] <-  "Birth order" 
d$EMvarlabel[d$EMvar=="hfoodsec"] <- "Household food security" 
d$EMvarlabel[d$EMvar=="nchldlt5"] <-   "Number of children <5"
d$EMvarlabel[d$EMvar=="hhwealth_quart"] <-  "Household wealth" 


#Subset to data of interest
unique(d$outcome_variable)

dp <- d %>% filter(outcome_variable=="stunted")
d <- d %>% filter(outcome_variable=="ever_stunted")





#Pool results across studies
# 
# RMAest <- NULL
# for(i in 1:length(unique(d$EMvar))){
#   
#   predf <- d[d$EMvar==unique(d$EMvar)[i],]
#   predf <- droplevels(predf)
#   
#   for(j in 1:length(unique(predf$EMlevel))){
#     predf2 <- predf[predf$EMlevel==unique(predf$EMlevel)[j],]
# 
#     for(k in 1:length(unique(predf2$intervention_level))){
#       predf3 <- predf2[predf2$intervention_level==unique(predf$intervention_level)[j],]
#       
#       for(l in 1:length(unique(predf3$agecat))){
#       temp <- predf3[predf3$agecat==unique(predf3$agecat)[l],] 
#      
#   
#   fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=temp, method="ML", measure="RR")
# 
#   est<-data.frame(study="Pooled estimate", temp$EMvar[1],unique(predf$EMlevel)[j] ,fit$b, fit$se)
#   colnames(est)<-c("study","variable","level","logRR.psi","logSE")
#   RMAest<-rbind(RMAest, est)
#        }
#      }
#   }
# }
# 
# RMAest$RR<-exp(RMAest$logRR)
# RMAest$RR.CI1<-exp(RMAest$logRR - 1.96 * RMAest$logSE)
# RMAest$RR.CI2<-exp(RMAest$logRR + 1.96 * RMAest$logSE)


#NOTE! Maybe make a group_by and do() function instead of 4 for loops




poolRR <- function(d){
  
  #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(N=n())
  
  fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="REML", measure="RR")

  est<-data.frame(fit$b, fit$se)
  colnames(est)<-c("logRR.psi","logSE")
  
  est$RR<-exp(est$logRR)
  est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
  est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
  
  est$Nstudies <- nstudies$N
  est$EMvarlabel <- d$EMvarlabel[1]
  return(est)
}

RMAest <- d %>% group_by(EMvar, agecat, intervention_level, EMlevel) %>%
              do(poolRR(.)) %>% as.data.frame()

#Make sure Nstudies is constant across EM levels
RMAest <- RMAest %>% group_by(EMvar, agecat, intervention_level) %>%
                     mutate(Nstudies=paste0("N studies: ",max(Nstudies)),
                            Nstudies=ifelse(EMlevel==first(EMlevel),Nstudies,"")) %>% ungroup()
                     #mutate(Nstudies=ifelse(Nstudies==max(Nstudies),paste0("N studies: ",max(Nstudies)),"")) %>% ungroup()

head(RMAest)


#Get number of studies for the unstratified plot
RMAest_overall <- d[d$EMvar=="unstratified",] %>% group_by(EMvar, agecat, intervention_level) %>%
              do(poolRR(.)) %>% mutate(Nstudies=paste0("N studies: ",max(Nstudies))) %>% as.data.frame()
RMAest_overall
RMAest <- RMAest[RMAest$EMvar!="unstratified",] 
RMAest <- bind_rows(RMAest_overall, RMAest)


#Order factors for plotting
RMAest$agecat <- as.character(RMAest$agecat)
RMAest$agecat <- factor(RMAest$agecat, levels = c("0-6 months", "6-24 months", "0-24 months"))
RMAest$intervention_level <- factor(RMAest$intervention_level, levels = c("Zinc", "Maternal", "LNS", "VitA"))


unique(RMAest$EMvar)
unique(RMAest$EMlevel)

#Print plots across all intervention arms
yticks <- c(0.125,0.25,0.5,1,2,4)
scaleFUN <- function(x) sprintf("%.2f", x)

pdf("C:/Users/andre/Dropbox/HBGDki figures/Nutritional Intervention Analysis/Intervention Plots Stunting CI.pdf", height=9, width=9)

for(i in c("unstratified", "sex", "enstunt","enwast","gagebrth", "hfoodsec",
          "hhwealth_quart", "mage","mbmi","meducyrs",      
          "mhtcm","predexfd6", "nchldlt5","parity")){

p <-  ggplot(RMAest[RMAest$EMvar==i,], aes(x=EMlevel)) + 
      geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
      geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
                     alpha=0.5, size = 3) +
      labs(x = "Effect modifier level", y = "Cumulative Incidence Ratio") +
      geom_hline(yintercept = 1) +
        geom_text(aes(x=0.75, y=4, label=Nstudies), size=3,  hjust=0) +
      #coord_cartesian(ylim=range(yticks)) +
      scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
      scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$intervention_level)) +
      scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$intervention_level)) +
      scale_size_continuous(range = c(0.5, 1))+
      theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
      facet_grid(agecat~intervention_level) +
      ggtitle(RMAest$EMvarlabel[RMAest$EMvar==i][1])
print(p)
    }

dev.off()








#Prevalence outcome

RMAestp <- dp %>% group_by(EMvar, agecat, intervention_level, EMlevel) %>%
              do(poolRR(.)) %>% as.data.frame()

#Make sure Nstudies is constant across EM levels
RMAestp <- RMAestp %>% group_by(EMvar, agecat, intervention_level) %>%
                     mutate(Nstudies=paste0("N studies: ",max(Nstudies)),
                            Nstudies=ifelse(EMlevel==first(EMlevel),Nstudies,"")) %>% ungroup()
                     #mutate(Nstudies=ifelse(Nstudies==max(Nstudies),paste0("N studies: ",max(Nstudies)),"")) %>% ungroup()

head(RMAestp)


#Get number of studies for the unstratified plot
RMAest_overallp <- dp[dp$EMvar=="unstratified",] %>% group_by(EMvar, agecat, intervention_level) %>%
              do(poolRR(.)) %>% mutate(Nstudies=paste0("N studies: ",max(Nstudies))) %>% as.data.frame()
RMAest_overallp
RMAestp <- RMAestp[RMAestp$EMvar!="unstratified",] 
RMAestp <- bind_rows(RMAest_overallp, RMAestp)



#Order factors for plotting
RMAestp$agecat <- as.character(RMAestp$agecat)
unique(RMAestp$agecat)
RMAestp$agecat <- factor(RMAestp$agecat, levels = c("Birth", "6 months", "24 months"))
RMAestp$intervention_level <- factor(RMAestp$intervention_level, levels = c("Zinc", "Maternal", "LNS", "VitA"))


unique(RMAestp$EMvar)
unique(RMAestp$EMlevel)

#Print plots across all intervention arms
yticks <- c(0.125,0.25,0.5,1,2,4)
scaleFUN <- function(x) sprintf("%.2f", x)

pdf("C:/Users/andre/Dropbox/HBGDki figures/Nutritional Intervention Analysis/Intervention Plots Stunting Prev.pdf", height=9, width=9)

for(i in c("unstratified", "sex", "enstunt","enwast","gagebrth", "hfoodsec",
          "hhwealth_quart", "mage","mbmi","meducyrs",      
          "mhtcm","predexfd6", "nchldlt5","parity")){

p <-  ggplot(RMAestp[RMAestp$EMvar==i,], aes(x=EMlevel)) + 
      geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
      geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
                     alpha=0.5, size = 3) +
      labs(x = "Effect modifier level", y = "Prevalence Ratio") +
      geom_hline(yintercept = 1) +
        geom_text(aes(x=0.75, y=4, label=Nstudies), size=3,  hjust=0) +
      #coord_cartesian(ylim=range(yticks)) +
      scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
      scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAestp$intervention_level)) +
      scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAestp$intervention_level)) +
      scale_size_continuous(range = c(0.5, 1))+
      theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
      facet_grid(agecat~intervention_level) +
      ggtitle(RMAestp$EMvarlabel[RMAestp$EMvar==i][1])
print(p)
    }

dev.off()



# 
# 
# #Make combined 6 month prev, 24 month prev, and 6-24mo CI.
# RMAest
# RMAestp
# 
# combest <- rbind(RMAest[RMAest$agecat=="6-24 months",], RMAestp[RMAestp$agecat!="Birth",])
# combest <- combest[combest$intervention_level!="VitA",]
# 
# combest$intervention_level <- as.character(combest$intervention_level)
# combest$intervention_level[combest$intervention_level=="Zinc"] <- "Zinc intervention"
# combest$intervention_level[combest$intervention_level=="Maternal"] <- "Maternal intervention"
# combest$intervention_level[combest$intervention_level=="LNS"] <- "LNS intervention"
# combest$intervention_level <- factor(combest$intervention_level, levels=c("Zinc intervention", "Maternal intervention", "LNS intervention"))
# 
# combest$agecat <- as.character(combest$agecat)
# combest$agecat[combest$agecat=="6 months"] <- "Stunted at 6 mo."
# combest$agecat[combest$agecat=="6-24 months"] <- "6-24 mo. stunting CI"
# combest$agecat[combest$agecat=="24 months"] <- "Stunted at 24 mo."
# combest$agecat <- factor(combest$agecat, levels=c("Stunted at 6 mo.", "6-24 mo. stunting CI", "Stunted at 24 mo."))
# 
# 
# combest <- droplevels(combest)
# 
# yticks <- c(0.125,0.25,0.5,1,2,4)
# scaleFUN <- function(x) sprintf("%.2f", x)
# 
# pdf("C:/Users/andre/Dropbox/HBGDki figures/Nutritional Intervention Analysis/Intervention Plots Stunting.pdf", height=9, width=9)
# 
# for(i in c("unstratified", "sex", "enstunt","enwast","gagebrth", "hfoodsec",
#           "hhwealth_quart", "mage","mbmi","meducyrs",      
#           "mhtcm","predexfd6", "nchldlt5","parity")){
# 
# p <-  ggplot(combest[combest$EMvar==i,], aes(x=EMlevel)) + 
#       geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
#       geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
#                      alpha=0.5, size = 3) +
#       labs(x = "Effect modifier level", y = "Prevalence Ratio") +
#       geom_hline(yintercept = 1) +
#         geom_text(aes(x=0.75, y=4, label=Nstudies), size=3,  hjust=0) +
#       #coord_cartesian(ylim=range(yticks)) +
#       scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
#       scale_fill_manual(values=rep(tableau10,4)) +
#       scale_colour_manual(values=rep(tableau10,4)) +
#       scale_size_continuous(range = c(0.5, 1))+
#       theme(strip.background = element_blank(),
#         legend.position="none",
#         strip.text.x = element_text(size=12),
#         axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
#       facet_grid(agecat~intervention_level) +
#       ggtitle(combest$EMvarlabel[combest$EMvar==i][1])
# print(p)
#     }
# 
# dev.off()
# 
# 
# #Create png versions of each plot
# setwd("C:/Users/andre/Dropbox/HBGDki figures/Nutritional Intervention Analysis/Individual plots/")
# 
# for(i in c("unstratified", "sex", "enstunt","enwast","gagebrth", "hfoodsec",
#           "hhwealth_quart", "mage","mbmi","meducyrs",      
#           "mhtcm","predexfd6", "nchldlt5","parity")){
#   
# png(file=paste0(i,"_intplot.png"),width=9*72,height=9*72,res=72)
# p <-  ggplot(combest[combest$EMvar==i,], aes(x=EMlevel)) + 
#       geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
#       geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
#                      alpha=0.5, size = 3) +
#       labs(x = "Effect modifier level", y = "Prevalence Ratio") +
#       geom_hline(yintercept = 1) +
#         geom_text(aes(x=0.75, y=4, label=Nstudies), size=3,  hjust=0) +
#       #coord_cartesian(ylim=range(yticks)) +
#       scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
#       scale_fill_manual(values=rep(tableau10,4)) +
#       scale_colour_manual(values=rep(tableau10,4)) +
#       scale_size_continuous(range = c(0.5, 1))+
#       theme(strip.background = element_blank(),
#         legend.position="none",
#         strip.text.x = element_text(size=12),
#         axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
#       facet_grid(agecat~intervention_level) +
#       ggtitle(combest$EMvarlabel[combest$EMvar==i][1])
# print(p)
# dev.off()
#     }




#Make a plot of the primary outcomes
RMAest


combest <- RMAest[RMAest$agecat!="0-24 months",]
combest <- combest[combest$intervention_level!="VitA",]

#Keep 0-6 mo CI for maternal interventions, and 6-24 for zinc and LNS interventions
combest <- combest[(combest$agecat=="6-24 months" & combest$intervention_level!="Maternal")|(combest$agecat=="0-6 months" & combest$intervention_level=="Maternal"),]


combest$intervention_level <- as.character(combest$intervention_level)
combest$intervention_level[combest$intervention_level=="Zinc"] <- "Zinc intervention (6-24mo. CI)"
combest$intervention_level[combest$intervention_level=="Maternal"] <- "Maternal intervention (0-6mo. CI)"
combest$intervention_level[combest$intervention_level=="LNS"] <- "LNS intervention (6-24mo. CI)"
combest$intervention_level <- factor(combest$intervention_level, levels=c("Maternal intervention (0-6mo. CI)", "LNS intervention (6-24mo. CI)", "Zinc intervention (6-24mo. CI)"))

combest$agecat <- as.character(combest$agecat)
combest$agecat[combest$agecat=="0-6 months"] <- "0-6 mo. stunting CI"
combest$agecat[combest$agecat=="6-24 months"] <- "6-24 mo. stunting CI"
combest$agecat <- factor(combest$agecat, levels=c("0-6 mo. stunting CI", "6-24 mo. stunting CI"))

combest <- droplevels(combest)


yticks <- c(0.125,0.25,0.5,1,2,4)
scaleFUN <- function(x) sprintf("%.2f", x)

pdf("C:/Users/andre/Dropbox/HBGDki figures/Nutritional Intervention Analysis/Intervention Plots Stunting.pdf", height=9, width=9)

for(i in c("unstratified", "sex", "enstunt","enwast","gagebrth", "hfoodsec",
          "hhwealth_quart", "mage","mbmi","meducyrs",      
          "mhtcm","predexfd6", "nchldlt5","parity")){

  if(i=="unstratified"){
      p <-  ggplot(combest[combest$EMvar==i,], aes(x=1)) + 
          geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
          geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
                         alpha=0.5, size = 3) +
          labs(x = "Effect modifier level", y = "Prevalence Ratio") +
          geom_hline(yintercept = 1) +
            geom_text(aes(x=0.75, y=1.3, label=Nstudies), size=5,  hjust=0) +
          #coord_cartesian(ylim=range(yticks)) +
          scale_y_continuous(breaks=c(1/1.3,1/1.2,1/1.1,1,1.1,1.2,1.3), trans='log10', labels=scaleFUN) +
          coord_cartesian(xlim=c(0,2)) +
      scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
      scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
          scale_size_continuous(range = c(0.5, 1))+
          theme(strip.background = element_blank(),
            legend.position="none",
            strip.text.x = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
          facet_wrap(~intervention_level) +
          ggtitle(combest$EMvarlabel[combest$EMvar==i][1])
    print(p)
  }else{
    p <-  ggplot(combest[combest$EMvar==i,], aes(x=EMlevel)) + 
          geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
          geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
                         alpha=0.5, size = 3) +
          labs(x = "Effect modifier level", y = "Prevalence Ratio") +
          geom_hline(yintercept = 1) +
            geom_text(aes(x=0.75, y=4, label=Nstudies), size=3,  hjust=0) +
          #coord_cartesian(ylim=range(yticks)) +
          scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
      scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
      scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
          scale_size_continuous(range = c(0.5, 1))+
          theme(strip.background = element_blank(),
            legend.position="none",
            strip.text.x = element_text(size=12),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
          facet_wrap(~intervention_level) +
          ggtitle(combest$EMvarlabel[combest$EMvar==i][1])
    print(p)
  }
}

dev.off()


#Create png versions of each plot
setwd("C:/Users/andre/Dropbox/HBGDki figures/Nutritional Intervention Analysis/Individual plots/")

for(i in c("unstratified", "sex", "enstunt","enwast","gagebrth", "hfoodsec",
          "hhwealth_quart", "mage","mbmi","meducyrs",      
          "mhtcm","predexfd6", "nchldlt5","parity")){
  
png(file=paste0(i,"_intplot.png"),width=9*72,height=9*72,res=72)
    if(i=="unstratified"){
      p <-  ggplot(combest[combest$EMvar==i,], aes(x=1)) + 
          geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
          geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
                         alpha=0.5, size = 3) +
          labs(x = "Effect modifier level", y = "Prevalence Ratio") +
          geom_hline(yintercept = 1) +
            geom_text(aes(x=0.75, y=1.3, label=Nstudies), size=5,  hjust=0) +
          #coord_cartesian(ylim=range(yticks)) +
          scale_y_continuous(breaks=c(1/1.3,1/1.2,1/1.1,1,1.1,1.2,1.3), trans='log10', labels=scaleFUN) +
          coord_cartesian(xlim=c(0,2)) +
          scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
          scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
          scale_size_continuous(range = c(0.5, 1))+
          theme(strip.background = element_blank(),
            legend.position="none",
            strip.text.x = element_text(size=12),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
          facet_wrap(~intervention_level) +
          ggtitle(combest$EMvarlabel[combest$EMvar==i][1])
    print(p)
  }else{
      p <-  ggplot(combest[combest$EMvar==i,], aes(x=EMlevel)) + 
            geom_point(aes(y=RR, fill=intervention_level, color=intervention_level), size = 4) +
            geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_level),
                           alpha=0.5, size = 3) +
            labs(x = "Effect modifier level", y = "Prevalence Ratio") +
            geom_hline(yintercept = 1) +
              geom_text(aes(x=0.75, y=4, label=Nstudies), size=3,  hjust=0) +
            #coord_cartesian(ylim=range(yticks)) +
            scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
            scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
            scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(combest$intervention_level)) +
            scale_size_continuous(range = c(0.5, 1))+
            theme(strip.background = element_blank(),
              legend.position="none",
              strip.text.x = element_text(size=12),
              axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
            facet_wrap(~intervention_level) +
            ggtitle(combest$EMvarlabel[combest$EMvar==i][1])
      print(p)
  }
dev.off()
    }


