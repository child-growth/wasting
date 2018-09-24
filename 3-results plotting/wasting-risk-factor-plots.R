
rm(list=ls())
library(tidyverse)
library(metafor)

load("C:/Users/andre/Documents/HBGDki/Results/wasting_results.rdata")


d <- results

allvars<-unique(d$intervention_variable)

head(d)

#Subset to relative risks
d <- d %>% filter(type=="RR")


#Subset to adjusted
d <- d %>% filter(adjustment_set!="unadjusted")

missing <- allvars[!(allvars %in% unique(d$intervention_variable))]
missing

#Subset to primary outcomes
table(d$agecat)

d <- d %>% filter(agecat=="0-6 months"| agecat=="6 months"| agecat=="6-24 months"| agecat=="24 months")


#Drop enrolled wasted as a RF for wasting
d <- d %>% filter(intervention_variable!="enwast")


#Drop diarrhea under 6 months
d <- d %>% filter(intervention_variable!="perdiar6")



head(d)


#Pooled estimate function
poolRR <- function(d){
  #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(N=n())
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$N)
  }else{
    
    fit<-NULL
    try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="REML", measure="RR"))
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
    
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
table(RMAest$agecat)

RMAest <- droplevels(RMAest)
RMAest$agecat <- as.character(RMAest$agecat)

RMAest$agecat[grepl("-",RMAest$agecat)] <- paste0(RMAest$agecat[grepl("-",RMAest$agecat)],"\ncumulative incidence")
RMAest$agecat[!grepl("-",RMAest$agecat)] <- paste0(RMAest$agecat[!grepl("-",RMAest$agecat)]," prevalence")

#RMAest$agecat[grepl(" \\(no birth st\\.\\)\\\ncumulative incidence",RMAest$agecat)] <- "cumulative incidence\n(no birth stunting)"


#RMAest$agecat <- factor(RMAest$agecat, levels = c("0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence"))
RMAest$agecat <- factor(RMAest$agecat, levels=c("3 months prevalence", "3-6 months\ncumulative incidence", "0-6 months (no birth st.)\ncumulative incidence","0-6 months\ncumulative incidence",
                                                "6 months prevalence","6-9 months\ncumulative incidence","9 months prevalence","9-12 months\ncumulative incidence","12 months prevalence",
                                                "12-15 months\ncumulative incidence","15 months prevalence","15-18 months\ncumulative incidence","18 months prevalence",
                                                "0-24 months (no birth st.)","6-24 months\ncumulative incidence","0-24 months (no birth st.)\ncumulative incidence","0-24 months\ncumulative incidence","24 months prevalence"))


#Fix WHZ quartile RF levels
# RMAest$RFlabel[RMAest$RFlabel=="1" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q1"
# RMAest$RFlabel[RMAest$RFlabel=="2" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q2"
# RMAest$RFlabel[RMAest$RFlabel=="3" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q3"
# RMAest$RFlabel[RMAest$RFlabel=="4" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q4"

#Change binary variables into yes/no
binvars <- c("hdlvry","vagbrth", "enwast","anywast06","pers_wast", "earlybf","predexfd6",
             "predfeed3","predfeed36","predfeed6","exclfeed3","exclfeed36","exclfeed6",
             "perdiar6","perdiar24","impsan","safeh20","trth2o","impfloor","cleanck")
RMAest$intervention_level[RMAest$intervention_level=="0" & RMAest$intervention_variable %in% binvars] <- "No"
RMAest$intervention_level[RMAest$intervention_level=="1" & RMAest$intervention_variable %in% binvars] <- "Yes"

#Att birthweight grams
RMAest$intervention_level[RMAest$intervention_level=="Low birth weight"] <- "< 2500 g"
RMAest$intervention_level[RMAest$intervention_level=="Normal or high birthweight"] <- ">= 2500 g"

unique(RMAest$intervention_level)
RMAest$intervention_level <- gsub("Wealth ","",RMAest$intervention_level)
RMAest$intervention_level <- factor(RMAest$intervention_level, 
                                    levels=c("0","1", "No", "Yes",
                                             "<48 cm" , "[48-50) cm",  ">=50 cm",                                  
                                             "< 2500 g",">= 2500 g", 
                                             "2","3","4","5","6","7","8","9",  "10" , "11","12" ,
                                             "<32" , "[32-38)", ">=38",
                                             "Low", "Medium", "High",                    
                                             "<162 cm", "[162-167) cm" , ">=167 cm",
                                             "Preterm", "Early term", "Full or late term",           
                                             "Food Insecure", "Mildly Food Insecure", "Food Secure",               
                                             "Q1", "Q2", "Q3", "Q4",
                                             "<25","[25-30)",">=30",                      
                                             "Underweight", "Normal weight", "Overweight or Obese",
                                             "<151 cm", "[151-155) cm", ">=155 cm",
                                             "<52 kg", "[52-58) kg", ">=58 kg",
                                             "2+","3 or less","4-5","6-7","8+","3+","4+",                                                 
                                             "0%","(0%, 5%]",">5%","Female","Male",
                                             "WHZ Q1", "WHZ Q2", "WHZ Q3", "WHZ Q4"))



unique(RMAest$intervention_variable)
RMAest$intervention_variable <- factor(RMAest$intervention_variable,
                                       levels=c("sex","birthlen","birthwt", "gagebrth",
                                                "hdlvry","vagbrth",
                                                "enwast","anywast06","pers_wast",
                                                "earlybf","predexfd6",
                                                "predfeed3","predfeed36","predfeed6",
                                                "exclfeed3","exclfeed36","exclfeed6",
                                                "perdiar6","perdiar24",
                                                "mage","fage","mhtcm","fhtcm",
                                                "mwtkg","mbmi","single",
                                                "meducyrs","feducyrs",
                                                "parity",
                                                "nchldlt5","nhh","nrooms",
                                                "hhwealth_quart","hfoodsec",
                                                "impsan","safeh20","trth2o",
                                                "impfloor","cleanck",
                                                "brthmon" ,"month",
                                                "lag_WHZ_quart"))   


#Add variable labels
unique(RMAest$intervention_variable)

RMAest$RFlabel <- NA
RMAest$RFlabel[RMAest$intervention_variable=="sex"] <-  "Gender"
RMAest$RFlabel[RMAest$intervention_variable=="enwast"] <-  "Enrolled wasted"
RMAest$RFlabel[RMAest$intervention_variable=="gagebrth"] <-  "Gestational age at birth"
RMAest$RFlabel[RMAest$intervention_variable=="predexfd6"] <-  "Exclusive or Predominant breastfeeding under 6 months"
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
RMAest$RFlabel[RMAest$intervention_variable=="birthwt"] <- "Birthweight (kg)" 
RMAest$RFlabel[RMAest$intervention_variable=="birthlen"] <- "Birth length (cm)" 
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
RMAest$RFlabel[RMAest$intervention_variable=="predfeed3"] <-  "Predominant breastfeeding under 3 months"
RMAest$RFlabel[RMAest$intervention_variable=="predfeed36"] <-  "Predominant breastfeeding from 3-6 months"
RMAest$RFlabel[RMAest$intervention_variable=="predfeed6"] <-  "Predominant breastfeeding under 6 months"
RMAest$RFlabel[RMAest$intervention_variable=="exclfeed3"] <-  "Exclusive breastfeeding under 3 months"
RMAest$RFlabel[RMAest$intervention_variable=="exclfeed36"] <-  "Exclusive breastfeeding from 3-6 months"
RMAest$RFlabel[RMAest$intervention_variable=="exclfeed6"] <-  "Exclusive breastfeeding under 6 months"
RMAest$RFlabel[RMAest$intervention_variable=="month"] <-  "Month of measurement"
RMAest$RFlabel[RMAest$intervention_variable=="brthmon"] <-  "Birth month"
RMAest$RFlabel[RMAest$intervention_variable=="lag_WHZ_quart"] <-  "Mean WHZ in the prior 3 months"





# Split prevalence and incidence
RMAprev <- RMAest[grepl("prevalence",RMAest$agecat),]
RMAest <- RMAest[!grepl("prevalence",RMAest$agecat),]


#Print plots across all intervention arms
yticks <- c(0.125,0.25,0.5,1,2,4,8,16)
scaleFUN <- function(x) sprintf("%.2f", x)

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
         paste, collapse="\n")
}  


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Wasting/")
pdf("CI only pooled/Risk Factor Plots Wasting", height=8, width=12)

j<-0
for(i in levels(RMAest$intervention_variable)){
  j<-j+1
  plotdf <- RMAest[RMAest$intervention_variable==i,]  
  
  if(nrow(plotdf)>0){
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
          strip.text.x = element_text(size=10), axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
    ggtitle(plotdf$RFlabel[1])  +
  facet_wrap(~agecat, nrow = 1, labeller = label_wrap) +
  ggsave(p, file=paste0("CI only pooled/",j,"_",i,"_RFplot.png"), height=8, width=12)
  ggsave(p, file=paste0("CI only pooled/Powerpoint size/",j,"_",i,"_RFplot.png"), width=5.7, height=4.6)
  
  print(p)
  }
}

dev.off()


label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
         paste, collapse="\n")
}  



setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Wasting/")
pdf("Prev only pooled/Risk Factor Plots Wasting", height=8, width=12)

RMAprev <- droplevels(RMAprev)

j<-0
for(i in levels(RMAprev$intervention_variable)){
  j<-j+1
  plotdf <- RMAprev[RMAprev$intervention_variable==i,]  
  
  if(nrow(plotdf)>0){
  p <-  ggplot(plotdf, aes(x=intervention_level)) + 
    geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
    geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                   alpha=0.5, size = 3) +
    labs(x = "Risk factor level", y = "Relative risk") +
    geom_hline(yintercept = 1) +
    geom_text(aes(x=0.75, y=ceiling(max(plotdf$RR.CI2)), label=Nstudies), size=3,  hjust=0) +
    #coord_cartesian(ylim=range(yticks)) +
    scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
    scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAprev$agecat)) +
    scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAprev$agecat)) +
    scale_size_continuous(range = c(0.5, 1))+
    theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
    facet_wrap(~agecat, nrow = 1, labeller = label_wrap) +
    ggtitle(plotdf$RFlabel[1])
  ggsave(p, file=paste0("Prev only pooled/",j,"_",i,"_RFplot.png"), height=8, width=12)
  ggsave(p, file=paste0("Prev only pooled/Powerpoint size/",j,"_",i,"_RFplot.png"), width=5.7, height=4.6)
  print(p)
  }
}

dev.off()



# 
# #Create 1 summary plot with the largest RR
sumdf <- RMAest %>% ungroup() %>%
  group_by(intervention_variable) %>%
  mutate(maxRR=max(abs(1-RR))) %>%
  filter(maxRR==abs(1-RR)) %>% ungroup() %>%
  arrange(RR) %>%
  mutate(order=row_number())

sumdf$intervention_variable <- as.character(sumdf$intervention_variable)
sumdf$intervention_variable <- factor(sumdf$intervention_variable, levels=unique(sumdf$intervention_variable))

yticks <- c(0.125,0.25,0.5,1,2,4,8,16,32,64)

sumdf <- droplevels(sumdf)
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
          legend.position="bottom",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        ggtitle("")

print(p)







#-------------------------------------------------------------------------------------------
# Risk factor plots 
#-------------------------------------------------------------------------------------------


setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar")

scaleFUN <- function(x) sprintf("%.2f", x)

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
         paste, collapse="\n")
}  


#-------------------------------------------------------------------------------------------
# prenatal characteristics
#-------------------------------------------------------------------------------------------


df <- RMAest %>%
  filter(agecat=="0-24 months (no birth st.)\ncumulative incidence"|
           agecat=="0-24 months\ncumulative incidence") %>%
  filter(intervention_variable %in% c("mhtcm","mwtkg","mbmi",
                                      "fhtcm","meducyrs","feducyrs",
                                      "hhwealth_quart"))
df <- droplevels(df)

df$intervention_variable <- factor(df$intervention_variable, levels=c("mhtcm","mwtkg","mbmi",
                                                                      "fhtcm","meducyrs","feducyrs",
                                                                      "hhwealth_quart"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))


yticks <- c(2/3, 1, 3/2)


df$intervention_level <- revalue(df$intervention_level, c("Overweight or Obese"="Overweight"))

p <-  ggplot(df, aes(x=intervention_level)) + 
  geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                 alpha=0.5, size = 3) +
  facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
  labs(x = "Exposure level", y = "Relative risk") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=2, y=(max(df$RR.CI2))-.1, label=Nstudies), size=2,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  #ggtitle("Prenatal characteristics associated with\nstunting from birth to 2 years")
  ggtitle("Outcome: any stunting from birth to 2 years")

ggsave(p, file="prenatal_RFplot.png",  width=6, height=5.2)


#-------------------------------------------------------------------------------------------
# at-birth characteristics
#-------------------------------------------------------------------------------------------


df <- RMAest %>%
  filter(agecat=="0-24 months (no birth st.)\ncumulative incidence"|
           agecat=="0-24 months\ncumulative incidence") %>%
  filter(intervention_variable %in% c("sex","birthlen", "birthwt", "gagebrth", "hdlvry","vagbrth", "parity"))
df <- droplevels(df)

df$intervention_variable <- factor(df$intervention_variable, levels=
                                     c("sex","gagebrth","birthlen", "birthwt",  "hdlvry","vagbrth", "parity"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))

# df$intervention_level <- revalue(df$intervention_level, c("Normal or high birthweight"="Normal birthweight",
#                                                           "Full or late term"="Full term"))


yticks <- c(2/3, 1, 3/2,2,3,4)

p <-  ggplot(df, aes(x=intervention_level)) + 
  geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                 alpha=0.5, size = 3) +
  facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
  labs(x = "Exposure level", y = "Relative risk") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=1.5, y=(max(df$RR.CI2))-.1, label=Nstudies), size=2,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Outcome: any stunting by 2 years, excluding stunting at birth")

ggsave(p, file="atbirth_RFplot.png",  width=6, height=5.2)


df <- df %>% filter((RR.CI1 < 1 & RR.CI2 < 1)|(RR.CI1 > 1 & RR.CI2 > 1))
as.data.frame(df)
#-------------------------------------------------------------------------------------------
# postnatal characteristics
#-------------------------------------------------------------------------------------------



df <- RMAest %>%
  #filter(agecat=="6-24 months\ncumulative incidence") %>%
  filter(agecat=="6-24 months (no birth st.)\ncumulative incidence"|
           agecat=="6-24 months\ncumulative incidence") %>%
  filter(intervention_variable %in% c("exclfeed3",
                                      "exclfeed36","exclfeed6","impfloor","impsan",
                                      "nhh","nrooms","hfoodsec"))
df <- droplevels(df)

df$intervention_variable <- factor(df$intervention_variable, levels=
                                     c("exclfeed3","exclfeed36",
                                       "exclfeed6","hfoodsec",
                                       "impfloor","impsan",
                                       "nhh","nrooms"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))



yticks <- c(2/3, 1, 3/2,2,3,4)


p <-  ggplot(df, aes(x=intervention_level)) + 
  geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                 alpha=0.5, size = 3) +
  facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
  labs(x = "Exposure level", y = "Relative risk") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=(min(df$RR.CI1))+.1, label=Nstudies), size=2,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Outcome: any stunting from 6 months to 2 years")


ggsave(p, file="postnatal_RFplot.png",  width=6, height=5.2)





#-------------------------------------------------------------------------------------------
# wasting as a risk factor
#-------------------------------------------------------------------------------------------
df2 <- df


df <- RMAest %>%
  filter(agecat=="6-24 months\ncumulative incidence") %>%
  filter(intervention_variable %in% c("anywast06","pers_wast"))

df_monthly <- RMAest_monthky %>% filter(agecat=="6-24 months")
df_monthly$Nstudies <- paste0("N studies ", as.character(df_monthly$Nstudies))
df_monthly$intervention_level <- c("No", "Yes")
df_monthly$intervention_variable <- "anywast06_monthly"

df <- bind_rows(df, df_monthly)
df <- droplevels(df)



df$intervention_variable <- factor(df$intervention_variable, levels=
                                     c("anywast06", "anywast06_monthly","pers_wast"))
df <- df %>% arrange(intervention_variable)
df$RFlabel[df$intervention_variable=="pers_wast"] <- "Persistent wasting\nbefore 6 months age"
df$RFlabel[df$intervention_variable=="anywast06_monthly"] <- "Any wasting\nbefore 6 months age\n(monthly studies)"
df$RFlabel[df$intervention_variable=="anywast06"] <- "Any wasting\nbefore 6 months age"
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))



yticks <- c(2/3, 1, 3/2,2,3,4)



p <-  ggplot(df, aes(x=intervention_level)) + 
  geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                 alpha=0.5, size = 3) +
  facet_wrap(~RFlabel, scales="free_x", ncol=3, labeller = label_wrap) +
  labs(x = "Exposure level", y = "Relative risk") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=Nstudies), size=3,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Associations between wasting before 6 months\nand stunting from 6 months to 2 years")


ggsave(p, file="Wasting_as_a_RF_plots.png",  width=6, height=4.2)




#-------------------------------------------------------------------------------------------
# Stunting by WHZ quartiles
#-------------------------------------------------------------------------------------------

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/webinar_results.rdata")

table(results$intervention_variable)
results <- results %>% filter(type=="RR" & intervention_variable=="lag_WHZ_quart")

#Pooled estimate function
poolRR <- function(d){
  #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(N=n())
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$N)
  }else{
    
    fit<-NULL
    try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="REML", measure="RR"))
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
    
    est<-data.frame(fit$b, fit$se)
    colnames(est)<-c("logRR.psi","logSE")
    
    est$RR<-exp(est$logRR)
    est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
    est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
    
    est$Nstudies <- nstudies$N
  }
  
  return(est)
}

RMAest <- results %>% group_by(intervention_variable, agecat, intervention_level) %>% 
  do(try(poolRR(.))) %>% as.data.frame()







label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
         paste, collapse="\n")
}  

yticks <- c(2/3, 1, 3/2,2,3,4)



df <- RMAest %>% filter(intervention_variable=="lag_WHZ_quart")
#df <- df[grepl("prev",df$agecat),]
df <- df[!grepl("-",df$agecat),]

#Temp fix agecat
df$agecat <- as.character(df$agecat)
df$agecat[df$agecat=="21 months"] <- "24 months"
df$agecat[df$agecat=="18 months"] <- "21 months"
df$agecat[df$agecat=="15 months"] <- "18 months"
df$agecat[df$agecat=="12 months"] <- "15 months"
df$agecat[df$agecat=="9 months"] <- "12 months"
df$agecat[df$agecat=="6 months"] <- "9 months"
df$agecat[df$agecat=="3 months"] <- "6 months"

#df$agecat <- factor(df$agecat, levels=c("3 months","6 months","9 months","12 months","15 months","18 months","21 months"))
df$agecat <- factor(df$agecat, levels=c("6 months","9 months","12 months","15 months","18 months","21 months", "24 months"))




df$intervention_level <- as.character(df$intervention_level)
df$intervention_level[df$intervention_level=="1"] <- "Q1"  
df$intervention_level[df$intervention_level=="2"] <- "Q2"  
df$intervention_level[df$intervention_level=="3"] <- "Q3"  
df$intervention_level[df$intervention_level=="4"] <- "Q4"  
df$intervention_level <- factor(df$intervention_level)

p <- ggplot(df, aes(x=intervention_level)) +
  geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                 alpha=0.5, size = 3) +
  facet_wrap(~agecat, scales="free_x", nrow=1, labeller = label_wrap) +
  labs(x = "Quartile of mean WLZ in the past three months",
       y = "Relative Risk of Stunting") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=paste0("Studies: ",Nstudies)), size=3,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=9, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Associations between prevalent stunting and\nquartile of mean WLZ in the past three months")
p

ggsave(p, file="Stunting_by_prior_WLZ_plots.png",  width=7, height=5)






df <- RMAest %>% filter(intervention_variable=="lag_WHZ_quart")
df <- df[grepl("-",df$agecat),]
df$agecat <- factor(df$agecat, levels=c("3-6 months","6-9 months","9-12 months","12-15 months","15-18 months"))

df$intervention_level <- as.character(df$intervention_level)
df$intervention_level[df$intervention_level=="1"] <- "Q1"  
df$intervention_level[df$intervention_level=="2"] <- "Q2"  
df$intervention_level[df$intervention_level=="3"] <- "Q3"  
df$intervention_level[df$intervention_level=="4"] <- "Q4"  
df$intervention_level <- factor(df$intervention_level)

p <- ggplot(df, aes(x=intervention_level)) +
  geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                 alpha=0.5, size = 3) +
  facet_wrap(~agecat, scales="free_x", nrow=1, labeller = label_wrap) +
  labs(x = "Quartile of mean WLZ in the prior three months to the CI period",
       y = "Relative Risk of Stunting") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=paste0("Studies: ",Nstudies)), size=3,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=9, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Associations between cumulative incidence of\nstunting over 3 months periods and\nquartile of mean WLZ in the prior 3 months periods")
p

ggsave(p, file="Stunting_CI_by_prior_WLZ_plots.png",  width=7, height=5)


#-------------------------------------------------------------------------------------------
# HAZ curves by WHZ quartiles
#-------------------------------------------------------------------------------------------

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/HAZ_by_WHZ.RData")



p<-ggplot(plotdf, aes(x=agedays, y=haz, group=lag_WHZ_quart, color=lag_WHZ_quart)) +
  geom_smooth(method = 'loess') +
  #geom_smooth(method = 'gam', formula= y ~ s(x,  k=4, bs = "cs")) +
  facet_wrap(~agecat, scales="free_x", nrow=1) +
  scale_color_manual(values=tableau10, name = "Quartile of WHZ in\nthe prior 3 months")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1)) +
  xlab("Child age in days") + ylab("LAZ") + 
  ggtitle("Spline curves of HAZ over 3-month age ranges\nstratified by quartile of WHZ in prior 3-month range.") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 0, hjust = 1)) 

ggsave(p, file="HAZcurves_by_WHZ.png", width=8.25, height=3.25)










#-------------------------------------------------------------------------------------------
# Recovery by 2 years
#-------------------------------------------------------------------------------------------


# sort by recovery %
rev_clean <- function(d){
  d$cohort=factor(d$cohort, levels = d$cohort[order(d$y)])
  d$y[d$cohort=="Pooled"]=d$y[d$cohort=="Pooled"]*100
  d$ci.lb[d$cohort=="Pooled"]=d$ci.lb[d$cohort=="Pooled"]*100
  d$ci.ub[d$cohort=="Pooled"]=d$ci.ub[d$cohort=="Pooled"]*100
  d$pooled=as.factor(ifelse(d$cohort=="Pooled",1,0))
  
  return(d)
}

plot.df <- rev_clean(plot.df)
plot.df36 <- rev_clean(plot.df36)
plot.df48 <- rev_clean(plot.df48)

plot.df$agecat <- plot.df36$agecat <- plot.df48$agecat <- "age"
plot.df$agecat[plot.df$cohort=="Pooled"] <- plot.df36$agecat[plot.df36$cohort=="Pooled"] <- plot.df48$agecat[plot.df48$cohort=="Pooled"] <- "pooled"

# plot recovery
prec <- ggplot(plot.df,aes(y=y,x=cohort), color=tableau10[3])+
  geom_point(aes(fill=agecat, color=agecat, shape=agecat), size = 4) +
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub, color=agecat), alpha=0.5, size = 3) +
  coord_flip()+
  scale_color_manual(values=c(tableau10[3],"black")) + scale_fill_manual(values=c(tableau10[3],"black"))+
  scale_shape_manual(values=c(21, 23)) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Cohort")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted and were recovered at 36 months") +
  theme(legend.position = "none")

ggplot(plot.df36,aes(y=y,x=cohort), color=tableau10[3])+
  geom_point(aes(fill=agecat, color=agecat, shape=agecat), size = 4) +
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub, color=agecat), alpha=0.5, size = 3) +
  coord_flip()+
  scale_color_manual(values=c(tableau10[3],"black")) + scale_fill_manual(values=c(tableau10[3],"black"))+
  scale_shape_manual(values=c(21, 23)) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Cohort")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted and were recovered at 36 months") +
  theme(legend.position = "none")


ggsave(prec, file="pooled_rec24.png", width=10, height=4)



