

rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")
load("C:/Users/andre/Documents/HBGDki/Results/wasting_results.rdata")
load("C:/Users/andre/Documents/HBGDki/Results/wast_RiskFactor_Ns.Rdata")


d <- results


head(d)

#Merge in N's

#NOTE: NEED TO CALC N's BY OUTCOME AS WELL!
#Change on GHAP
head(prevN)


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
poolRR <- function(d, method="REML"){
    #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(N=n())
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$N)
  }else{
    
  #cat(d$intervention_variable[1]," ", levels(d$agecat)[1]," ", d$intervention_level[1],"\n")
  fit<-NULL
  try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"))
  if(is.null(fit) & method=="REML"){fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR")}

  est<-data.frame(fit$b, fit$se)
  colnames(est)<-c("logRR.psi","logSE")
  
  est$RR<-exp(est$logRR)
  est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
  est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
  
  est$Nstudies <- nstudies$N
  }
  
  return(est)
}




#Edit df for plotting
colnames(d)


#Strip grant identifier and add country
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}


  d$studyid <- gsub("^k.*?-" , "", d$studyid)
  d$studyid <- paste0(d$studyid, ", ", paste0(substring(as.character(d$country),1,1), tolower(substring(as.character(d$country),2))))
  d$studyid <- gsub("Tanzania, united republic of", "Tanzania", d$studyid)
  d$studyid <- gsub("africa", "Africa", d$studyid)

#Add region
  d <- d %>% mutate(region = case_when(
    country=="BANGLADESH" | country=="INDIA"|
      country=="NEPAL" | country=="PAKISTAN"|
      country=="PHILIPPINES"                   ~ "Asia", 
      
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
table(d$region)



#Pooled effects
RMAest_RE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
              do(poolRR(.)) %>% mutate(studyid="Pooled - Random", region="Pooled", pooled=1) %>% as.data.frame()
RMAest_FE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
              do(poolRR(., method = "FE")) %>% mutate(studyid="Pooled - Fixed", region="Pooled", pooled=1) %>% as.data.frame()


#Add regional estimates
RMAest_RE_africa <- d %>% filter(region=="Africa") %>% 
              group_by(intervention_variable, agecat, intervention_level) %>%
              do(poolRR(.)) %>% mutate(studyid="Pooled - Africa", region="Africa", pooled=1) %>% as.data.frame()
RMAest_RE_asia <- d %>% ungroup() %>% filter(region=="Asia") %>% do(droplevels(.)) %>% 
              group_by(intervention_variable, agecat, intervention_level) %>%
              do(poolRR(.)) %>% mutate(studyid="Pooled - Asia", region="Asia", pooled=1) %>% as.data.frame()
RMAest_RE_latamer <- d %>% filter(region=="Latin America") %>% 
              group_by(intervention_variable, agecat, intervention_level) %>%
              do(poolRR(.)) %>% mutate(studyid="Pooled - Latin America", region="Latin America", pooled=1) %>% as.data.frame()


#df <- d %>% filter(region=="Asia") %>% filter(intervention_variable=="mage"& agecat=="6 months" & intervention_level==">=30")



#merge in pooled effects
dfull <- d
d <- d %>% rename(RR=estimate, RR.CI1=ci_lower, RR.CI2=ci_upper) %>% 
  mutate(Nstudies=1, pooled=0, region=as.character(region)) %>% 
  subset(., select=c(studyid, country, region, intervention_variable,agecat,intervention_level, baseline_level,
                    RR, RR.CI1, RR.CI2, pooled, Nstudies, adjustment_set))
d <- bind_rows(d, RMAest_RE_africa, RMAest_RE_asia, RMAest_RE_latamer, RMAest_RE, RMAest_FE)





#Order factors for plotting

d <- droplevels(d)
d$agecat <- as.character(d$agecat)
d$agecat[d$agecat=="0-6 months"] <- "0-6 month\ncumulative incidence"
d$agecat[d$agecat=="6 months"] <- "6 month prevalence"
d$agecat[d$agecat=="6-24 months"] <- "6-24 month\ncumulative incidence"
d$agecat[d$agecat=="24 months"] <- "24 month prevalence"
d$agecat <- factor(d$agecat, levels = c("0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence"))

unique(d$intervention_level)
d$intervention_level <- factor(d$intervention_level, 
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

unique(d$intervention_variable)
d$intervention_variable <- factor(d$intervention_variable,
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
unique(d$intervention_variable)

d$RFlabel <- NA
d$RFlabel[d$intervention_variable=="enwast"] <-  "Enrolled wasted"
d$RFlabel[d$intervention_variable=="gagebrth"] <-  "Gestational age at birth"
d$RFlabel[d$intervention_variable=="predexfd6"] <-  "Exclusive or Predominant breastfeed under 6 months"
d$RFlabel[d$intervention_variable=="mage"] <- "Mother's age" 
d$RFlabel[d$intervention_variable=="mhtcm"] <- "Mother's height" 
d$RFlabel[d$intervention_variable=="mwtkg"] <- "Mother's weight" 
d$RFlabel[d$intervention_variable=="mbmi"] <- "Mother's BMI" 
d$RFlabel[d$intervention_variable=="meducyrs"] <- "Mother's education" 
d$RFlabel[d$intervention_variable=="parity"] <-  "Birth order" 
d$RFlabel[d$intervention_variable=="hfoodsec"] <- "Household food security" 
d$RFlabel[d$intervention_variable=="nchldlt5"] <-   "Number of children <5 in household"
d$RFlabel[d$intervention_variable=="hhwealth_quart"] <-  "Household wealth" 
d$RFlabel[d$intervention_variable=="fage"] <- "Father's age" 
d$RFlabel[d$intervention_variable=="fhtcm"] <- "Father's height" 
d$RFlabel[d$intervention_variable=="birthwt"] <- "Birthweight (Z-scored)" 
d$RFlabel[d$intervention_variable=="birthlen"] <- "Birth length (Z-scored)" 
d$RFlabel[d$intervention_variable=="vagbrth"] <- "Vaginal birth" 
d$RFlabel[d$intervention_variable=="hdlvry"] <- "Child delivered at home" 
d$RFlabel[d$intervention_variable=="single"] <- "Single parent" 
d$RFlabel[d$intervention_variable=="nrooms"] <- "Number of rooms in household" 
d$RFlabel[d$intervention_variable=="nhh"] <- "Number of people in household" 
d$RFlabel[d$intervention_variable=="meducyrs"] <- "Maternal education quartile" 
d$RFlabel[d$intervention_variable=="feducyrs"] <- "Paternal education quartile" 
d$RFlabel[d$intervention_variable=="anywast06"] <- "Any wasting before 6 months age" 
d$RFlabel[d$intervention_variable=="pers_wast"] <- "Persistent wasting before 6 months age" 
d$RFlabel[d$intervention_variable=="trth2o"] <- "Treats drinking water" 
d$RFlabel[d$intervention_variable=="cleanck"] <- "Clean cooking fuel usage" 
d$RFlabel[d$intervention_variable=="impfloor"] <- "Improved floor" 
d$RFlabel[d$intervention_variable=="impsan"] <- "Improved sanitation" 
d$RFlabel[d$intervention_variable=="safeh20"] <- "Safe water source" 
d$RFlabel[d$intervention_variable=="perdiar6"] <- "Quartile of diarrhea longitudinal\nprevalence under 6 months" 
d$RFlabel[d$intervention_variable=="perdiar24"] <- "Quartile of diarrhea longitudinal\nprevalence under 24 months" 
d$RFlabel[d$intervention_variable=="earlybf"] <- "Breastfeed within an hour of birth" 


#order by region
d$region <- as.character(d$region)
d$region <- factor(d$region, levels=c("Pooled","Asia", "Latin America","Africa","Europe"))
d <- d %>% arrange(rev(pooled), region)

d$studyid <- as.character(d$studyid)
d$studyid <- factor(d$studyid, levels=unique(d$studyid))

#Clean up adjustment sets (drop W_ and the missingness indicators)
d$adjustment_set <- gsub("W_","",d$adjustment_set)
#d$adjustment_set <- gsub("delta_","m_",d$adjustment_set)
for(i in 1:30){d$adjustment_set <- gsub("(delta_).*?,", "", d$adjustment_set)}
d$adjustment_set <- gsub("(delta_).*?", "", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)

#Print plots across all intervention arms
yticks <- c(0.25,0.5,1,2,4,8,16,32,64,128)
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("black","#1F77B4","#FF7F0E","#2CA02C","#D62728", 
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

df <- d %>% filter(intervention_variable=="gagebrth" & agecat=="6 month prevalence")
df$studyid <- as.character(df$studyid)
df$studyid <- factor(df$studyid, levels=unique(df$studyid))

df <- df %>% filter(RR.CI1!=RR.CI2) #drop reference level
df <- droplevels(df)
df$pooled <- factor(df$pooled)

stdyID <- levels(df$studyid)
Nstud <- addNA(factor(df$adjustment_set))

  p <-  ggplot(df, aes(x=studyid)) + 
        #geom_point(aes(y=RR, fill=region, color=region), size = 4, shape= ifelse(df$pooled==1,5,6)) +
        geom_point(aes(shape=pooled, y=RR, fill=region, color=region), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=region)) +
        coord_flip(ylim=range(yticks)) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_vline(xintercept = 2.5, linetype=2) +
        geom_vline(xintercept = 5.5) +
        geom_text(aes(y=0.12, label=Nstudies), size=3,  hjust=0) +
        geom_text(aes(y=8, label=adjustment_set), size=2,  hjust=0) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_shape_manual(values=c(21, 23)) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$region)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$region)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="bottom",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~intervention_level, ncol=1) +
        ggtitle(paste0("Risk factor: ", df$RFlabel[1], "\n",
                       "Ref. level: ", df$baseline_level[!is.na(df$baseline_level)], "\n", 
                       "Outcome: ", df$agecat))
  p




df <- d %>% filter(agecat=="6 month prevalence")


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/")
pdf("Risk Factor Forest Plots 6mo prevalence.pdf", height=8, width=12)

j<-0
for(i in levels(df$intervention_variable)){
  j<-j+1 
  plotdf <- df[df$intervention_variable==i,]  
    
  p <-  ggplot(df, aes(x=studyid)) + 
        #geom_point(aes(y=RR, fill=region, color=region), size = 4, shape= ifelse(df$pooled==1,5,6)) +
        geom_point(aes(shape=pooled, y=RR, fill=region, color=region), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=region)) +
        coord_flip(ylim=range(yticks)) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_vline(xintercept = 2.5, linetype=2) +
        geom_vline(xintercept = 5.5) +
        geom_text(aes(y=0.12, label=Nstudies), size=3,  hjust=0) +
        geom_text(aes(y=8, label=adjustment_set), size=2,  hjust=0) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_shape_manual(values=c(21, 23)) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$region)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$region)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="bottom",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~intervention_level, ncol=1) +
        ggtitle(paste0("Risk factor: ", plotdf$RFlabel[1], "\n",
                       "Ref. level: ", plotdf$baseline_level[!is.na(plotdf$baseline_level)], "\n", 
                       "Outcome: ", plotdf$agecat))
  print(p)
}

dev.off()

