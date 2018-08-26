




rm(list=ls())
library(tidyverse)
library(metafor)


load("C:/Users/andre/Documents/HBGDki/Results/intervention_results.rdata")


d <- results
head(d)



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
EM$EMlevel[is.na(EM$EMlevel)] <- "Unstratified" 





EM[is.na(EM)] <-""

EM$EMlevel <- paste0(EM[,1],EM[,2],EM[,3],EM[,4],EM[,5],EM[,6],EM[,7],EM[,8],EM[,9],EM[,10],EM[,11],EM[,12],EM[,13])
table(EM$EMlevel)

EM <- EM %>% select(EMvar, EMlevel)

unique(paste0(EM$EMvar," ", EM$EMlevel))
unique(EM$EMlevel)

EM$EMlevel <- factor(EM$EMlevel, levels = 
c("Unstratified","Female", "Male","<259", "[259-273)", "[273-287)",">=287", "0",                       
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
           filter(untransformed_se!=0) %>%
           subset(., select= - c( birthwt, birthlen, vagbrth, hdlvry, mwtkg, single, fage, fhtcm, nrooms,  nhh, month, brthmon, feducyrs, anywast06, pers_wast, cleanck, impfloor, impsan,  safeh20, perdiar24, trth2o, perdiar6))

# Subset to primary outcomes of intervention analysis
d <- d %>% filter((agecat=="0-6 months" & intervention_level=="Maternal") | (agecat=="6-24 months" & intervention_level!="Maternal"))



#TEMP
d$N<-1
d$N_cases <-1




#------------------------------------------------
#Plot themes
#------------------------------------------------
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("black","#1F77B4","#FF7F0E","#2CA02C","#D62728", 
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


#------------------------------------------------
#Pooled estimate function
#------------------------------------------------

poolRR <- function(d, method="REML"){
    #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(Nstudies=n(), N_cases=sum(N_cases, na.rm=T), N=sum(N, na.rm=T))
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$Nstudies, N= nstudies$N, N_cases= nstudies$N_cases)
  }else{
    
  #cat(d$intervention_variable[1]," ", levels(d$agecat)[1]," ", d$intervention_level[1],"\n")
  fit<-NULL
  try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"),silent = TRUE)
  if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"),silent = TRUE)}
  if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"),silent = TRUE)}
  if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="EB", measure="RR"),silent = TRUE)}

  if(is.null(fit)){
        est <- data.frame(logRR.psi=NA, logSE=NA, RR=NA, RR.CI1=NA, RR.CI2=NA, Nstudies= nstudies$Nstudies, N= nstudies$N, N_cases= nstudies$N_cases)
  }else{
  est<-data.frame(fit$b, fit$se)
  colnames(est)<-c("logRR.psi","logSE")
  
  est$RR<-exp(est$logRR)
  est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
  est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
  
  est$Nstudies <- nstudies$Nstudies
    est$N <- nstudies$N
  est$N_cases <- nstudies$N_cases

  }
  }
  
  return(est)
}

#------------------------------------------------
#Study capitalization function
#------------------------------------------------
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}


#------------------------------------------------
#Forest plot function
#------------------------------------------------

# dfull<-d
# d<-dfull
# parameter="RR"
# measure="RR"
# EM="sex"
# EM_to_drop=c("enstunt", "meducyrs")
# yticks = c(0.12, 0.25,0.5,1,2,4,8,16,32,64, 128)
# yticks = c( 0.25,0.5,1,2,4)

KI_int_forest_plot <- function(d, parameter="RR", measure="RR", EM="sex",
                           EM_to_drop=c("enstunt"),
                           yticks =  c(0.25,0.5,1,2,4),
                           pdfname="Intervention Forest Plots.pdf"){
  

#Subset to relative risks
d <- d[d$type==parameter,]

#Drop unwanted risk factors
d <- d[!(d$intervention_variable %in% EM_to_drop),]

#Drop Mal-ED Tanzania HHwealth 6-24mo (only has 2 levels)
d <- d %>% filter(!(intervention_variable=="hhwealth_quart" & agecat=="6-24 months" & studyid=="ki0047075b-MAL-ED" & country=="TANZANIA, UNITED REPUBLIC OF"))



#Strip grant identifier and add country
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



#Pooled effects
  if(measure=="RR"){
    
    RMAest_RE <- d %>% group_by(intervention_variable, agecat, intervention_level, EMvar, EMlevel) %>%
                  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Random", region="Pooled", pooled=1) %>% as.data.frame()
    RMAest_FE <- d %>% group_by(intervention_variable, agecat, intervention_level, EMvar, EMlevel) %>%
                  do(poolRR(., method = "FE")) %>% mutate(studyid="Pooled - Fixed", region="Pooled", pooled=1) %>% as.data.frame()
    
    #merge in pooled effects
    d <- d %>% rename(RR=estimate, RR.CI1=ci_lower, RR.CI2=ci_upper) %>% 
      mutate(Nstudies=1, pooled=0, region=as.character(region)) %>% 
      subset(., select=c(studyid, country, region, intervention_variable,agecat,intervention_level, EMvar, EMlevel,
                        RR, RR.CI1, RR.CI2, pooled, Nstudies, N, N_cases, adjustment_set))
    #d <- bind_rows(d, RMAest_RE_africa, RMAest_RE_asia, RMAest_RE_latamer, RMAest_RE, RMAest_FE)
    d <- bind_rows(d, RMAest_RE, RMAest_FE)
  }
  
  
  if(measure=="ATE"){
    RMAest_RE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Random", region="Pooled", pooled=1) %>% as.data.frame()
    RMAest_FE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(., method = "FE")) %>% mutate(studyid="Pooled - Fixed", region="Pooled", pooled=1) %>% as.data.frame()
    
    
    #Add regional estimates
    RMAest_RE_africa <- d %>% filter(region=="Africa") %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Africa", region="Africa", pooled=1) %>% as.data.frame()
    RMAest_RE_asia <- d %>% ungroup() %>% filter(region=="Asia") %>% do(droplevels(.)) %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Asia", region="Asia", pooled=1) %>% as.data.frame()
    RMAest_RE_latamer <- d %>% filter(region=="Latin America") %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Latin America", region="Latin America", pooled=1) %>% as.data.frame()
    
    #merge in pooled effects
    d <- d %>% rename(ATE=estimate, ATE.CI1=ci_lower, ATE.CI2=ci_upper) %>% 
      mutate(Nstudies=1, pooled=0, region=as.character(region)) %>% 
      subset(., select=c(studyid, country, region, intervention_variable,agecat,intervention_level, baseline_level,
                        ATE, ATE.CI1, ATE.CI2, pooled, Nstudies, N, N_cases, adjustment_set))
    d <- bind_rows(d, RMAest_RE_africa, RMAest_RE_asia, RMAest_RE_latamer, RMAest_RE, RMAest_FE)

  }


  
  #Drop pooled other category:
d <- d %>% filter(!(pooled==1 & intervention_level=="Other"))  

#Drop pooled estimates from single studies
d <- d %>% filter(!(pooled==1 & Nstudies==1))  


#Order factors for plotting
d <- droplevels(d)
d$agecat <- as.character(d$agecat)
d$agecat[d$agecat=="Birth"] <- "At-birth prevalence"
d$agecat[d$agecat=="0-24 months"] <- "0-24 month\ncumulative incidence"
d$agecat[d$agecat=="0-6 months"] <- "0-6 month\ncumulative incidence"
d$agecat[d$agecat=="6 months"] <- "6 month prevalence"
d$agecat[d$agecat=="6-24 months"] <- "6-24 month\ncumulative incidence"
d$agecat[d$agecat=="24 months"] <- "24 month prevalence"
d$agecat <- factor(d$agecat, levels = c("At-birth prevalence","0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence","0-24 month\ncumulative incidence"))

unique(d$EMlevel)
d$EMlevel <- factor(d$EMlevel, 
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

unique(d$EMvar)
d$EMvar <- factor(d$EMvar, levels=c("birthlen","birthwt", "gagebrth",
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
unique(d$EMvar)

d$RFlabel <- NA
d$RFlabel[d$EMvar=="enwast"] <-  "Enrolled wasted"
d$RFlabel[d$EMvar=="gagebrth"] <-  "Gestational age at birth"
d$RFlabel[d$EMvar=="predexfd6"] <-  "Exclusive or Predominant breastfeed under 6 months"
d$RFlabel[d$EMvar=="mage"] <- "Mother's age" 
d$RFlabel[d$EMvar=="mhtcm"] <- "Mother's height" 
d$RFlabel[d$EMvar=="mwtkg"] <- "Mother's weight" 
d$RFlabel[d$EMvar=="mbmi"] <- "Mother's BMI" 
d$RFlabel[d$EMvar=="meducyrs"] <- "Mother's education" 
d$RFlabel[d$EMvar=="parity"] <-  "Birth order" 
d$RFlabel[d$EMvar=="hfoodsec"] <- "Household food security" 
d$RFlabel[d$EMvar=="nchldlt5"] <-   "Number of children <5 in household"
d$RFlabel[d$EMvar=="hhwealth_quart"] <-  "Household wealth" 
d$RFlabel[d$EMvar=="fage"] <- "Father's age" 
d$RFlabel[d$EMvar=="fhtcm"] <- "Father's height" 
d$RFlabel[d$EMvar=="birthwt"] <- "Birthweight (Z-scored)" 
d$RFlabel[d$EMvar=="birthlen"] <- "Birth length (Z-scored)" 
d$RFlabel[d$EMvar=="vagbrth"] <- "Vaginal birth" 
d$RFlabel[d$EMvar=="hdlvry"] <- "Child delivered at home" 
d$RFlabel[d$EMvar=="single"] <- "Single parent" 
d$RFlabel[d$EMvar=="nrooms"] <- "Number of rooms in household" 
d$RFlabel[d$EMvar=="nhh"] <- "Number of people in household" 
d$RFlabel[d$EMvar=="meducyrs"] <- "Maternal education quartile" 
d$RFlabel[d$EMvar=="feducyrs"] <- "Paternal education quartile" 
d$RFlabel[d$EMvar=="anywast06"] <- "Any wasting before 6 months age" 
d$RFlabel[d$EMvar=="pers_wast"] <- "Persistent wasting before 6 months age" 
d$RFlabel[d$EMvar=="trth2o"] <- "Treats drinking water" 
d$RFlabel[d$EMvar=="cleanck"] <- "Clean cooking fuel usage" 
d$RFlabel[d$EMvar=="impfloor"] <- "Improved floor" 
d$RFlabel[d$EMvar=="impsan"] <- "Improved sanitation" 
d$RFlabel[d$EMvar=="safeh20"] <- "Safe water source" 
d$RFlabel[d$EMvar=="perdiar6"] <- "Quartile of diarrhea longitudinal\nprevalence under 6 months" 
d$RFlabel[d$EMvar=="perdiar24"] <- "Quartile of diarrhea longitudinal\nprevalence under 24 months" 
d$RFlabel[d$EMvar=="earlybf"] <- "Breastfeed within an hour of birth" 

#d$intervention_variable[d$intervention_variable=="LNS"]
d$intervention_level <- factor(d$intervention_level, levels=c("LNS", "Maternal",  "Zinc",  "VitA", "Other"))

#order by region
d$region <- as.character(d$region)
d$region <- factor(d$region, levels=c("Pooled","Asia", "Latin America","Africa","Europe"))
d <- d[with(d, order(desc(pooled), region)),]
unique(d$studyid)

d$studyid <- as.character(d$studyid)
d$studyid <- factor(d$studyid, levels=unique(d$studyid))
d$order <- as.numeric(d$studyid)

d$study_arm <- paste0(d$studyid, ":\n", d$intervention_level)
d <- d[with(d, order(intervention_level, desc(pooled), region)),]
d$study_arm <- factor(d$study_arm, levels=unique(d$study_arm))


  if(measure=="RR"){Ylab="Relative Risk\nwith N's and number of cases printed on the left and\nthe set of adjustment variables printed on the right"}
  if(measure=="ATE"){Ylab="Average Treatment Effect"}

d <- droplevels(d)

#Print plots across all intervention arms

pdf(pdfname, height=12, width=20)

for(i in levels(d$EMvar)){
  
  df <- d[d$EMvar==i & !is.na(d$EMvar),]  
  
  df$studyid <- as.character(df$studyid)
  df <- df[with(df, order(desc(pooled), region, country, studyid, desc(EMlevel))),]
  df$studyid <- factor(df$studyid, levels=unique(df$studyid))
  df$study_arm_level <- paste0(df$study_arm," ",df$EMlevel)
  df$study_arm_level <- factor(df$study_arm_level, levels=unique(df$study_arm_level))

  # df$study_arm_level <- paste0(df$study_arm," ",df$EMlevel)
  # df <- df[with(df, order(desc(pooled), region, country, studyid, EMlevel)),]
  # df$study_arm_level <- factor(df$study_arm_level, levels=unique(df$study_arm_level))
  
  
    p <-  ggplot(df, aes(x=study_arm_level)) + 
          geom_point(aes(shape=factor(pooled), y=RR, fill=studyid, color=studyid), size = 4) +
          geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=studyid)) +
          coord_flip(ylim=range(yticks)) +
          labs(x = "Study-specific intervention average treatment effects stratified by effect modifier level", y = Ylab) +
          geom_hline(yintercept = 1) +
          #geom_vline(xintercept = 2.5, linetype=2) +
          #geom_vline(xintercept = Npooled+0.5) +
          # geom_text(aes(y=0.12, label=Ns), size=3,  hjust=0) +
          # geom_text(aes(y=16, label=adjustment_set), size=3,  hjust=0) +
          scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
          scale_shape_manual(values=c(21, 5)) +
          scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$studyid)) +
          scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$studyid)) +
          scale_size_continuous(range = c(0.5, 4))+
          theme(strip.background = element_blank(),
            legend.position="none",
            strip.text.x = element_text(size=12),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
          #facet_wrap(~EMlevel, ncol=1) +
          facet_wrap(~intervention_level, nrow=1, scales="free") +
          ggtitle(paste0("Effect modifier: ", df$EMvar[1]))
    print(p)
    
  }
dev.off()
}


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/Forest Plots")


head(d)
table(d$type)
table(d$agecat)

KI_int_forest_plot(d)
dev.off()


