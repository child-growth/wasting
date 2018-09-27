

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(metafor)


load("C:/Users/andre/Documents/HBGDki/Results/wasting_results.rdata")


d <- results

allvars<-unique(d$intervention_variable)

head(d)

#Subset to relative risks
d <- d %>% filter(type=="RR")


#Subset to adjusted
d <- d %>% filter(adjustment_set!="unadjusted" | intervention_variable=="sex")

missing <- allvars[!(allvars %in% unique(d$intervention_variable))]
missing

#Subset to primary outcomes
table(d$agecat)

d <- d %>% filter(agecat=="6-24 months")


#Drop enrolled wasted as a RF for wasting
d <- d %>% filter(intervention_variable!="enwast")

#Look at prevalence and incidence
d <- d %>% filter(outcome_variable=="ever_wasted")

table(d$intervention_variable)



d <- d %>% filter(intervention_variable=="sex")

#Drop duplicates
d <- distinct(d, strata_label, intervention_level, .keep_all=T)

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
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1)
  }else{
    
    #cat(d$intervention_variable[1]," ", levels(d$agecat)[1]," ", d$intervention_level[1],"\n")
    fit<-NULL
    try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"),silent = TRUE)
    if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"),silent = TRUE)}
    if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"),silent = TRUE)}
    if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="EB", measure="RR"),silent = TRUE)}
    
    if(is.null(fit)){
      est <- data.frame(logRR.psi=NA, logSE=NA, RR=NA, RR.CI1=NA, RR.CI2=NA)
    }else{
      est<-data.frame(fit$b, fit$se)
      colnames(est)<-c("logRR.psi","logSE")
      
      est$RR<-exp(est$logRR)
      est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
      est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
      
      
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

parameter="RR"
agerange="6-24 months"
measure="RR"
RF_to_drop=c("enstunt", "trth2o")
yticks = c(0.12, 0.25,0.5,1,2,4,8,16,32,64, 128)



#Subset to relative risks
d <- d[d$type==parameter,]


#Subset to age of outcome
#d <- d[d$agecat==agerange,]
d <- d %>% filter(agecat=="6-24 months")



#Strip grant identifier and add country
d$studyid <- gsub("^k.*?-" , "", d$studyid)
d$studyid <- paste0(d$studyid, ", ", paste0(substring(as.character(d$country),1,1), tolower(substring(as.character(d$country),2))))
d$studyid <- gsub("Tanzania, united republic of", "Tanzania", d$studyid)
d$studyid <- gsub("africa", "Africa", d$studyid)

#Add Region
d <- d %>% mutate(Region = case_when(
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

RMAest_RE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Random", Region="Pooled", pooled=1) %>% as.data.frame()
RMAest_FE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
  do(poolRR(., method = "FE")) %>% mutate(studyid="Pooled - Fixed", Region="Pooled", pooled=1) %>% as.data.frame()


#Add Regional estimates
RMAest_RE_africa <- d %>% filter(Region=="Africa") %>% 
  group_by(intervention_variable, agecat, intervention_level) %>%
  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Africa", Region="Africa", pooled=1) %>% as.data.frame()
RMAest_RE_asia <- d %>% ungroup() %>% filter(Region=="Asia") %>% do(droplevels(.)) %>% 
  group_by(intervention_variable, agecat, intervention_level) %>%
  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Asia", Region="Asia", pooled=1) %>% as.data.frame()
RMAest_RE_latamer <- d %>% filter(Region=="Latin America") %>% 
  group_by(intervention_variable, agecat, intervention_level) %>%
  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Latin America", Region="Latin America", pooled=1) %>% as.data.frame()

#merge in pooled effects
d <- d %>% rename(RR=estimate, RR.CI1=ci_lower, RR.CI2=ci_upper) %>% 
  mutate(Nstudies=1, pooled=0, Region=as.character(Region)) %>% 
  subset(., select=c(studyid, country, Region, intervention_variable,agecat,intervention_level, baseline_level,
                     RR, RR.CI1, RR.CI2, pooled, adjustment_set))
d <- bind_rows(d, RMAest_RE_africa, RMAest_RE_asia, RMAest_RE_latamer, RMAest_RE, RMAest_FE)





table(d$agecat)

d <- droplevels(d)
d$agecat <- as.character(d$agecat)

d$agecat[grepl("-",d$agecat)] <- paste0(d$agecat[grepl("-",d$agecat)],"\ncumulative incidence")
d$agecat[!grepl("-",d$agecat)] <- paste0(d$agecat[!grepl("-",d$agecat)]," prevalence")

#d$agecat <- factor(d$agecat, levels = c("0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence"))
d$agecat <- factor(d$agecat, levels=c("3 months prevalence", "3-6 months\ncumulative incidence", "0-6 months (no birth st.)\ncumulative incidence","0-6 months\ncumulative incidence",
                                      "6 months prevalence","6-9 months\ncumulative incidence","9 months prevalence","9-12 months\ncumulative incidence","12 months prevalence",
                                      "12-15 months\ncumulative incidence","15 months prevalence","15-18 months\ncumulative incidence","18 months prevalence",
                                      "0-24 months (no birth st.)","6-24 months\ncumulative incidence","0-24 months (no birth st.)\ncumulative incidence","0-24 months\ncumulative incidence","24 months prevalence"))





unique(d$intervention_level)
d$intervention_level <- factor(d$intervention_level, 
                               levels=c("0","1",
                                        "<48 cm" , "[48-50) cm",                                    
                                        "Low birth weight","Normal or high birthweight", 
                                        "2","3","4","5","6","7","8","9",  "10" , "11","12" ,
                                        "<32" , "[32-38)", ">=38",
                                        "Low", "Medium", "High",                    
                                        "<162 cm", "[162-167) cm" , ">=167 cm",
                                        "Preterm", "Early term", "Full or late term",           
                                        "Food Insecure", "Mildly Food Insecure", "Food Secure",               
                                        "Wealth Q1", "Wealth Q2", "Wealth Q3", "Wealth Q4",
                                        "<25","[25-30)",">=30",                      
                                        "Underweight", "Normal weight", "Overweight or Obese",
                                        "<151 cm", "[151-155) cm", ">=155 cm",
                                        "<52 kg", "[52-58) kg", ">=58 kg",
                                        "2+","3 or less","4-5","6-7","8+","3+","4+",                                                 
                                        "0%","(0%, 5%]",">5%","Female","Male",
                                        "WHZ Q1", "WHZ Q2", "WHZ Q3", "WHZ Q4"))



unique(d$intervention_variable)
d$intervention_variable <- factor(d$intervention_variable,
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
unique(d$intervention_variable)

d$RFlabel <- NA
d$RFlabel[d$intervention_variable=="sex"] <-  "Gender"
d$RFlabel[d$intervention_variable=="enwast"] <-  "Enrolled wasted"
d$RFlabel[d$intervention_variable=="gagebrth"] <-  "Gestational age at birth"
d$RFlabel[d$intervention_variable=="predexfd6"] <-  "Exclusive or Predominant breastfeeding under 6 months"
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
d$RFlabel[d$intervention_variable=="birthwt"] <- "Birthweight (kg)" 
d$RFlabel[d$intervention_variable=="birthlen"] <- "Birth length (cm)" 
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
d$RFlabel[d$intervention_variable=="predfeed3"] <-  "Predominant breastfeeding under 3 months"
d$RFlabel[d$intervention_variable=="predfeed36"] <-  "Predominant breastfeeding from 3-6 months"
d$RFlabel[d$intervention_variable=="predfeed6"] <-  "Predominant breastfeeding under 6 months"
d$RFlabel[d$intervention_variable=="exclfeed3"] <-  "Exclusive breastfeeding under 3 months"
d$RFlabel[d$intervention_variable=="exclfeed36"] <-  "Exclusive breastfeeding from 3-6 months"
d$RFlabel[d$intervention_variable=="exclfeed6"] <-  "Exclusive breastfeeding under 6 months"
d$RFlabel[d$intervention_variable=="month"] <-  "Month of measurement"
d$RFlabel[d$intervention_variable=="brthmon"] <-  "Birth month"
d$RFlabel[d$intervention_variable=="lag_WHZ_quart"] <-  "Mean WHZ in the prior 3 months"


#order by Region
d$Region <- as.character(d$Region)
d$Region <- factor(d$Region, levels=c("Pooled","Asia", "Latin America","Africa","Europe"))
d <- d[with(d, order(desc(pooled), Region)),]
unique(d$studyid)

d$studyid <- as.character(d$studyid)
d$studyid <- factor(d$studyid, levels=unique(d$studyid))
d$order <- as.numeric(d$studyid)

#Clean up adjustment sets (drop W_ and the missingness indicators)
d$adjustment_set <- gsub("W_","",d$adjustment_set)
#d$adjustment_set <- gsub("delta_","m_",d$adjustment_set)
for(i in 1:30){d$adjustment_set <- gsub("(delta_).*?,", "", d$adjustment_set)}
d$adjustment_set <- gsub("(delta_).*?", "", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)

for(i in 1:nrow(d)){
  if(nchar(d$adjustment_set[i])>50 & !is.na(d$adjustment_set[i])){
    d$adjustment_set[i] <- paste0(substr(d$adjustment_set[i],1,50),"\n",substr(d$adjustment_set[i],50, nchar(d$adjustment_set[i])))
  }
}

if(measure=="RR"){Ylab="Relative Risk\nwith N's and number of cases printed on the left and\nthe set of adjustment variables printed on the right"}
if(measure=="ATE"){Ylab="Average Treatment Effect"}

d <- droplevels(d)






# Make plots
levels(d$intervention_variable)

i  <- "sex"

df <- d[d$intervention_variable==i,]  


df$studyid <- as.character(df$studyid)

#Add reference N's to the studyid label
df <- df %>% fill(baseline_level) %>%  fill(baseline_level, .direction="up") 

if(measure=="RR"){
  df$studyid2 <- paste0(df$studyid, ": ",df$N_cases," cases / ",df$N)
  df$studyid2[df$pooled==1] <- paste0(df$studyid[df$pooled==1], ": ",df$Nstudies[df$pooled==1]," studies, ",df$N_cases[df$pooled==1]," cases / ",df$N[df$pooled==1])
  df$studyid2[df$intervention_level!=df$baseline_level] <- NA
}else{
  df$Ns <- paste0("N= ",df$N," Mean=",df$mean)
  df$studyid2 <- paste0(df$studyid, ": ","N= ",df$N," Mean=",df$mean)
  df$studyid2[df$intervention_level!=df$baseline_level] <- NA
  df$studyid2[df$pooled==1] <- df$studyid[df$pooled==1]
}

df <- df %>% 
  group_by(studyid) %>% 
  fill(studyid2) %>%  fill(studyid2, .direction="up") %>% 
  ungroup()

table(df$studyid2)

df <- df[with(df, order(desc(pooled), Region)),]
df$studyid <- factor(df$studyid, levels=unique(df$studyid))
df$studyid2 <- factor(df$studyid2, levels=unique(df$studyid2))

df <- df %>% filter(RR.CI1!=RR.CI2) #drop reference level
df <- droplevels(df)
Npooled <- sum(df$pooled)/length(unique(df$intervention_level))
df$pooled <- factor(df$pooled)

stdyID <- levels(df$studyid)
Nstud <- addNA(factor(df$adjustment_set))

if(measure=="RR"){
  df$Ns <- paste0(df$N_cases," cases / ",df$N)
  df$Ns[df$pooled==1] <- paste0(df$Nstudies[df$pooled==1]," studies, ",df$N_cases[df$pooled==1]," cases / ",df$N[df$pooled==1])
  df$Ns[is.na(df$N)] <- ""
}else{
  df$Ns <- paste0("N= ",df$N," Mean=",df$mean)
  df$Ns[is.na(df$N)] <- ""
}


yticks <- c( 0.50, 1.00, 2.00, 4.00, 8.00)
#hbgdki pallet
tableau10 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
#scale_fill_manual(values=Tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
  
  
  df$agecat <- "6-24 months cumulative incidence)"
  Ylab <- "Relative Risk of boys compared to girls"
  
  p <-  ggplot(df, aes(x=studyid)) + 
    #geom_point(aes(y=RR, fill=Region, color=Region), size = 4, shape= ifelse(df$pooled==1,5,6)) +
    geom_point(aes(shape=pooled, y=RR, fill=Region, color=Region), size = 4) +
    geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=Region)) +
    coord_flip(ylim=range(0.5,8)) +
    #labs(x = "Study-specific results stratified by risk factor level\nwith reference category N's and cases printed", y = Ylab) +
    labs(x = "Cohort", y = Ylab) +
    geom_hline(yintercept = 1) +
    geom_vline(xintercept = 2.5, linetype=2) +
    geom_vline(xintercept = Npooled+0.5) +
    #geom_text(aes(y=0.5, label=Ns), size=3,  hjust=0) +
    #geom_text(aes(y=3, label=adjustment_set), size=3,  hjust=0) +
    scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
    #scale_x_discrete(labels= df$studyid2) +
    scale_shape_manual(values=c(21, 23)) +
    scale_colour_manual(values=tableau10) +
    scale_fill_manual(values=tableau10) +
    scale_size_continuous(range = c(0.5, 1))+
    theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
    # facet_wrap(~intervention_level, ncol=1) +
    # ggtitle(paste0("Risk factor: ", df$RFlabel[1], "\n",
    #                "Reference level: ", df$baseline_level[!is.na(df$baseline_level)], "\n", 
    #                "Outcome: ", df$agecat))
    ggtitle("Associations between gender and wasting\nincidence from 6-24 months:\nStudy-specific and pooled results") +guides(shape=FALSE)
  print(p)
  
  
  
  ggsave(p, file="C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures/example_forest_plot_wasting.png", width=6, height=5)
  
  
  
  
  