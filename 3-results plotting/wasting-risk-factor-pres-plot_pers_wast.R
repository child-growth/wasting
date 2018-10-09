




rm(list=ls())
library(tidyverse)
library(metafor)

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")
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

d <- d %>% filter(agecat=="6-24 months"| agecat=="0-24 months (no birth wast"| agecat=="0-24 months")


#Drop enrolled wasted as a RF for wasting
d <- d %>% filter(intervention_variable!="enwast" & intervention_variable!="perdiar24"  & intervention_variable!="predfeed3"  & intervention_variable!="predfeed36"  & intervention_variable!="predfeed6")

#Look at persistant wasting
d <- d %>% filter(outcome_variable=="pers_wast")





RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
  do(poolRR(.)) %>% as.data.frame()





#Clean dataframe
RMAest$agecat <- as.character(RMAest$agecat)
RMAest$agecat[RMAest$agecat=="0-24 months (no birth wast)"] <- "0-24 months"
RMAest$agecat[RMAest$agecat=="0-6 months (no birth wast)"] <- "0-6 months"

#Drop nonsensical exposure-outcome pairs
RMAest <- RMAest[!((RMAest$agecat=="0-6 months" | RMAest$agecat=="Birth") & RMAest$intervention_variable %in% c("exclfeed3","exclfeed36","exclfeed6","perdiar6","predexfd6","predfeed3","predfeed36","predfeed6")),]
RMAest <- RMAest[!(RMAest$agecat=="Birth" & (RMAest$intervention_variable=="birthwt"| RMAest$intervention_variable=="earlybf"| RMAest$intervention_variable=="birthlen"| RMAest$intervention_variable=="gagebrth")),] 


RMAest <- RMA_clean(RMAest, outcome="pers_wasted", 
                    agecatlevels=c("Birth prevalence","0-6 months\npersistant wasting","6 months prevalence", "6-24 months\npersistant wasting",  "0-24 months\npersistant wasting","24 months prevalence"))
RMAest$RFlabel[RMAest$RFlabel=="Exclusive or Predominant breastfeeding under 6 months"] <- "Exclusive or Predominant\nbreastfeeding under 6 months" 

RMAest$intervention_variable <- factor(RMAest$intervention_variable)

#Add the reference level to the RFlabel
#RMAest$RFlabel <- paste0(RMAest$RFlabel," (Ref:",RMAest$baseline,")")


d <- RMAest



#Set order of RF categories
unique(d$RFtype)
d$RFtype <- factor(d$RFtype , levels=c("Parent", "Parent\nanthro", "Birth", "Child", "Breast-\nfeeding", "SES", "House-\nhold", "WASH"))         
d <- d %>% arrange(RFtype)

#Maybe make heatmap with 4 columns, leave blank if less than 4 levels, and color grey for reference level.
#Print the name of the category in the box?

RMAestfull <- d %>% group_by(agecat, intervention_variable) %>% arrange(intervention_level) %>% 
  mutate(order=row_number()+1) %>% as.data.frame()


#Subset to outcome and agecat of interest
groups024 <- c("Parent","Parent\nanthro","Birth","SES","House-\nhold","Child")  
groups624 <- c("Child","Breast-\nfeeding","WASH")


#Subset to outcome and agecat of interest
RMAestfull <- RMAestfull %>% filter((agecat=="0-24 months\npersistant wasting" & RFtype %in% groups024) |
                                      (agecat=="6-24 months\npersistant wasting" & RFtype %in% groups624)) %>%
  filter(!(intervention_variable=="sex" & agecat=="6-24 months\npersistant wasting")) %>%
  filter(!(intervention_variable=="parity" & agecat=="6-24 months\npersistant wasting")) %>%
  filter(!(intervention_variable=="perdiar6" & agecat=="0-24 months\npersistant wasting"))


unique(RMAestfull$intervention_variable)








setwd("C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures/")
theme_set(theme_bw())
label_wrap <- function(variable, value){
  lapply(strwrap(as.character(value), width=21, simplify=FALSE), 
         paste, collapse="\n")
}  

yticks <- c(2/3, 1, 3/2,2,3,4)








vars <- c("mage", "mwtkg","birthwt","sex")


df <- RMAestfull %>% filter(intervention_variable %in% vars)
df$intervention_variable <- factor(df$intervention_variable, levels=vars)
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(as.character(df$RFlabel), levels=unique(as.character(df$RFlabel)))


p <-  ggplot(df, aes(x=intervention_level)) + 
  geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                 alpha=0.5, size = 3) +
  facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
  labs(x = "Exposure level", y = "Relative risk") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=0.9, label=Nstudies), size=3,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Outcome: persistant wasting from birth to 2 years")
p

ggsave(p, file="pers_wasting_pres_RFplot1.png",  width=6, height=6)










vars <- c("meducyrs","hhwealth_quart","hfoodsec","parity")


df <- RMAestfull %>% filter(intervention_variable %in% vars)
df$intervention_variable <- factor(df$intervention_variable, levels=vars)
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(as.character(df$RFlabel), levels=unique(as.character(df$RFlabel)))

p <-  ggplot(df, aes(x=intervention_level)) + 
  geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                 alpha=0.5, size = 3) +
  facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
  labs(x = "Exposure level", y = "Relative risk") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=2.4, label=Nstudies), size=3,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=10, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Outcome: persistant wasting from birth to 2 years")
p

ggsave(p, file="pers_wasting_pres_RFplot2.png",  width=6, height=6)

