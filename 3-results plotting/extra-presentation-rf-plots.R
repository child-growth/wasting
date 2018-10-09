


rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")
load("C:/Users/andre/Documents/HBGDki/Results/webinar_results.rdata")

d <- results

head(d)
table(d$type)

#Subset to relative risks
d <- d %>% filter(type=="RR")


#Subset to adjusted
d <- d %>% filter(adjustment_set!="unadjusted" | intervention_variable=="sex")

#Subset to primary outcomes
table(d$agecat)

#Look at prevalence and incidence
table(d$outcome_variable)
#d <- d %>% filter(outcome_variable=="ever_wasted" | outcome_variable=="wasted")



head(d)



RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level,outcome_variable) %>%
  do(poolRR(.)) %>% as.data.frame()


RMAest





#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
         paste, collapse="\n")
}  

yticks <- c(2/3, 1, 3/2,2,3,4)



#Plot EBF x BW interaction
df <- RMAest %>% filter(intervention_variable=="birthwtXexclfeed6")
df$agecat <- factor(df$agecat, levels=c("6 months","6-24 months","24 months"))

df$intervention_level <- as.character(df$intervention_level)
df$intervention_level[df$intervention_level=="Normal or high birthweight X EBF"] <- "Normal BW X EBF"
df$intervention_level[df$intervention_level=="Normal or high birthweight X No EBF"] <- "Normal BW X No EBF"
df$intervention_level[df$intervention_level=="Low birth weight X EBF"] <- "Low BW X EBF"
df$intervention_level[df$intervention_level=="Low birth weight X No EBF"] <- "Low BW X No EBF"
df$intervention_level <- factor(df$intervention_level, levels=c( "Normal BW X EBF", 
                                                                 "Normal BW X No EBF",
                                                                "Low BW X EBF",
                                                                "Low BW X No EBF"))
df <- df %>% arrange(intervention_level)

p <- ggplot(df, aes(x=intervention_level)) +
  geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                 alpha=0.5, size = 3) +
  facet_wrap(~agecat, scales="free_x", nrow=1, labeller = label_wrap) +
  labs(x = "Exclusive breastfeeding stratified by birthweight",
       y = "Relative Risk\nof Stunting") +
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
  ggtitle("Associations between nstunting and combinations of\nbirthweight and exclusive breastfeeding before 6 months (EBF)")
p

ggsave(p, file="Stunting_byBWxEBF.png",  width=7, height=5)



#Plot wasting and stunting among those born wasted or stunted
df <- RMAest %>% filter(intervention_variable=="born_stunted" | intervention_variable=="born_wasted")

df$status <- NA
df$status[df$intervention_variable=="born_stunted"] <- "Born stunted and recovered"
df$status[df$intervention_variable=="born_wasted"] <- "Born wasted"

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
         paste, collapse="\n")
}  
p <- ggplot(df[df$outcome_variable=="ever_stunted",], aes(x=intervention_level)) +
  geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                 alpha=0.5, size = 3) +
  facet_grid(status~agecat,  labeller = label_wrap) +
  labs(x = "Birth status",
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
  ggtitle("Risk of stunting among\nchildren born stunted or wasted")
p

setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar")
ggsave(p, file="anthro_at_birth_RFplot.png",  width=6, height=5.2)


