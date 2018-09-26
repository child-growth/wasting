



#-----------------------------------
# Wasting webinar plots
#-----------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
library(ggthemes)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# load base functions
source("C:/Users/andre/Documents/HBGDki/Wasting/1-outcomes/0_st_basefunctions.R")

load("C:/Users/andre/Documents/HBGDki/Results/Wasting_descriptive_epi_results.Rdata")

setwd("C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures/")


#-------------------------------------------------------------------------------------------
#Prevalence
#-------------------------------------------------------------------------------------------

clean_nmeans<-function(nmeas){
  nmeas <- round(nmeas/1000)
  nmeas.f <- paste0("N=",nmeas,"K children")
  return(nmeas.f)
}

clean_agecat<-function(agecat){
  agecat <- as.character(agecat)
  agecat <- gsub("months","mo.", agecat)
  agecat <- factor(agecat, levels=unique(agecat))
  return(agecat)
}

prev.res <- prev.data$prev.res

prev.res$nmeas.f <- clean_nmeans(prev.res$nmeas)
prev.res$agecat <- clean_agecat(prev.res$agecat)


p1 <- ggplot(prev.res,aes(y=est,x=agecat)) +
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  # scale_fill_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  # scale_colour_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  scale_color_manual(values=rep(tableau10[1],20))+  scale_fill_manual(values=rep(tableau10[1],20))+
  xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(-4,60))+
  annotate("text",x=prev.res$agecat,y=0,label=prev.res$nmeas.f,size=3)+
  annotate("text",x=prev.res$agecat,y=-3,label=prev.res$nstudy.f,size=3)+
  annotate("text",label=prev.res$ptest.f,x=prev.res$agecat,
           y=prev.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled point prevalence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

ggsave(p1, file="pooled_prev.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Cumulative Incidence
#-------------------------------------------------------------------------------------------

ci.res <- ci.data$ci.res

ci.res$nmeas.f <- clean_nmeans(ci.res$nmeas)
ci.res$agecat.f <- clean_agecat(ci.res$agecat)

p2 <- ggplot(ci.res, aes(y=est,x=agecat.f, color=agecat.f)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[2],20))+  scale_fill_manual(values=rep(tableau10[2],20))+
  scale_y_continuous(limits=c(0,100))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=5,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

ggsave(p2, file="pooled_CI.png", width=10, height=4)



#-------------------------------------------------------------------------------------------
# Cumulative Incidence
#-------------------------------------------------------------------------------------------

ci.res <- ci.data.nobirth$ci.res

ci.res$nmeas.f <- clean_nmeans(ci.res$nmeas)
ci.res$agecat.f <- clean_agecat(ci.res$agecat)

p2_nobirth <- ggplot(ci.res, aes(y=est,x=agecat.f, color=agecat.f)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[2],20))+  scale_fill_manual(values=rep(tableau10[2],20))+
  scale_y_continuous(limits=c(0,100))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=5,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

ggsave(p2_nobirth, file="pooled_CI_no_birth.png", width=10, height=4)


#-------------------------------------------------------------------------------------------
# Recovery
#-------------------------------------------------------------------------------------------

rev.res <- rec.data$ci.res

rev.res$agecat <- clean_agecat(rev.res$agecat)


p3 <- ggplot(rev.res,aes(y=est,x=agecat))+
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[3],20))+  scale_fill_manual(values=rep(tableau10[3],20))+
  xlab("Age category")+ ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,75))+
  annotate("text",x=rev.res$agecat,y=1.2,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat,y=0.1,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat,
           y=rev.res$est,hjust=-0.4,size=3)+
  ggtitle("Percentage of children who were wasted and recovered within age intervals")+
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

ggsave(p3, file="pooled_rev.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Incidence rate
#-------------------------------------------------------------------------------------------

df <- ir.data$ir.res

df$nmeas.f <- paste0(round(df$nmeas/1000, 0),rep("K at-risk",8))
df$agecat.f2 <- clean_agecat(df$agecat)


p4 <- ggplot(df, aes(y=est,x=agecat.f2, fill=agecat.f2, color=agecat.f2))+
  geom_point( size = 4, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb, ymax=ub), alpha=0.5, size = 3) +
  scale_y_continuous(limits=c(-2,10))+
  xlab("Age category") +
  ylab("Percent wasted (95% CI)") +
  annotate("text",x=df$agecat.f2,y=1,label=df$nmeas.f,size=3) +
  annotate("text",x=df$agecat.f2,y=-2,label=df$nstudy.f,size=3) +
  annotate("text",x=df$agecat.f2, y=5,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
  annotate("text",label=df$ptest.f,x=df$agecat.f2, y=df$est,hjust=-2,size=3)+
  theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
  scale_colour_manual(values=rep(tableau10[10],20)) +
  ggtitle("Pooled cumulative incidence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p4
ggsave(p4, file="pooled_inc_rate.png", width=10, height=4)


#-------------------------------------------------------------------------------------------
# Incidence rate- no birth
#-------------------------------------------------------------------------------------------

df <- ir.data.nobirth$ir.res

df$nmeas.f <- paste0(round(df$nmeas/1000, 0),rep("K at-risk",8))
df$agecat.f2 <- clean_agecat(df$agecat)


p4_nobirth <- ggplot(df, aes(y=est,x=agecat.f2, fill=agecat.f2, color=agecat.f2))+
  geom_point( size = 4, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb, ymax=ub), alpha=0.5, size = 3) +
  scale_y_continuous(limits=c(-2,10))+
  xlab("Age category") +
  ylab("Percent wasted (95% CI)") +
  annotate("text",x=df$agecat.f2,y=1,label=df$nmeas.f,size=3) +
  annotate("text",x=df$agecat.f2,y=-2,label=df$nstudy.f,size=3) +
  annotate("text",x=df$agecat.f2, y=5,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
  annotate("text",label=df$ptest.f,x=df$agecat.f2, y=df$est,hjust=-2,size=3)+
  theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
  scale_colour_manual(values=rep(tableau10[10],20)) +
  ggtitle("Pooled cumulative incidence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p4_nobirth
ggsave(p4_nobirth, file="pooled_inc_rate_no_birth.png", width=10, height=4)






#-------------------------------------------------------------------------------------------
# Duration
#-------------------------------------------------------------------------------------------

vel <- rbind(data.frame(measure="Length velocity (cm per month)" , poollencm[poollencm$stratacol=="pooled" & poollencm$region=="Overall",]),
             data.frame(measure="LAZ change (Z-score per month)" , poolhaz[poolhaz$stratacol=="pooled" & poolhaz$region=="Overall",]))


vel$nmeas.f <- clean_nmeans(vel$N)
vel$strata <- clean_agecat(vel$strata)


p7 <- ggplot(vel, aes(y=Mean,x=strata))+
  geom_point(aes(fill=strata, color=strata), size = 4) +
  geom_linerange(aes(ymin=Lower.95.CI, ymax=Upper.95.CI, color=strata),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[4],20))+  
  xlab("Age category")+ ylab("")+
  #scale_y_continuous(limits=c(0,20))+
  # annotate("text",x=vel$strata,y=.12,label=vel$nmeas.f,size=3)+
  # annotate("text",x=vel$strata,y=0.1,label=vel$nstudy.f,size=3)+
  facet_wrap(~measure, scales="free_y") +
  ggtitle("")+
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 25, hjust = 1)) 

ggsave(p7, file="pooled_velocity.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Mean LAZ
#-------------------------------------------------------------------------------------------

clean_nmeans<-function(nmeas){
  nmeas <- round(nmeas/1000)
  nmeas.f <- paste0("N=",nmeas,"K children")
  return(nmeas.f)
}

clean_agecat<-function(agecat){
  agecat <- as.character(agecat)
  agecat <- gsub("months","mo.", agecat)
  agecat <- factor(agecat, levels=unique(agecat))
  return(agecat)
}

laz.res$nmeas.f <- clean_nmeans(laz.res$nmeas)
laz.res$agecat <- clean_agecat(laz.res$agecat)


p8 <- ggplot(laz.res,aes(y=est,x=agecat)) +
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  geom_hline(yintercept=0) +
  # scale_fill_tableau(drop=TRUE, limits = levels(laz.res$agecat)) +
  # scale_colour_tableau(drop=TRUE, limits = levels(laz.res$agecat)) +
  scale_color_manual(values=rep(tableau10[9],20))+  scale_fill_manual(values=rep(tableau10[9],20))+
  xlab("Age category")+
  ylab("Mean Z-score")+
  scale_y_continuous(limits=c(-3,0.5))+
  annotate("text",x=laz.res$agecat,y=-2.8, label=laz.res$nmeas.f,size=3)+
  annotate("text",x=laz.res$agecat,y=-3, label=laz.res$nstudy.f,size=3)+
  annotate("text",label=round(laz.res$est,2),x=laz.res$agecat,
           y=laz.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled mean LAZ") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p8

ggsave(p8, file="pooled_laz.png", width=10, height=4)







#-------------------------------------------------------------------------------------------
# Risk factor plots 
#-------------------------------------------------------------------------------------------


setwd("C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures/")

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
  #ggtitle("Prenatal characteristics associated with\nwasting from birth to 2 years")
  ggtitle("Outcome: any wasting from birth to 2 years")

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
  ggtitle("Outcome: any wasting by 2 years, excluding wasting at birth")

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
  ggtitle("Outcome: any wasting from 6 months to 2 years")


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
  ggtitle("Associations between wasting before 6 months\nand wasting from 6 months to 2 years")


ggsave(p, file="Wasting_as_a_RF_plots.png",  width=6, height=4.2)



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
  ggtitle("Percentage of children who became wasted and were recovered at 36 months") +
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
  ggtitle("Percentage of children who became wasted and were recovered at 36 months") +
  theme(legend.position = "none")


ggsave(prec, file="pooled_rec24.png", width=10, height=4)
