

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
#source("C:/Users/andre/Documents/HBGDki/Wasting/1-outcomes/0_st_basefunctions.R")

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

#combine regular and severe wasting
prev.res <- rbind(data.frame(Status="Wasted",prev.data$prev.res), data.frame(Status="Severely\nWasted",sev.prev.data$prev.res))

prev.res$nmeas.f <- clean_nmeans(prev.res$nmeas)
prev.res$agecat <- clean_agecat(prev.res$agecat)



p1 <- ggplot(prev.res,aes(y=est,x=agecat, group=Status)) + 
  geom_point(aes(fill=Status, color=Status), size = 4, position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Status),
                 alpha=0.5, size = 3, position = position_dodge(width = 0.2)) +
  # scale_fill_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  # scale_colour_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  scale_color_manual(values=rep(tableau10[c(1,4)],20)) + 
  xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(0,16))+
  annotate("text",x=prev.res$agecat,y=15.2,label=prev.res$nmeas.f,size=3)+
  annotate("text",x=prev.res$agecat,y=16,label=prev.res$nstudy.f,size=3)+
  annotate("text",label=prev.res$ptest.f,x=prev.res$agecat,
           y=prev.res$est,hjust=-2,size=3)+
  ggtitle("Pooled point prevalence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p1
ggsave(p1, file="pooled_prev.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Cumulative Incidence
#-------------------------------------------------------------------------------------------

ci.res <- ci.data$ci.res
ci.res <- ci.data.nobirth$ci.res

ci.res$nmeas.f <- clean_nmeans(ci.res$nmeas)
ci.res$agecat.f <- clean_agecat(ci.res$agecat)

p2 <- ggplot(ci.res, aes(y=est,x=agecat.f, color=agecat.f)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[2],20))+  scale_fill_manual(values=rep(tableau10[2],20))+
  scale_y_continuous(limits=c(0,25))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=3,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=0,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of wasting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p2
ggsave(p2, file="pooled_CI.png", width=10, height=4)



#-------------------------------------------------------------------------------------------
# Cumulative Incidence - no birth
#-------------------------------------------------------------------------------------------

ci.res <- ci.data.nobirth$ci.res

ci.res$nmeas.f <- clean_nmeans(ci.res$nmeas)
ci.res$agecat.f <- clean_agecat(ci.res$agecat)

p2_nobirth <- ggplot(ci.res, aes(y=est,x=agecat.f, color=agecat.f)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[2],20))+  scale_fill_manual(values=rep(tableau10[2],20))+
  scale_y_continuous(limits=c(0,10))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=0,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of wasting- excluding wasting at birth") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p2_nobirth
ggsave(p2_nobirth, file="pooled_CI_no_birth.png", width=10, height=4)


#-------------------------------------------------------------------------------------------
# Recovery
#-------------------------------------------------------------------------------------------

# rev.res <- rec.data$ci.res
# 
# rev.res$agecat <- clean_agecat(rev.res$agecat)
# 
# 
# p3 <- ggplot(rev.res,aes(y=est,x=agecat))+
#   geom_point(aes(fill=agecat, color=agecat), size = 4) +
#   geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
#                  alpha=0.5, size = 3) +
#   scale_color_manual(values=rep(tableau10[3],20))+  scale_fill_manual(values=rep(tableau10[3],20))+
#   xlab("Age category")+ ylab("Percentage (95% CI)")+
#   scale_y_continuous(limits=c(0,13))+
#   annotate("text",x=rev.res$agecat,y=1.2,label=rev.res$nmeas.f,size=3)+
#   annotate("text",x=rev.res$agecat,y=0.1,label=rev.res$nstudy.f,size=3)+
#   annotate("text",label=rev.res$ptest.f,x=rev.res$agecat,
#            y=rev.res$est,hjust=-0.4,size=3)+
#   ggtitle("Percentage of children who were wasted and recovered within age intervals")+
#   theme(strip.background = element_blank(),
#         legend.position="none",
#         strip.text.x = element_text(size=12),
#         axis.text.x = element_text(size=12)) 
# p3
# ggsave(p3, file="pooled_rev.png", width=10, height=4)

# Recovery in 30,60,90 days
rev.res <- rbind(rec.data30$ci.res,rec.data60$ci.res,rec.data90$ci.res)
rev.res$length <- factor(c(rep("Recovery within 30 days",4),rep("Recovery within 60 days",4),rep("Recovery within 90 days",4)))
rev.res$agecat <- clean_agecat(rev.res$agecat)
p3 <- ggplot(rev.res,aes(y=est,x=agecat, group=length))+
  geom_point(aes(fill=length, color=length), size = 4, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=length),
                 alpha=0.5, size = 3, position=position_dodge(width=0.25)) +
  scale_color_manual(values=rep(tableau10[c(6,3,5)],20), name = "Recovery Time")+  
  scale_fill_manual(values=rep(tableau10[c(3,4,5)],20), name = "Recovery Time")+
  xlab("Age category")+ ylab("Percent recovered (95% CI)")+
  scale_y_continuous(limits=c(0,80))+
  annotate("text",x=rev.res$agecat,y=1,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat,y=5,label=rev.res$nstudy.f,size=3)+
  #annotate("text",label=rev.res$ptest.f,x=rev.res$agecat,
  #         y=rev.res$est,hjust=-1,size=3)+
  ggtitle("Proportion of wasted children who recovered within 60 days")+
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p3
ggsave(p3, file="pooled_rev60.png", width=10, height=4)

#-------------------------------------------------------------------------------------------
# Incidence rate
#-------------------------------------------------------------------------------------------

df <- ir.data$ir.res

df$nmeas.f <- paste0(round(df$nmeas/1000, 0),rep("K at-risk",8))
df$agecat.f2 <- clean_agecat(df$agecat)


p4 <- ggplot(df, aes(y=est*1000,x=agecat.f2, fill=agecat.f2, color=agecat.f2))+
  geom_point( size = 4, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb*1000, ymax=ub*1000), alpha=0.5, size = 3) +
  scale_y_continuous(limits=c(-0.25,5))+
  xlab("Age category") +
  ylab("IR per 1000 person-days") +
  annotate("text",x=df$agecat.f2,y=-0.25,label=df$nmeas.f,size=3) +
  annotate("text",x=df$agecat.f2,y=0,label=df$nstudy.f,size=3) +
  annotate("text",label=df$ptest.f,x=df$agecat.f2, y=df$est*1000,hjust=-2,size=3)+
  theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
  scale_colour_manual(values=rep(tableau10[2],20)) +
  ggtitle("Pooled wasting incidence rate per 1,000 days") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p4
ggsave(p4, file="pooled_inc_rate.png", width=10, height=4)


#-------------------------------------------------------------------------------------------
# Incidence rate- no birth, but plot single 0-3 with birth IR
#-------------------------------------------------------------------------------------------

df <- rbind(ir.data$ir.res[1,], ir.data.nobirth$ir.res)
df$birthwasting <- factor(c(1, rep(0,8)))
df$nmeas.f <- paste0(round(df$nmeas/1000, 0),rep("K at-risk",8))
df$agecat.f2 <- clean_agecat(df$agecat)


p4_nobirth <- ggplot(df, aes(y=est*1000,x=agecat.f2, fill=birthwasting, color=birthwasting))+
  geom_point( size = 4)+ #, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb*1000, ymax=ub*1000), alpha=0.5, size = 3) +
  scale_y_continuous(limits=c(-0.25,5))+
  xlab("Age category") +
  ylab("Episodes per 1000 person-days at risk") +
  annotate("text",x=1.8,y=df$est[1]*1000,label="Including wasting at birth: 3.44",size=3) +
  annotate("text",x=df$agecat.f2[-1],y=-0.25,label=df$nmeas.f[-1],size=3) +
  annotate("text",x=df$agecat.f2[-1],y=0,label=df$nstudy.f[-1],size=3) +
  annotate("text",label=df$ptest.f[-1],x=df$agecat.f2[-1], y=df$est[-1]*1000,hjust=-1,size=3)+
  theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
  scale_colour_manual(values=rep(tableau10[c(2,4)],20)) +
  ggtitle("Pooled wasting incidence rate per 1,000 days - no birth wasting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p4_nobirth
ggsave(p4_nobirth, file="pooled_inc_rate_no_birth.png", width=10, height=4)


#   geom_point(aes(fill=strata, color=strata), size = 4) +
#   geom_linerange(aes(ymin=Lower.95.CI, ymax=Upper.95.CI, color=strata),
#                  alpha=0.5, size = 3) +
#   scale_color_manual(values=rep(tableau10[4],20))+
#   xlab("Age category")+ ylab("")+
#   #scale_y_continuous(limits=c(0,20))+
#   # annotate("text",x=vel$strata,y=.12,label=vel$nmeas.f,size=3)+
#   # annotate("text",x=vel$strata,y=0.1,label=vel$nstudy.f,size=3)+
#   ggtitle("")+
#   theme(strip.background = element_blank(),
#         legend.position="none",
#         strip.text.x = element_text(size=12),
#         axis.text.x = element_text(size=12, angle = 25, hjust = 1))
# p7
# ggsave(p7, file="pooled_duration.png", width=10, height=4)
# 



#-------------------------------------------------------------------------------------------
# Mean LAZ
#-------------------------------------------------------------------------------------------

# clean_nmeans<-function(nmeas){
#   nmeas <- round(nmeas/1000)
#   nmeas.f <- paste0("N=",nmeas,"K children")
#   return(nmeas.f)
# }
# 
# clean_agecat<-function(agecat){
#   agecat <- as.character(agecat)
#   agecat <- gsub("months","mo.", agecat)
#   agecat <- factor(agecat, levels=unique(agecat))
#   return(agecat)
# }
# 
# laz.res$nmeas.f <- clean_nmeans(laz.res$nmeas)
# laz.res$agecat <- clean_agecat(laz.res$agecat)


# p8 <- ggplot(laz.res,aes(y=est,x=agecat)) +
#   geom_point(aes(fill=agecat, color=agecat), size = 4) +
#   geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
#                  alpha=0.5, size = 3) +
#   geom_hline(yintercept=0) +
#   # scale_fill_tableau(drop=TRUE, limits = levels(laz.res$agecat)) +
#   # scale_colour_tableau(drop=TRUE, limits = levels(laz.res$agecat)) +
#   scale_color_manual(values=rep(tableau10[9],20))+  scale_fill_manual(values=rep(tableau10[9],20))+
#   xlab("Age category")+
#   ylab("Mean Z-score")+
#   scale_y_continuous(limits=c(-3,0.5))+
#   annotate("text",x=laz.res$agecat,y=-2.8, label=laz.res$nmeas.f,size=3)+
#   annotate("text",x=laz.res$agecat,y=-3, label=laz.res$nstudy.f,size=3)+
#   annotate("text",label=round(laz.res$est,2),x=laz.res$agecat,
#            y=laz.res$est,hjust=-0.75,size=3)+
#   ggtitle("Pooled mean LAZ") +
#   theme(strip.background = element_blank(),
#         legend.position="none",
#         strip.text.x = element_text(size=12),
#         axis.text.x = element_text(size=12)) 
# p8
# 
# ggsave(p8, file="pooled_laz.png", width=10, height=4)




perswast.data
