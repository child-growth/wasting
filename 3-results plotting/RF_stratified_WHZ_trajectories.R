
rm(list=ls())
library(tidyverse)
library(directlabels)
library(ggrepel)

#Set theme
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728", 
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")





load("U:/Data/Wasting/rf_wasting_data.RData")
d <- d %>% subset(., select=-c(tr))

#subset to monthly outcomes
d <- d %>% filter(measurefreq=="monthly")

#merge WLZ outcomes with covariates

setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_clean_covariates.rds")


cov <- cov %>% subset(., select=-c( month, W_gagebrth,    W_birthwt,     W_birthlen,   
                                    W_mage,        W_mhtcm,       W_mwtkg,       W_mbmi,        W_fage,        W_fhtcm,       W_meducyrs,    W_feducyrs,   
                                    W_nrooms,      W_nhh,         W_nchldlt5,    W_parity,         
                                    W_perdiar6,    W_perdiar24))

d <- left_join(d, cov, by=c("studyid","country","subjid"))

d <- d %>% filter(agedays < 24 * 30.4167)

#Calculate age in months
d$agemonth <- d$agedays/30.4167

#Add RF labels
d <- d %>% rename(
  `Gender`=sex ,
  `Enrolled wasted`= enwast,
  `Gestational age at birth`= gagebrth,
  `Exclusive or Predominant breastfeeding under 6 months`= predexfd6,
  `Mother's age`= mage,
  `Mother's height`= mhtcm,
  `Mother's weight`= mwtkg,
  `Mother's BMI`= mbmi,
  `Mother's education`= meducyrs,
  `Birth order`= parity,
  `Household food security`= hfoodsec,
  `Number of children <5 in household`= nchldlt5,
  `Household wealth`= hhwealth_quart,
  `Father's age`= fage,
  `Father's height`= fhtcm,
  `Birthweight (kg)`= birthwt,
  `Birth length (cm)`= birthlen,
  `Vaginal birth`= vagbrth,
  `Child delivered at home`= hdlvry,
  `Single parent`= single,
  `Number of rooms in household`= nrooms,
  `Number of people in household`= nhh,
  `Maternal education quartile`= meducyrs,
  `Paternal education quartile`= feducyrs,
  `Any wasting before 6 months age`= anywast06,
  `Persistent wasting before 6 months age`= pers_wast,
  `Treats drinking water`= trth2o,
  `Clean cooking fuel usage`= cleanck,
  `Improved floor`= impfloor,
  `Improved sanitation`= impsan,
  `Safe water source`= safeh20,
  `Quartile of diarrhea longitudinal\nprevalence under 6 months`= perdiar6,
  `Quartile of diarrhea longitudinal\nprevalence under 24 months`= perdiar24,
  `Breastfeed within an hour of birth`= earlybf,
  `Predominant breastfeeding under 3 months`= predfeed3,
  `Predominant breastfeeding from 3-6 months`= predfeed36,
  `Predominant breastfeeding under 6 months`= predfeed6,
  `Exclusive breastfeeding under 3 months`= exclfeed3,
  `Exclusive breastfeeding from 3-6 months`= exclfeed36,
  `Exclusive breastfeeding under 6 months`= exclfeed6,
  `Month of measurement`= month,
  `Birth month`= brthmon)

d <- subset(d, select = -c(id))

#Strip grant identifier and add country
d$studyid <- gsub("^k.*?-" , "", d$studyid)
d$studyid <- paste0(d$studyid, ", ", paste0(substring(as.character(d$country),1,1), tolower(substring(as.character(d$country),2))))
d$studyid <- gsub("Tanzania, united republic of", "Tanzania", d$studyid)
d$studyid <- gsub("africa", "Africa", d$studyid)



pdf("U:/Figures/Risk Factor WLZ curves.pdf", height=8, width=12)
for(i in 11:ncol(d)){
  df <- d[!is.na(d[,i]),]
  Aname <- colnames(df)[i]
  colnames(df)[i] <- "Avar"
  p<-ggplot(df, aes(x=agemonth, y=whz, group=Avar, color=Avar)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
    scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of ", Aname))+
    xlab(Aname) + ylab("WLZ") + 
    ggtitle(paste0("Spline curves of WLZ, stratified by levels of ", Aname))
  
  print(p)

}
dev.off()


#Cohort stratified
pdf("U:/Figures/Risk Factor WLZ curves-cohort stratified.pdf", height=12, width=12)
for(i in 11:ncol(d)){
  df <- d[!is.na(d[,i]),]
  Aname <- colnames(df)[i]
  colnames(df)[i] <- "Avar"
  p<-ggplot(df, aes(x=agemonth, y=whz, group=Avar, color=Avar)) + 
    #geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
    geom_smooth(method = 'gam') +
    scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of ", Aname))+
    facet_wrap(~studyid) +
    xlab(Aname) + ylab("WLZ") + 
    ggtitle(paste0("Spline curves of WLZ, stratified by levels of ", Aname))
  
  print(p)
  
}
dev.off()







colnames(d)
df <- d %>% ungroup() %>% select(studyid, `Month of measurement`, agemonth, whz, country)


# region stratified
df <- df %>% mutate(region = case_when(
  country=="BANGLADESH" | country=="INDIA"|
    country=="NEPAL" | country=="PAKISTAN"|
    country=="PHILIPPINES"                   ~ "Asia", 
  country=="KENYA"|
    country=="GHANA"|
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

df <- df %>% filter(region!="Europe")

df$region <- factor(df$region , levels=c( "Asia","Africa","Latin America"))

p<-ggplot(df[!is.na(df$region),], aes(x=agemonth, y=whz, group=region, color=region)) +
  #geom_smooth(method = 'gam', se = FALSE, formula= y ~ s(x, bs = "cs"), size=2) +
  geom_smooth(method = 'loess',se = FALSE, size=2) +
  scale_color_manual(values=rep(tableau10,2), name = paste0("Region"))+
  xlab("Child age in months") + ylab("WLZ") + coord_cartesian(xlim=c(0,24), ylim = c(-1,1), expand=c(0,0)) +
  scale_x_continuous(breaks=c(0,6,12,18,24)) +
  ggtitle("Region") + theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))



ggsave(p, file="U:/Figures/Stunting Webinar/region_WLZ_trajectories.png", width = 9, height = 3.5)


df$month<-as.numeric(df$`Month of measurement`)
monthlabs <- c("Jan", "", "Mar", "", "May", "", "Jul", "", "Sep", "", "Nov", "", "", "", "")

# plotdf <- df[df$studyid=="ki1000108-CMC-V-BCS-2002",]
# p<-ggplot(plotdf, aes(x=month, y=whz,  color=region)) + 
#   #geom_smooth(method = 'gam', se = FALSE, size=2, formula= y ~ s(x, bs = "cs")) +
#   coord_cartesian(ylim=c(0,-1.35)) +
#   scale_x_continuous(breaks=c(1:12), labels= monthlabs, expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   geom_smooth(method = 'loess',se = FALSE, size=2) +
#   scale_color_manual(values=rep(tableau10[2],2))+
#   xlab("Month of measurement") + ylab("WLZ") + #
#   ggtitle("WLZ over season in the CMC Vellore, India, cohort") + theme(legend.position="none", axis.text.y = element_text(size=12), axis.text.x = element_text(size=12)) 
# p
# 
# ggsave(p, file="U:/Figures/Stunting Webinar/cmc_season_WLZ_trajectories.png", width = 6, height = 5.2)
# 


df$countrycohort<-paste0(df$studyid,df$country)

plotdf <- df[df$region=="Asia",]
plotdf$highlight <- 1
#plotdf$highlight[plotdf$studyid=="ki1000108-CMC-V-BCS-2002"] <- 2

plotdf <- plotdf %>% rename(Month=month)
plotdf$studyid[plotdf$studyid=="GMS-Nepal, Nepal"] <- "GMS-Nepal"
plotdf$studyid[plotdf$studyid=="CMC-V-BCS-2002, India"] <- "CMC, India"
plotdf$studyid <- gsub("Bangladesh","BD",plotdf$studyid)


p_asia<-ggplot(plotdf, aes(x=Month, y=whz, group=factor(studyid), color=factor(studyid))) + 
  #geom_smooth(aes(size=highlight, linetype=factor(2-highlight)), method = 'gam', se = FALSE, formula= y ~ s(x, bs = "cs")) +
  geom_smooth(method = 'loess',se = FALSE) +
  coord_cartesian(ylim=c(0,-1.35), xlim=c(1,15)) +
  scale_x_continuous(breaks=c(1:15), labels= monthlabs, expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_manual(values=rep(tableau10[c(1,3,4,2,5:10)],20),name = "Cohort") +
  scale_size(range=c(1, 2), guide=FALSE) +
  xlab("Month of measurement") + ylab("WLZ") + 
  ggtitle("Example: WLZ over season in South Asian cohorts") + 
  theme(legend.position="none", axis.text.y = element_text(size=12), axis.text.x = element_text(size=12)) 
  #geom_dl(aes(label = studyid), method = list("angled.boxes"))
  #geom_dl(aes(label = studyid), method = list("last.qp"), inherit.aes=T) +
  #geom_dl(label=as.factor(plotdf$studyid), method="maxvar.points", inherit.aes=T)



library(data.table)
smooth_dat <- setDT(ggplot_build(p_asia)$data[[1]])
smooth_lab <- smooth_dat[smooth_dat[, .I[x == max(x)], by=group]$V1]
smooth_lab$label <- levels(factor(plotdf$studyid))
#smooth_lab$y[6] <- 

p_asia <- p_asia + annotate("text", x = smooth_lab$x, y=smooth_lab$y, 
             label=smooth_lab$label, colour=smooth_lab$colour,
             hjust=-0.05)



ggsave(p_asia, file="U:/Figures/Stunting Webinar/season_WLZ_trajectories.png", width = 6, height = 5.2)





