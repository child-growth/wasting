



#-----------------------------------
# preamble
#-----------------------------------
rm(list=ls())
library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('ggplot2')
library('gridExtra')


setwd('C:/Users/andre/Documents/HBGDki/Results')

# bright color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"




load("C:/Users/andre/Documents/HBGDwasting/R/andrew/RiskFactorRes/ARM_IRres.Rdata")

load("Nutritional_Intervention_Results.Rdata")

#Merge maternal 0-6 and other intervention 0-24 month CI

wast_inc_06_unadj <- wast_inc_06_unadj %>% filter(level=="Mat")
wast_inc_024_unadj <- wast_inc_024_unadj %>% filter(level!="Mat")
# wast_inc_06_unadj <- wast_inc_06_unadj %>% filter(level=="Mat")
# wastrec60d_024mo <- wastrec60d_024mo %>% filter(level!="Mat")

  d <- rbind(wast_inc_024_unadj,wast_inc_06_unadj)
  #recd <- rbind(wast_inc_024_unadj,wast_inc_06_unadj)

Anywast024mo <- d
wastrec60d_024mo <- wastrec60d_024mo

#Drop divids study and ecuador zinc study
ls()
Anywast024mo <- Anywast024mo %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")
wastrec60d_024mo <- wastrec60d_024mo %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")
#sevwastfalt60d_024mo <- sevwastfalt60d_024mo %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")
heatmap_df <- heatmap_df %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")

#remove grant identifiers
Anywast024mo$STUDYID<- gsub("^k.*?-" , "", Anywast024mo$STUDYID)
wastrec60d_024mo$STUDYID<- gsub("^k.*?-" , "", wastrec60d_024mo$STUDYID)
#sevwastfalt60d_024mo$STUDYID<- gsub("^k.*?-" , "", sevwastfalt60d_024mo$STUDYID)
heatmap_df$STUDYID<- gsub("^k.*?-" , "", heatmap_df$STUDYID)
tr_codes$STUDYID<- gsub("^k.*?-" , "", tr_codes$STUDYID)


#merge in treatment arms
levels(tr_codes$tr)
tr_codes$tr <- as.character(tr_codes$tr)
tr_codes$tr[tr_codes$tr=="C"] <- "Control"
tr_codes$tr[tr_codes$tr=="CF"] <- "Comp. Feeding"
tr_codes<-rename(tr_codes, level=tr)

Anywast024mo$level <- as.character(Anywast024mo$level)
Anywast024mo$order <- 1
Anywast024mo$order[Anywast024mo$level=="Mat"] <- 2
Anywast024mo$order[Anywast024mo$level=="Zinc"] <- 3
Anywast024mo$order[Anywast024mo$level=="LNS"] <- 4
Anywast024mo$order[Anywast024mo$level=="Other"] <- 5
Anywast024mo <- Anywast024mo %>% arrange(order)
Anywast024mo$level <- factor(Anywast024mo$level, levels=unique(Anywast024mo$level))


# Anywast024mo<-left_join(Anywast024mo, tr_codes, by=c("STUDYID","level"))
# Anywast024mo$ARM[is.na(Anywast024mo$ARM)] <- ""
# Anywast024mo$level <- factor(Anywast024mo$level)

#Temporarily set arm to blank until above code can be fixed.
Anywast024mo$ARM=""
wastrec60d_024mo$ARM=""

tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


n<-length(unique(heatmap_df$STUDYID))


#mark ILiNS DYAD-M as both maternal and LNS intervention
dyadLNS <- heatmap_df[heatmap_df$STUDYID=="iLiNS-DYAD-M" & heatmap_df$tr=="Mat", ]
dyadLNS$tr <- "LNS"
heatmap_df <- rbind(heatmap_df,dyadLNS)

#reoder treatment levels
heatmap_df$tr <- as.character(heatmap_df$tr)
heatmap_df$order <- 1
heatmap_df$order[heatmap_df$tr=="CF"] <- 2
heatmap_df$order[heatmap_df$tr=="Zinc"] <- 3
heatmap_df$order[heatmap_df$tr=="MMN"] <- 4
heatmap_df$order[heatmap_df$tr=="LNS"] <- 5
heatmap_df$order[heatmap_df$tr=="Other"] <- 6
heatmap_df <- heatmap_df %>% arrange(order)
heatmap_df$tr <- factor(heatmap_df$tr, levels=unique(heatmap_df$tr))
table(heatmap_df$tr)
table(is.na(heatmap_df$tr))

#rename WHZ variable
heatmap_df <- heatmap_df %>% rename(WHZ=meanWHZ)


#define a color for fonts
textcol <- "grey20"
theme_set(theme_bw())

# heat map plot scheme
hm <- ggplot(heatmap_df,aes(x= tr,y=STUDYID)) +
  # facet over measurement frequency
  geom_tile(colour="white",size=0.25)+
  #remove extra space
  #scale_y_discrete(expand=c(0,0))+
  # scale_x_continuous(expand=c(0,0),
  #                    breaks=1:7,labels=1:7)+
  #one unit on x-axis is equal to one unit on y-axis.
  #equal aspect ratio x and y axis
  coord_equal()+
  #set base size for all font elements
  theme_bw() +
  #theme options
  theme(
    # legend options
    legend.title=element_text(color=textcol,size=8),
    #reduce/remove legend margin
    legend.margin = margin(grid::unit(0.1,"cm")),
    #change legend text properties
    legend.text=element_text(colour=textcol,size=7,face="bold"),
    #change legend key height
    legend.key.height=grid::unit(0.2,"cm"),
    #set a slim legend
    legend.key.width=grid::unit(1,"cm"),
    #move legend to the bottom
    legend.position = "bottom",
    #set x axis text size and colour
    axis.text.x=element_text(size=8,colour=textcol,angle=0,vjust=0.5),
    #set y axis text colour and adjust vertical justification
    axis.text.y=element_text(size=8,vjust = 0.2,colour=textcol),
    #change axis ticks thickness
    axis.ticks=element_line(size=0.4),
    # axis.ticks.x=element_blank(),
    #change title font, size, colour and justification
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    #format facet labels
    strip.text.x = element_text(size=10),
    strip.text.y = element_text(angle=270,size=10),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank()
    #remove plot margins
    # plot.margin=margin(grid::unit(1,"cm"))
  )
wphm <- hm +
  aes(fill=WHZ) +
  labs(x="Treatment group",y="",title="Mean WHZ by\nintervention arm") #+
  # scale_fill_brewer(palette = "YlOrRd",na.value="grey90",
  #                   guide=guide_legend(title="Mean WHZ",title.vjust = 1,
  #                                      label.position="bottom",label.hjust=0.5,nrow=1))
 
#wphm






#----------------------------------------------------
# Function to create fixed or random effects pooled RRs
#----------------------------------------------------

#Fit RE and FE models
meta_fun <- function(res, method, reference=NULL){
  
  #ID reference level
  levels<-levels(res$level)
  res<-res[!is.na(res$logRR.var),]
  if(is.null(reference)){
    reference<- levels[!(levels %in% unique(res$level))]
  }
  require(metafor)
  RMAest<-data.frame(study=paste0("Pooled estimate"), res$variable[1],reference ,b=NA, se=NA)
  colnames(RMAest)<-c("study","variable","level","logRR.psi","logSE")

  for(j in 1:length(unique(res$level))){
    temp<-res[res$level==unique(res$level)[j],]
  fit<-rma(yi=logRR.psi, vi=logRR.var, data=temp, method=method, measure="RR")
  est<-data.frame(study=paste0("Pooled estimate"), temp$variable[1],temp$level[1] ,fit$b, fit$se)
  colnames(est)<-c("study","variable","level","logRR.psi","logSE")
  RMAest<-rbind(RMAest, est)
  }
  
RMAest$RR<-exp(RMAest$logRR)
RMAest$RR.CI1<-exp(RMAest$logRR - 1.96 * RMAest$logSE)
RMAest$RR.CI2<-exp(RMAest$logRR + 1.96 * RMAest$logSE)

#rename SE column to var just to allow binding to original results:
#not used again in calculations
colnames(RMAest)[5]<-"logRR.var"
    return(RMAest)
}





#----------------------------------------------------
# Clean results dfs function
#----------------------------------------------------

cleandf <- function(d, meta_method="REML", RF_levels=c("<=2600","2600-3000","3000-3300",">3300")){
  
  #remove grant identifier
  d$study<- gsub("^k.*?-" , "", d$study)
  
  #create labels for country-specific cohorts
  d$country <- as.character(d$country)
  d$country[d$country=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
  d$study <- paste0(d$study, " ", d$country)
  
  #drop studies with <5 cases in either reference or comparison level
  d <- d %>% filter(is.na(b) | !(b<5 | d<5))
  
  d_meantab <- d %>% subset(.,select=c(study,meanLevel:meanY, mean.CI1, mean.CI2))
  colnames(d_meantab) <- c("Study","Level", "Number in quartile", "Cumulative incidence of wasting", "95% CI-lower", "95% CI-upper")

  d <- d %>% subset(.,select=c(study, variable, level, logRR.psi, logRR.var,
                                               RR, RR.CI1, RR.CI2, ARM))
  
  #Estimate and merge in pooled RR's
  d_RE<-meta_fun(d, method=meta_method)
  d_RE$ARM <- ""
  d<-rbind(d_RE,d) %>% subset(., select=-c(logRR.psi,  logRR.var))

  # Clean dataframes for plotting
  #Sort levels/studies for plot
  rankedRR <- d %>% group_by(study) %>% summarize(maxRR = max(RR, na.rm=T)) %>% arrange(maxRR)
  rankedRR$order <- 1:nrow(rankedRR)
  d <- left_join(d, rankedRR, by="study")
  d$order[d$study=="Pooled estimate"] <- 1
  
  #order level
  d$level_order<-NA
  for(i in 1:length(RF_levels)){
    d$level_order[d$level==RF_levels[i]]<-i
  }
  d <- d %>% arrange(level_order) %>% mutate(level=as.character(level))
  d$level <- factor(d$level, levels=unique(d$level))

  
  #Drop studies with no estimated RR and order by size of RR and arrange the dataframe
  d <- d %>% filter(!(maxRR=="-Inf")) %>% arrange(order, level)
  d$x<-1:nrow(d)
  d$study <- factor(d$study , as.character(unique(d$study )))

  return(d)
}




#----------------------------------------------------
# RR Plot function
#----------------------------------------------------
RRplot_fun <- function(d, reflevel, title, units="", levels=NULL, free_Y=T){
  
  
  cbPalette <- c( "#56B4E9" , rep("#999999",20)) 

  
  #change names of risk factor levels
  if(!is.null(levels)){
    d$level <- factor(as.numeric(d$level))
    levels(d$level) <- levels
  }
  
  #plot
n <- nrow(d)
plot_df <- d %>% filter(level!=reflevel) %>% arrange(level)
plot_df$size<-ifelse(plot_df$study=="Pooled estimate",1,0.5)
RRplot<-ggplot(data=plot_df) + 
  labs(title = d$variable, x = "Cohort", y = paste0("Risk ratio (reference = ",reflevel," ", units,")")) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.125,0.25,0.5,1,2,4), trans='log10') +
  coord_flip(ylim = c(0.5, 2)) +
  scale_fill_manual(values=cbPalette) +
  scale_colour_manual(values=cbPalette) +
  scale_size_continuous(range = c(0.5, 1))+
  geom_pointrange( mapping=aes(x=study, y=RR, ymin=RR.CI1, ymax=RR.CI2, colour=study, size=size)) +
  #geom_text(aes(x=study, y=0.25, label=ARM, size=0.1, alpha=0.5)) +
    theme(panel.border = element_blank(), 
    strip.background = element_blank(), legend.position = "none")

if(free_Y){
  RRplot<-RRplot + facet_wrap(~level, ncol=1, scales = "free_y") 
}else{
  RRplot<-RRplot + facet_wrap(~level, ncol=1) 
}

RRplot<-RRplot + geom_vline(xintercept=1.5, color="grey20", linetype=2) +
   # geom_label(aes(label="Overall RR", x=1, y=6, size=.1)) + #Need to make smaller
    ggtitle(title) +theme(legend.position="none")#+ guides(colour = guide_legend(reverse=T)) 
return(RRplot)
}



#----------------------------------------------------
# Clean and plot risk factor results
#----------------------------------------------------



#drop guatemala BSC
#Anywast024mo<-Anywast024mo[Anywast024mo$STUDYID!="Guatemala BSC",]
Anywast024mo <- Anywast024mo %>% rename(study=STUDYID, country=COUNTRY)
Anywast024mo <- cleandf(Anywast024mo, RF_levels=c("C", "Mat", "Zinc", "LNS",  "Other"))

#Drop pooled estimate for Other arm
Anywast024mo$RR[which(Anywast024mo$level=="Other" & Anywast024mo$study=="Pooled estimate")]<-NA


#Anywast024mo$hline <- ifelse(Anywast024mo$level=="Other",5,1.5)
Anywast024mo_plot <-RRplot_fun(Anywast024mo, 
                             reflevel="Control", 
                             title="Cumulative incidence ratios:\nAny wasting", 
                             units="",
                             levels = c("Control","Maternal Intervention, 1-6mo CIR","Zinc, 1-24mo CIR","Lipid-based Nutrient Supplement, 1-24mo CIR","Other"),
                             free_Y = T)
Anywast024mo_plot


wastrec60d_024mo <- wastrec60d_024mo %>% rename(study=STUDYID, country=COUNTRY)
wastrec60d_024mo <- cleandf(wastrec60d_024mo[,-c(3:4)], RF_levels=c("C", "Mat", "Zinc", "LNS",  "Other"))

#Drop pooled estimate for Other arm
wastrec60d_024mo$RR[which(wastrec60d_024mo$level=="Other" & wastrec60d_024mo$study=="Pooled estimate")]<-NA
#Anywast024mo$hline <- ifelse(Anywast024mo$level=="Other",5,1.5)
wastrec60d_024mo <- wastrec60d_024mo %>% filter(!is.na(RR))
wastrec60d_024mo_plot <-RRplot_fun(wastrec60d_024mo, 
                             reflevel="Control", 
                             title="Cumulative incidence ratios:\nAny wasting from 1-24 months age", 
                             units="",
                             levels = c("Control","Maternal Intervention","Zinc","Lipid-based Nutrient Supplement","Other"),
                             free_Y = T)
wastrec60d_024mo_plot





#Plot pooled results only
pooled_df <- Anywast024mo[Anywast024mo$study=="Pooled estimate",] %>% filter(!is.na(RR))
pooled_df$intervention <- c("Maternal\nIntervention,\n1-6mo CIR", 
                            "Zinc\nsupplements,\n1-24mo CIR", 
                            "Lipid-based\nNutrient\nSupplements,\n1-24mo CIR")

pooled_plot<-ggplot(data=pooled_df) + 
  labs(title = "Cumulative incidence ratios:\nAny wasting", x = "Intervention group", y = paste0("Risk ratio (reference = ","Control arms)")) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(
    breaks=c(0.8,0.9,1,1.11,1.25), 
    trans='log10') +
  coord_flip(ylim = c(0.8, 1.25)) +
  scale_fill_manual(values=tableau10) +
  scale_colour_manual(values=tableau10) +
  scale_size_continuous(range = c(0.5, 1))+
  geom_pointrange( mapping=aes(x=intervention, y=RR, ymin=RR.CI1, ymax=RR.CI2, colour=intervention, size=2)) +
  #geom_text(aes(x=study, y=0.25, label=ARM, size=0.1, alpha=0.5)) +
    theme(panel.border = element_blank(), 
    strip.background = element_blank(), legend.position = "none")
pooled_plot


setwd("C:/Users/andre/Documents/HBGDki/Figures/Nutritional Intervention Analysis")

pdf("nutritional_int_plots.pdf",width=6,height=10)
wphm
Anywast6mo_plot

dev.off()











#-----------------------------------
# plots for CI for full study period
#-----------------------------------


load("ARM_IRres_fullstudyCI.Rdata")


#Drop divids study and ecuador zinc study
ls()
Anywast <- Anywast %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")
wastrec60d <- wastrec60d %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")
sevwastfalt60d <- sevwastfalt60d %>% filter(STUDYID!="ki1000301-DIVIDS" & STUDYID!="ki1112895-Ecuador Zn")

#remove grant identifiers
Anywast$STUDYID<- gsub("^k.*?-" , "", Anywast$STUDYID)
wastrec60d$STUDYID<- gsub("^k.*?-" , "", wastrec60d$STUDYID)
sevwastfalt60d$STUDYID<- gsub("^k.*?-" , "", sevwastfalt60d$STUDYID)



tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


n<-length(unique(heatmap_df$STUDYID))




#----------------------------------------------------
# Clean and plot risk factor results
#----------------------------------------------------

Anywast <- cleandf(Anywast, RF_levels=c("Control", "Zinc", "MMN", "Comp. feeding",  "LNS", "Other"))
Anywast_plot <-RRplot_fun(Anywast, 
                             reflevel="Control", 
                             title="Cumulative incidence ratios:\nAny wasting within trial", 
                             units="",
                             levels = c("Control","Zinc","Multiple Micronutrients","Complementary Feeding","Lipid-based Nutrient Supplement","Other"),
                             free_Y = T)
Anywast_plot











wastrec60d <- cleandf(wastrec60d, RF_levels=c("Control", "Zinc", "MMN", "Comp. feeding",  "LNS", "Other"))
wastrec60d_plot <-RRplot_fun(wastrec60d, 
                             reflevel="Control", 
                             title="Cumulative incidence ratios:\nAny wasting within trial", 
                             units="",
                             levels = c("Control","Zinc","Multiple Micronutrients","Complementary Feeding","Lipid-based Nutrient Supplement","Other"),
                             free_Y = T)
wastrec60d_plot


sevwastfalt60d <- cleandf(sevwastfalt60d, RF_levels=c("Control", "Zinc", "MMN", "Comp. feeding",  "LNS", "Other"))
sevwastfalt60d_plot <-RRplot_fun(sevwastfalt60d, 
                             reflevel="Control", 
                             title="Cumulative incidence ratios:\nAny wasting within trial", 
                             units="",
                             levels = c("Control","Zinc","Multiple Micronutrients","Complementary Feeding","Lipid-based Nutrient Supplement","Other"),
                             free_Y = T)
sevwastfalt60d_plot












#Old plot code

# Anywast_plot<-ggplot(data=d) + 
#   labs(title = d$variable, x = "Intervention Arm", y = "Risk ratio (reference = Control arm)") +
#   geom_hline(yintercept = 1) +
#   scale_y_continuous(breaks=c(0.125,0.25,0.5,1,2,4), trans='log10') +
#   coord_flip() +
#   facet_wrap( ~level, scales = "free_y") +
#   geom_pointrange( mapping=aes(x=study, y=RR, ymin=RR.CI1, ymax=RR.CI2, colour=study)) +
#     theme(panel.border = element_blank(), 
#     strip.background = element_blank()) +
#   #scale_x_discrete(breaks = 1:n, labels=Anywast6mo$level) +
#   scale_colour_manual(values=cbPalette) +
#  #geom_text(aes(label=rep(c("(ref.)","","",""),n/4), x=c(1:n), y=rep(1,n),colour=study)) +
#   theme(legend.position = "none") +
#   ggtitle("Risk ratio of any wasting by 24 months")  #guides(colour = guide_legend(reverse=T))
# Anywast_plot

