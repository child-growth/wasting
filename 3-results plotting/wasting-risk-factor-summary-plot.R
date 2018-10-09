

rm(list=ls())
library(tidyverse)
library(metafor)

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

d <- d %>% filter( agecat=="Birth"|  agecat=="6 months"|  agecat=="24 months"| 
  agecat=="0-6 months"|  agecat=="6-24 months"| agecat=="0-24 months"|  agecat=="0-24 months (no birth wast)"| agecat=="0-6 months (no birth wast)")


#Drop enrolled wasted as a RF for wasting
d <- d %>% filter(intervention_variable!="enwast" & intervention_variable!="perdiar24"  & intervention_variable!="predfeed3"  & intervention_variable!="predfeed36"  & intervention_variable!="predfeed6")

#Look at prevalence and incidence
d <- d %>% filter(outcome_variable=="ever_wasted" | outcome_variable=="wasted")



head(d)



RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level,outcome_variable) %>%
  do(poolRR(.)) %>% as.data.frame()

   

#Clean dataframe
RMAest$agecat <- as.character(RMAest$agecat)
RMAest$agecat[RMAest$agecat=="0-24 months (no birth wast)"] <- "0-24 months"
RMAest$agecat[RMAest$agecat=="0-6 months (no birth wast)"] <- "0-6 months"

#Drop nonsensical exposure-outcome pairs
RMAest <- RMAest[!((RMAest$agecat=="0-6 months" | RMAest$agecat=="Birth") & RMAest$intervention_variable %in% c("exclfeed3","exclfeed36","exclfeed6","perdiar6","predexfd6","predfeed3","predfeed36","predfeed6")),]
RMAest <- RMAest[!(RMAest$agecat=="Birth" & (RMAest$intervention_variable=="birthwt"| RMAest$intervention_variable=="earlybf"| RMAest$intervention_variable=="birthlen"| RMAest$intervention_variable=="gagebrth")),] 
                 
                 
RMAest <- RMA_clean(RMAest, outcome="ever_wasted", 
                    agecatlevels=c("Birth prevalence","0-6 months\ncumulative incidence","6 months prevalence", "6-24 months\ncumulative incidence",  "0-24 months\ncumulative incidence","24 months prevalence"))
RMAest$intervention_variable <- factor(RMAest$intervention_variable)



#Calculate P-value
#https://www.bmj.com/content/343/bmj.d2304
RMAest$Z <- RMAest$logRR.psi/RMAest$logSE
RMAest$Pvalue <- exp(-0.717 * RMAest$Z - 0.416 * RMAest$Z^2)
RMAest <- RMAest %>% group_by(intervention_variable, agecat, outcome_variable) %>% mutate(maxP=max(Pvalue), minP=max(Pvalue))
RMAest$Pvalue[is.na(RMAest$Pvalue)] <- RMAest$maxP[is.na(RMAest$Pvalue)] 

RMAest$Pvalue_cat <- ""
RMAest$Pvalue_cat[RMAest$Pvalue<0.05] <- "*"
RMAest$Pvalue_cat[RMAest$Pvalue<0.01] <- "**"
RMAest$Pvalue_cat[RMAest$Pvalue<0.001] <- "***"
table(RMAest$Pvalue_cat)





#Print plots across all intervention arms


#Grab largest single comparison (by distance of CI from null)
RMAestfull <- RMAest

RMAestsub <- RMAest %>% group_by( agecat,intervention_variable) %>%
  mutate(sig=1*( (RR.CI1>1 & RR.CI2 >1) | (RR.CI1<1 & RR.CI2 <1) ),
         anysig=max(sig)) %>%
  filter(sig==anysig) %>% #grab significant if any comparisons are
  mutate(maxCI=ifelse(RR>1, max(RR.CI1), min(RR.CI2)),
         bestRR=max(maxCI),
         posRR=ifelse(RR>1,RR,1/RR),
         maxposRR=max(posRR)) %>%
  filter(((RR.CI1==maxCI | RR.CI2==maxCI) & sig==1) | ((RR==maxposRR | 1/RR == maxposRR) & sig==0))


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

yticks <- c(0.125,0.25,0.5,1,2,4,8,16)
scaleFUN <- function(x) sprintf("%.2f", x)

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
         paste, collapse="\n")
}  


#Note:
#Print reference level on the x-axis label
#Can I facet or multiplot the 0-6 and 6-24 together
#Could multiplot, with ggtitle as outcome, and X-axis labels turned off for right plot
#Print comparison level on the plot

  p <-  ggplot(RMAestsub[RMAestsub$agecat=="6-24 months\ncumulative incidence",], aes(x=RFlabel )) + 
    geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
    geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                   alpha=0.5, size = 3) +
    labs(x = "Risk factor level", y = "RR") +
    geom_hline(yintercept = 1) +
    geom_text(aes(x=0.5, y=-0.05, label=Nstudies), size=3,  hjust=0) +
    #coord_cartesian(ylim=ytick_range) +
    scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
    scale_fill_manual(values=rep(tableau10,4))+ #, drop=TRUE, limits = levels(RMAestsub$agecat)) +
    scale_colour_manual(values=rep(tableau10,4))+ #, drop=TRUE, limits = levels(RMAestsub$agecat)) +
    scale_size_continuous(range = c(0.5, 1))+
    theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=10), axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
    #facet_wrap(~agecat, nrow = 1, labeller = label_wrap) +
    #facet_grid(RFtype~agecat) +
    facet_wrap(~RFtype, ncol = 1, scales="free_y", labeller = label_wrap) +
    ggtitle("") + coord_flip() 





#Make heatmap of RR of largest single comparison (maybe anotate with text or colored dot for significance?)
#Each column a different outcome- prev across age, CI, recovery
#How to order Y-axis? Group by types of variables?


#define a color for fonts
textcol <- "grey20"

#
# categorize RR
# dd$stpcat <- cut(dd$stuntprev,breaks=c(0,5,10,20,30,40,50,60,100),labels=c("<5","5-10","10-20","20-30","30-40","40-50","50-60",">60"))
# dd$stpcat <- factor(dd$stpcat)


# heat map plot scheme
hm1 <- ggplot(RMAestsub,aes(x=agecat,y=RFlabel)) +
  #facet_grid(agecat~RFtype, scales='free_y',space='free_y') +
  #facet_wrap(~agecat, nrow=1) +
  facet_grid(RFtype~., scales='free_y',space='free_y') +
  geom_tile(colour="white",size=0.25)+
  scale_y_discrete(expand=c(0,0))+
  # scale_x_continuous(expand=c(0,0),
  #                    breaks = scales::pretty_breaks(n = 8))+
  #coord_equal()+
  theme_bw(base_size=12)+
  theme(
    strip.background = element_blank(),
    legend.title=element_text(color=textcol,size=12),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour=textcol,size=8),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    legend.position = "bottom",
    axis.text.x=element_text(size=12,colour=textcol,angle=0,vjust=0.5),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=270,size=12),
    plot.background=element_blank(),
    panel.border=element_blank()) +
  aes(fill=RR) +
  geom_text(aes(label=Pvalue_cat), color="white") + 
  labs(x="Exposure level",y="",title="") + theme(legend.position="bottom") +
  scale_fill_viridis_c(na.value="grey80",
                       guide="colourbar")
hm1









#Maybe make heatmap with 4 columns, leave blank if less than 4 levels, and color grey for reference level.
#Print the name of the category in the box?

RMAestfull <- RMAestfull %>% group_by(agecat, outcome_variable, intervention_variable) %>% arrange(intervention_level) %>% 
     mutate(order=row_number())

RMAestfull$RR[RMAestfull$RR==1] <- NA #Set reference to NA


# heat map plot scheme
hm3 <- ggplot(RMAestfull[RMAestfull$agecat=="6-24 months\ncumulative incidence",],aes(x=order,y=RFlabel)) +
  #facet_grid(agecat~RFtype, scales='free_y',space='free_y') +
  #facet_wrap(~agecat, nrow=1) +
  facet_grid(RFtype~., scales='free_y',space='free_y') +
  geom_tile(colour="white",size=0.25)+
  scale_y_discrete(expand=c(0,0))+
  # scale_x_continuous(expand=c(0,0),
  #                    breaks = scales::pretty_breaks(n = 8))+
  #coord_equal()+
  theme_bw(base_size=12)+
  theme(
    strip.background = element_blank(),
    legend.title=element_text(color=textcol,size=12),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour=textcol,size=8),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    legend.position = "bottom",
    axis.text.x=element_text(size=12,colour=textcol,angle=0,vjust=0.5),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=270,size=12),
    plot.background=element_blank(),
    panel.border=element_blank()) +
  aes(fill=RR) +
  labs(x="Exposure level",y="",title="6-24 months\ncumulative incidence") + theme(legend.position="bottom") +
  scale_fill_viridis_c(na.value="grey80",
                       guide="colourbar")
hm3


#NOTE:
#Need to add diverging color scale, so 1 (null) is a sensible value
#Maybe rerun with the low level set as the reference level for all.
