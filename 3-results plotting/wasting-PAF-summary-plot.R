

rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")
load("C:/Users/andre/Documents/HBGDki/Results/wasting_results.rdata")


d <- results

allvars<-unique(d$intervention_variable)

head(d)

#Subset to relative risks
d <- d %>% filter(type=="PAF")


#Subset to adjusted
d <- d %>% filter(adjustment_set!="unadjusted" | intervention_variable=="sex")

missing <- allvars[!(allvars %in% unique(d$intervention_variable))]
missing

#Subset to primary outcomes
table(d$agecat)

d <- d %>% filter(agecat=="0-24 months"|  agecat=="0-24 months (no birth wast)")


#Drop enrolled wasted as a RF for wasting
d <- d %>% filter(intervention_variable!="enwast" & intervention_variable!="perdiar24"  & intervention_variable!="predfeed3"  & intervention_variable!="predfeed36"  & intervention_variable!="predfeed6")

#Look at prevalence and incidence
d <- d %>% filter(outcome_variable=="ever_wasted")



head(d)



#Clean dataframe
d$agecat <- as.character(d$agecat)
d$agecat[d$agecat=="0-24 months (no birth wast)"] <- "0-24 months"

#Drop nonsensical exposure-outcome pairs
d <- d[!((d$agecat=="0-6 months" | d$agecat=="Birth") & d$intervention_variable %in% c("exclfeed3","exclfeed36","exclfeed6","perdiar6","predexfd6","predfeed3","predfeed36","predfeed6")),]
d <- d[!(d$agecat=="Birth" & (d$intervention_variable=="birthwt"| d$intervention_variable=="earlybf"| d$intervention_variable=="birthlen"| d$intervention_variable=="gagebrth")),] 

d$Nstudies <-1
d <- RMA_clean(d, outcome="ever_wasted", 
                    agecatlevels=c("Birth prevalence","0-6 months\ncumulative incidence","6 months prevalence", "6-24 months\ncumulative incidence",  "0-24 months\ncumulative incidence","24 months prevalence"))
d$intervention_variable <- factor(d$intervention_variable)

#Create cohort marker
d$cohort <- paste0(d$studyid," ", d$country)


#Make all PAF's positive
d$estimate[d$estimate<0] <- d$estimate[d$estimate<0] * (-1)


#Calculate P-value
#https://www.bmj.com/content/343/bmj.d2304
d$Z <- d$untransformed_estimate/d$untransformed_se
d$Pvalue <- exp(-0.717 * d$Z - 0.416 * d$Z^2)
d$Pvalue_cat <- ""
d$Pvalue_cat[d$Pvalue<0.05] <- "*"
d$Pvalue_cat[d$Pvalue<0.01] <- "**"
d$Pvalue_cat[d$Pvalue<0.001] <- "***"
table(d$Pvalue_cat)

#Set non-sig estimates to NA
d$estimate[d$Pvalue>=0.05] <- NA
table(is.na(d$estimate))

d$sig <- 0
d$sig[d$Pvalue<0.05] <- 1



#Drop studies or RF without sig PAF's
d <- d %>% group_by(studyid, country) %>%
  mutate(sum(sig)) %>%
  filter(sig > 0)

d <- d %>% group_by(intervention_variable) %>%
  mutate(sum(sig)) %>%
  filter(sig > 0)
d<-droplevels(d)

#-------------------------------------------------------------------
#Create heatmap
#-------------------------------------------------------------------

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

scaleFUN <- function(x) sprintf("%.2f", x)

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
         paste, collapse="\n")
}  



#Make heatmap of RR of largest single comparison (maybe anotate with text or colored dot for significance?)
#Each column a different outcome- prev across age, CI, recovery
#How to order Y-axis? Group by types of variables?


#define a color for fonts
textcol <- "grey20"


# categorize PAF
d$pafcat <- cut(d$estimate,breaks=c(0,0.1,0.2,0.3,0.4,0.5,100),labels=c("<0.1",".1-.2",".2-.3",".3-.4",".4-.5",">0.5"))
d$pafcat <- factor(d$pafcat)


# heat map plot scheme
hm1 <- ggplot(d, aes(x=intervention_variable,y=cohort)) +
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
    axis.text.x=element_text(size=12,colour=textcol,angle=45,vjust=0.5),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=270,size=12),
    plot.background=element_blank(),
    panel.border=element_blank()) +
  aes(fill=pafcat) +
  labs(x="Exposure level",y="",title="") + theme(legend.position="bottom") +
  scale_fill_viridis_d(na.value="grey80")
hm1

#Note:
#Group map by study region and by group of variables



