

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
table(d$type)

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
table(d$outcome_variable)
d <- d %>% filter(outcome_variable=="pers_wast")



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


RMAest <- RMA_clean(RMAest, outcome="pers_wasted", 
                    agecatlevels=c("Birth prevalence","0-6 months\npersistant wasting","6 months prevalence", "6-24 months\npersistant wasting",  "0-24 months\npersistant wasting","24 months prevalence"))
RMAest$RFlabel[RMAest$RFlabel=="Exclusive or Predominant breastfeeding under 6 months"] <- "Exclusive or Predominant\nbreastfeeding under 6 months" 

RMAest$intervention_variable <- factor(RMAest$intervention_variable)

#Add the reference level to the RFlabel
#RMAest$RFlabel <- paste0(RMAest$RFlabel," (Ref:",RMAest$baseline,")")


d <- RMAest


#Function to relevel RR
relevel_RF <- function(d, reference, variable){
  dfull <- d %>% filter(intervention_variable!=variable)
  d <- d %>% filter(intervention_variable==variable)
  
  d$ref <- 0
  d$ref[d$intervention_level==reference] <- 1
  
  d <- d %>% group_by(agecat,outcome_variable) %>% mutate(reflogRR=sum(ref*logRR.psi), logRR2=logRR.psi-reflogRR, RR2=exp(logRR2))
  d$RFlabel <- paste0(d$RFlabel," (Ref:",reference,")")
  d<- bind_rows(dfull, d)
  return(d)
}

unique(d$intervention_level)
d <- relevel_RF(d, variable="fage", reference="<32")  
d <- relevel_RF(d, variable="feducyrs", reference="High")
d <- relevel_RF(d, variable="fhtcm", reference=">=167 cm")
d <- relevel_RF(d, variable="gagebrth", reference="Full or late term")
d <- relevel_RF(d, variable="hdlvry", reference="No")
d <- relevel_RF(d, variable="hfoodsec", reference="Food Secure")
d <- relevel_RF(d, variable="hhwealth_quart", reference="Q4")
d <- relevel_RF(d, variable="impfloor", reference="Yes")
d <- relevel_RF(d, variable="impsan", reference="Yes")  
d <- relevel_RF(d, variable="mage", reference="<25")
d <- relevel_RF(d, variable="mbmi", reference="Overweight or Obese")   
d <- relevel_RF(d, variable="meducyrs", reference="High")
d <- relevel_RF(d, variable="mhtcm", reference=">=155 cm")    
d <- relevel_RF(d, variable="mwtkg", reference=">=58 kg")
d <- relevel_RF(d, variable="nchldlt5", reference="2+")
d <- relevel_RF(d, variable="nhh", reference="3 or less")  
d <- relevel_RF(d, variable="nrooms", reference="4+")
d <- relevel_RF(d, variable="parity", reference="1")  
d <- relevel_RF(d, variable="perdiar6", reference="0%")  
d <- relevel_RF(d, variable="predexfd6", reference="Yes")  
d <- relevel_RF(d, variable="exclfeed3", reference="Yes")  
d <- relevel_RF(d, variable="exclfeed36", reference="Yes")  
d <- relevel_RF(d, variable="exclfeed6", reference="Yes")  
d <- relevel_RF(d, variable="earlybf", reference="Yes")  
d <- relevel_RF(d, variable="safeh20", reference="Yes")
d <- relevel_RF(d, variable="sex", reference="Female")
d <- relevel_RF(d, variable="single", reference="No") 
d <- relevel_RF(d, variable="trth2o", reference="Yes")    
d <- relevel_RF(d, variable="vagbrth", reference="No")
d <- relevel_RF(d, variable="cleanck", reference="Yes")
d <- relevel_RF(d, variable="birthwt", reference=">= 2500 g")
d <- relevel_RF(d, variable="birthlen", reference=">=50 cm")
d$RR2[is.na(d$RR2)] <- d$RR[is.na(d$RR2)] 


#Calculate P-value
#https://www.bmj.com/content/343/bmj.d2304
d$Z <- abs(d$logRR.psi)/d$logSE
d$Pvalue <- exp(-0.717 * d$Z - 0.416 * d$Z^2)
d <- d %>% group_by(intervention_variable, agecat, outcome_variable) %>% mutate(maxP=max(Pvalue, na.rm=T), minP=min(Pvalue, na.rm=T))
d$Pvalue[is.na(d$Pvalue)] <- d$minP[is.na(d$Pvalue)] 

d$Pvalue_cat <- ""
d$Pvalue_cat[d$Pvalue<0.05] <- "*"
d$Pvalue_cat[d$Pvalue<0.01] <- "**"
d$Pvalue_cat[d$Pvalue<0.001] <- "***"
table(d$Pvalue_cat)



d$intervention_level <- factor(d$intervention_level, levels= c("0",  "No",                 
                                                               "Yes",">=50 cm","[48-50) cm","<48 cm",">= 2500 g", "< 2500 g",         
                                                               "4+",  "3", "2", "1",                             
                                                               "<32",  "[32-38)", ">=38",             
                                                               "High", "Medium","Low",
                                                               ">=167 cm", "[162-167) cm",  "<162 cm",            
                                                               "Full or late term", "Early term","Preterm",            
                                                               "Food Secure", "Mildly Food Insecure", "Food Insecure",      
                                                               "Q4","Q3", "Q2", "Q1",
                                                               "<25","[25-30)",">=30",               
                                                               "Overweight or Obese","Normal weight","Underweight",
                                                               ">=155 cm", "[151-155) cm",  "<151 cm",
                                                               ">=58 kg", "[52-58) kg","<52 kg", 
                                                               "2+","3+",  
                                                               "3 or less",  "4-5",    "6-7",    "8+",          
                                                               "0%", "(0%, 5%]", ">5%",                 
                                                               "Female", "Male"))           
d <- d %>% arrange(intervention_level)


#Set order of RF categories
unique(d$RFtype)
d$RFtype <- factor(d$RFtype , levels=c("Parent background",  "Parent anthro", "Birth characteristics" , "Postnatal child health", "Breastfeeding","SES", "Household","WASH"))      
d <- d %>% arrange(RFtype)


#Maybe make heatmap with 4 columns, leave blank if less than 4 levels, and color grey for reference level.
#Print the name of the category in the box?

RMAestfull <- d %>% filter(RR2!=1) %>% group_by(agecat, outcome_variable, intervention_variable) %>% arrange(intervention_level) %>% 
  mutate(order=row_number()+1) %>% as.data.frame()

#RMAestfull$RR[RMAestfull$RR==1] <- NA #Set reference to NA
#RMAestfull$RR2[RMAestfull$RR2==1] <- NA #Set reference to NA

#Subset to outcome and agecat of interest
groups024 <- c("Parent background",  "Parent anthro", "Birth characteristics" ,"SES","Household", "Postnatal child health")  
groups624 <- c("Postnatal child health","Breastfeeding","WASH")



#Subset to outcome and agecat of interest
RMAestfull <- RMAestfull %>% filter((agecat=="0-24 months\npersistant wasting" & RFtype %in% groups024) |
                                      (agecat=="6-24 months\npersistant wasting" & RFtype %in% groups624)) %>%
  filter(!(intervention_variable=="sex" & agecat=="6-24 months\npersistant wasting")) %>%
  filter(!(intervention_variable=="parity" & agecat=="6-24 months\npersistant wasting")) %>%
  filter(!(intervention_variable=="perdiar6" & agecat=="0-24 months\npersistant wasting"))



#Plot parameters
textcol <- "grey20"
plottitle="Persistant wasting (>=50% of measurements < -2 WLZ)"


# categorize RR
summary(RMAestfull$RR2)
RMAestfull$RRcat <- cut(RMAestfull$RR2,breaks=c(0, 1, 1.05, 1.1, 1.15, 1.2, 1.3, 1.5, 100),labels=c("<1","1-1.05","1.05-1.1","1.1-1.15","1.15-1.2","1.2-1.3","1.3-1.5",">1.5"))
RMAestfull$RRcat <- factor(RMAestfull$RRcat)

# heat map plot scheme
hm <- ggplot(RMAestfull,aes(x=order,y=RFlabel)) +
  #facet_grid(agecat~RFtype, scales='free_y',space='free_y') +
  #facet_wrap(~agecat, nrow=1) +
  facet_grid(RFtype~., scales='free_y',space='free_y') +
  geom_tile(colour="white",size=0.25)+
  scale_x_discrete(expand=c(0,0))+
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
    axis.text.x=element_text(size=12,colour=textcol,angle=0,vjust=0.5),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=0,size=14),
    plot.background=element_blank()) 


hm1 <- hm + aes(fill=RR2) +
  labs(x="Exposure level",y="",title=plottitle) + theme(legend.position="nont") +
  scale_fill_viridis_c(na.value="grey80", guide="colourbar", trans = "log")


hm2 <- hm + aes(fill=RRcat) +
  labs(x="Exposure level",y="",title=plottitle) + theme(legend.position="nont") +
  scale_fill_viridis_d(na.value="grey80", drop=F) + labs(fill="Relative Risk")
#hm2 + geom_label(aes(label=paste0(intervention_level," RR:",round(RR2,2))), color="grey20", fill="white")


setwd("C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures")
ggsave(hm2, file="Wasting_RF_heatmap_persi_wast_.png", width=9.2, height=5.6)



#Set insignificant to missing
RMAestfull$RRcat[RMAestfull$Pvalue >= 0.05] <- NA

hm3 <- ggplot(RMAestfull,aes(x=order,y=RFlabel)) +
  facet_grid(RFtype~., scales='free_y',space='free_y') +
  geom_tile(colour="white",size=0.25)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  theme_bw(base_size=12)+
  theme(
    strip.background = element_blank(),
    legend.title=element_text(color=textcol,size=12),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour=textcol,size=8),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    axis.text.x=element_text(size=12,colour=textcol,angle=0,vjust=0.5),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=0,size=14),
    plot.background=element_blank())  + aes(fill=RRcat) +
  labs(x="Exposure level",y="",title=plottitle) + theme(legend.position="none") +
  scale_fill_viridis_d(na.value="grey80") + labs(fill="Relative Risk") #+ geom_text(aes(label=paste0(intervention_level,"\nRR:",round(RR2,2))))
hm3

ggsave(hm3, file="Wasting_RF_heatmap_greyNA_persi_wast_.png", width=9.2, height=5.6)


#Drop insignificant
RMAestfull_sig <- RMAestfull %>% group_by(intervention_variable) %>% filter(min(Pvalue) < 0.05)

#Shorten printed label
RMAestfull_sig$intervention_level <- gsub("Mildly Food Insecure","Mild Food Ins.",RMAestfull_sig$intervention_level)

hm <- ggplot(RMAestfull_sig,aes(x=order,y=RFlabel)) +
  facet_grid(RFtype~., scales='free_y',space='free_y') +
  geom_tile(colour="white",size=0.25)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
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
    strip.text.y = element_text(angle=0,size=14),
    plot.background=element_blank()) 


hm4 <- hm + aes(fill=RRcat) +
  labs(x="Exposure level",y="",title=plottitle) + theme(legend.position="nont") +
  scale_fill_viridis_d(na.value="grey80", drop=F) + labs(fill="Relative Risk") 
#geom_text(aes(label=paste0(intervention_level,"\nRR:",round(RR2,2))))  




ggsave(hm4, file="Wasting_RF_heatmap_justsig_persi_wast_.png", width=9.2, height=5.6)

hm5 <- hm4 + geom_label(aes(label=paste0(intervention_level," RR:",round(RR2,2))), color="grey20", fill="white", label.padding=unit(0.25, "lines"), size=3)

ggsave(hm5, file="Wasting_RF_heatmap_justsig_lable_persi_wast_.png", width=9.2, height=5.6)
