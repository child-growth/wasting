
#-----------------------------------
# wasting-study-inventory-heatmaps.R
#
# ben arnold (benarnold@berkeley.edu)
#
# create a heatmap of data availability
# for weight and height by study 
# in GHAP using meta-data
# (GHAP_metadata) that Andrew created
# using GHAPStudyMetadata.R
#
#-----------------------------------

#-----------------------------------
# input files:
#    GHAP_metadata.rds (created by GHAPStudyMetadata.R)
#
# output files:
#    wasting-study-inventory-heatmap-n.pdf
#    wasting-study-inventory-heatmap-nbig.pdf
#    wasting-study-inventory-heatmap-prev.pdf
#    wasting-study-inventory-heatmap-n-prev.pdf
#    wasting-study-inventory-heatmap-WHOanonymous.pdf
#-----------------------------------

#-----------------------------------
# preamble
#-----------------------------------
rm(list=ls())
library('dplyr')
library('tidyr')
library('stringr')
library('scales')
library('RColorBrewer')
library('ggplot2')
library('gridExtra')


C:/Users/andre/Documents/HBGDki/Results
setwd('~/HBGDwasting/R/ben/')

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

#-----------------------------------
# load the meta-data table from Andrew (GHAP_metadata)
#-----------------------------------
md <- readRDS('~/HBGDwasting/R/andrew/GHAP_metadata.rds')

# convert wasting prevalence and numsubj to numeric
md$wastprev <- as.numeric(md$wastprev)
md$numsubj <- as.numeric(md$numsubj)
for(i in 1:24){
  ni <- paste("n",i,sep="")
  wi <- paste("wastprev_m",i,sep="")
  md[ni] <- as.numeric(md[,c(ni)])
  md[wi] <- as.numeric(md[,c(wi)])
}

# calculate the total number of measurements
md$nmeas <- rowSums(md[,paste('n',1:24,sep='')],na.rm=TRUE)


#-----------------------------------
# STUDY SELECTION / FILTERING
#-----------------------------------
dd <- md

#-----------------------------------
# 1 restrict to data that is QCd
#-----------------------------------
dim(dd)
table(dd$status=='QC completed')
dd<- filter(dd,status=='QC completed')
dim(dd)

#-----------------------------------
# 2 restrict to longitudinal studies
#-----------------------------------
dim(dd)
table(dd$study_type=='Longitudinal')
dd<- filter(dd,study_type=='Longitudinal')
dim(dd)

#-----------------------------------
# 3 drop higher income countries USA, NLD, Singapore
#-----------------------------------
length(grep('USA',dd$country))
length(grep('NLD',dd$country))
dd <- filter(dd,country!='USA'& country!='NLD' & country!='SGP')
dim(dd)

#-----------------------------------
# 4 have anthro measurements 0-24 months
#-----------------------------------
# table(dd$anthropometric_data=='None'|dd$anthropometric_data=='TBD')
# dd <- filter(dd,anthropometric_data!='None' & anthropometric_data!='TBD')
# dim(dd)

table(dd$nmeas<=0 | is.na(dd$nmeas))
dd_nomeas <- filter(dd,nmeas<=0 | is.na(dd$nmeas))
select(dd_nomeas,study_id,short_description)

dd <- filter(dd,nmeas>0 & !is.na(dd$nmeas))
dim(dd)

#-----------------------------------
# 5 drop studies with either wrong
# study designs (GEMS) or not sufficiently
# high resolution measurements (WASH B)
# N=9
# bigcs_ultrasound (birth + 12 mos)
# iLiNS DOSE and iLiNS DYAD (every 6 mos)
# Amanhi (only birth outcomes)
# Peru Zinc (every 2 months)
# ZnMort Bhandari et al 2007 (http://jn.nutrition.org/content/137/1/112.full) 
#-----------------------------------
#wrong_design <- c('WASH-Bangladesh','WASH-Kenya','BIGCS Ultrasound','iLiNS-DOSE','iLiNS-DYAD-M','IMNCI','AMANHI','Peru Zn','ZnMort')
wrong_design <- c('BIGCS Ultrasound','AMANHI', 'IMNCI')

dd <- dd[!(dd$study_id %in% wrong_design),]
dim(dd)


#-----------------------------------
# 6 drop studies with non-representative
# study populations
#
# N=5
# PeruPersistentDiarrhea: Children in Lima, Peru with persistent diarrhea
# Ecuador Zinc: Children with HAZ < -1.3
# DIVIDS: Low birth weight term newborns born in a large government hospital serving a low income population in Delhi, India
# ZincInf:  Infants with acute diarrhea living in India, Pakistan and Ethiopia
# GRIP: Children aged < 5 y in Ali Akber Shah goth, Karachi, with lower respiratory tract infection
#-----------------------------------
nonrepres <- c('Peru PersistDiarrhea','Ecuador Zn','DIVIDS','ZincInf','Grip')
dd_nonrep <-dd[(dd$study_id %in% nonrepres),]
dd <- dd[ !(dd$study_id %in% nonrepres),]
dim(dd)

#-----------------------------------
# drop studies that are too small
# N=3
#-----------------------------------
toosmall <- c('Ecuador Egg','Bangladesh Diarrhea', "Peru Zn")
dd_small <- filter(dd,study_id=='Ecuador Egg'|study_id=='Bangladesh Diarrhea'|study_id=='Peru Zn')
dd <- filter(dd,study_id!='Ecuador Egg' & study_id!='Bangladesh Diarrhea' & study_id!='Peru Zn')
dim(dd)

#-----------------------------------
# Dropped studies flagged for 
# questionable anthropometry measurements
#-----------------------------------
dd <- dd[-which(dd$study_id=='MAL-ED' & dd$countrycohort=='PAKISTAN'),]
dim(dd)


#-----------------------------------
# seperate cohorts into monthly, quarterly, or yearly
# and drop any non-intervention cohorts with only yearly 
# measurements
#-----------------------------------
dd$cohort <- paste0(dd$study_id," ", dd$countrycohort)
dd$median_length_between_measures <- as.numeric(dd$median_length_between_measures)
paste0(unique(dd$cohort[dd$median_length_between_measures<=40]), ": ",unique(dd$median_length_between_measures[dd$median_length_between_measures<=40]))
paste0(unique(dd$cohort[dd$median_length_between_measures>40 & dd$median_length_between_measures<=100]), ": ",unique(dd$median_length_between_measures[dd$median_length_between_measures>40 & dd$median_length_between_measures<=100]))
paste0(unique(dd$cohort[dd$median_length_between_measures>100]), ": ",unique(dd$median_length_between_measures[dd$median_length_between_measures>100]))

monthly <- c('gms_nepal','respak','irc','divids','ee','cmc_v_bcs_2002','mal_ed','tanzaniachild2','cmin','guatemala_bsc','incap','tdc','peru_huascar','content')
dd$monthly<-0
dd$monthly[dd$fstudy_id %in% monthly] <- 1

# Two of the CMIN cohorts (Brazil and Guinea-Bissau)  do not have monthly measurments
dd$monthly[dd$study_id=="CMIN" & dd$countrycohort=="BRAZIL"] <- 0
dd$monthly[dd$study_id=="CMIN" & dd$countrycohort=="GUINEA-BISSAU"] <- 0

#Mark less-than-quarterly studies
yearly <- c('jivita_3','wash_bangladesh','wash_kenya','ilins_dose','ilins_dyad_m','znmort')
dd$yearly<-0
dd$yearly[dd$fstudy_id %in% yearly] <- 1

dd$measure_freq<-"Quarterly Measurements"
dd$measure_freq[dd$monthly==1]<-"Monthly Measurements"
dd$measure_freq[dd$yearly==1]<-"Yearly"
dd$measure_freq <- factor(dd$measure_freq)

temp <- cbind(dd$study_id, dd$countrycohort, dd$median_length_between_measures, dd$measure_freq, dd$numobs)

#drop any non-intervention cohorts with only yearly measurements
dd <- dd[dd$cohort!="COHORTS BRAZIL" & dd$cohort!="COHORTS SOUTH AFRICA",]


#-----------------------------------
# basic counts and total summary stats
#-----------------------------------
# number of cohorts with monthly measurement
sum(dd$monthly==1)
# number of children
sum(dd$numsubj[dd$monthly==1])
# number of child-months
sum(dd$nmeas[dd$monthly==1])

# number of cohorts with quarterly measurement
sum(dd$monthly==0 & dd$yearly==0)
# number of children
sum(dd$numsubj[dd$monthly==0 & dd$yearly==0])
# number of child-months
sum(dd$nmeas[dd$monthly==0 & dd$yearly==0])


# number of cohorts with yearly measurement
sum(dd$yearly==1)
# number of children
sum(dd$numsubj[dd$yearly==1])
# number of child-months
sum(dd$nmeas[dd$yearly==1])


#overall totals
# number of cohorts, children, child-months
nrow(dd)
sum(dd$numsubj)
sum(dd$nmeas)

#-----------------------------------
# Do some final tidying up for the plot
#-----------------------------------

# shorten the description for a few studies
dd$short_description[dd$study_id=='CONTENT'] <- 'Eval & Control of Negl. Mucosal Enteric Inf'
dd$short_description[dd$study_id=='LCNI-5'] <- 'Lungwena Child Nutr Int Study'
dd$short_description[dd$study_id=='Burkina Faso Zn'] <- 'Zinc Int Trial'
dd$short_description[dd$study_id=='AgaKhanUniv'] <- 'Aga Khan Evidence Based Nutr Int Study'
dd$short_description[dd$study_id=='SAS-FoodSuppl'] <- 'Food Suppl Trial'
dd$short_description[dd$study_id=="MAL-ED"] <- 'MAL-ED Study'
dd$short_description[dd$study_id=='CMIN'] <- 'Child Malnutrition and Inf Network'
dd$short_description[dd$study_id=='Guatemala BSC'] <- 'Longitudinal study of BSC'
dd$short_description[dd$study_id=='Peru Huascar'] <- 'Infant growth in Huascar'
dd$short_description[dd$study_id=='EE'] <- 'Study of Biomarkers for EE'
dd$short_description[dd$study_id=='GMS-Nepal'] <- 'Growth Monitoring Study'
dd$short_description[dd$study_id=='NIH-Crypto'] <- 'NIH Crypto study'

# simplify Tanzania label
dd$countrycohort[dd$countrycohort=='TANZANIA, UNITED REPUBLIC OF'] <- 'TANZANIA'

# make a study-country label, and make the monthly variable into a factor
# including an anonymous label (temporary) for sharing with WHO
dd <- mutate(dd,
             country=str_to_title(str_to_lower(countrycohort)), 
             studycountry=paste(short_description,'-',country))

dd <- mutate(dd,
             monthly=factor(monthly,levels=c(1,0),
                            labels=c('Monthly Measurements','Quarterly Measurements')),
             studycountry = factor(studycountry,
                                   levels=unique(studycountry[order(monthly,wastprev)]), 
                                   ordered=TRUE),
             anonym = factor(paste("Cohort",1:nrow(dd),dd$country)),
             anonym = factor(anonym,levels=unique(anonym[order(monthly,wastprev)]),
                             ordered=TRUE)
             )

# categorize wasting prevalence
dd$wpcat <- cut(dd$wastprev,breaks=c(0,5,10,15,20,25,100),labels=c("<5","5-10","10-15","15-20","20-25",">25"))
dd$wpcat <- factor(dd$wpcat)


#Create dataset of studies we are using
colnames(dd)

studylist <- dd[,c(1:10,29:38)] %>% distinct()
library(xlsx)
write.xlsx(studylist, "UCBerkeley_wasting_studylist.xlsx")
saveRDS(studylist, "UCBerkeley_wasting_studylist.rds")
#-----------------------------------
# Create a long format dataset
# for ggplot2
#-----------------------------------

# gather N measurements by month data into long format
dnsubj <- select(dd,study_id,anonym,country,studycountry,measure_freq,wastprev,starts_with('n')) %>%
  select(-neurocog_data,-nutrition,-notes,-num_countries,-numcountry,-numsubj,-numobs,-nmeas) %>%
  gather(age,nobs,-study_id,-anonym,-country,-studycountry,-measure_freq,-wastprev) %>%
  mutate(age=as.integer(str_sub(age,2,-1)),nobs=as.integer(nobs)) %>%
  select(study_id,anonym,country,studycountry,measure_freq,wastprev,age,nobs) %>%
  filter(age>=1 & age <=24 ) %>%
  arrange(measure_freq,wastprev)

# gather wasting prev by month data into long format
dwastp <- select(dd,study_id,anonym,country,studycountry,starts_with('wastprev_m')) %>%
  gather(age,wp,-study_id,-anonym,-country,-studycountry) %>%
  mutate(age=as.integer(str_sub(age,11,-1)),wp=as.numeric(wp)) %>%
  select(study_id,anonym,country,studycountry,age,wp) %>%
  filter(age>=1 & age <=24 )

# join the long tables together and sort countries by measure_freq and wasting prev
dp <- left_join(dnsubj,dwastp,by=c('study_id','anonym','country','studycountry','age'))
  

# categorize wasting prevalence, set wasting prevalence category estimates to missing if n<50
dp$wpcat <- cut(dp$wp,breaks=c(0,5,10,15,20,25,100),labels=c("<5","5-10","10-15","15-20","20-25",">25"))
dp$wpcat <- factor(dp$wpcat)
dp$wpcat[dp$nobs<50 | is.nan(dp$wp)] <- NA

# categorize number of observations
dp$ncat <- cut(dp$nobs,
               breaks=c(1,50,seq(200,1400,by=200),100000),
               labels=c('<50','50-200','200-400','400-600','600-800','800-1000','1000-1200','1200-1400','>1400'))
dp$ncat <- factor(dp$ncat)


#-----------------------------------
# Basic plot schemes
#
# there is one for a heat map and 
# a second for a side bar plot
# to summarize a single dimension
# (such as N or wasting prevalence)
#
# there is a little bit of 
# a trick here  to ensure that
# they have the two plots render
# have the same exact dimensions
# so that the line up properly in
# the grid.arrange() function.
# to do this, you need to create a 
# "ghost" legend in the side bar
# using a fake fill.  It uses the 
# aes.overide arguments to make 
# everything white so that you can't
# see it.
#-----------------------------------

#define a color for fonts
textcol <- "grey20"

# heat map plot scheme
hm <- ggplot(dp,aes(x=age,y=studycountry)) +
  # facet over measurement frequency
  facet_grid(measure_freq~.,scales='free_y',space='free_y') +
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),
                     breaks=1:24,labels=1:24)+
  #one unit on x-axis is equal to one unit on y-axis.
  #equal aspect ratio x and y axis
  coord_equal()+
  #set base size for all font elements
  theme_grey(base_size=10)+
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
    panel.border=element_blank(),
    #remove plot margins
    # plot.margin=margin(grid::unit(1,"cm"))
  )


# side bar plot scheme
sidebar <- ggplot(data = dd, aes(x = studycountry)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  facet_grid(measure_freq~.,scales='free_y',space='free_y') +
  #remove extra space
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_manual(values=rep('gray70',5),na.value="grey90",
                    guide=guide_legend(title="",title.hjust = 0.5,
                                       label.position="bottom",label.hjust=0.5,nrow=1,
                                       override.aes = list(color = "white", fill="white"))) +
  theme_grey(base_size=10) +
  theme(
    # legend options
    # has to be the exact same format as for the other panel (for correct alignment)
    legend.title=element_text(color=textcol,size=8),
    #reduce/remove legend margin
    legend.margin = margin(grid::unit(0.1,"cm")),
    #change legend text properties
    legend.text=element_text(colour=NA,size=7,face="bold"),
    #change legend key height
    legend.key.height=grid::unit(0.2,"cm"),
    #set a slim legend
    legend.key.width=grid::unit(0.2,"cm"),
    #move legend to the bottom
    legend.position = "bottom",
    # remove study labels
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    #adjust facet labels
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    # x-axis labels
    axis.title.x = element_text(size=10),
    # title has to be the exact same format as for the other panel (for correct alignment)
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    # remove grid lines
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
    
  )


#-----------------------------------
# WASTING PREVALENCE HEAT MAP
#-----------------------------------

# heat map
wphm <- hm +
  aes(fill=wpcat) +
  labs(x="Age in months",y="",title="Wasting prevalence by month of age") +
  scale_fill_brewer(palette = "YlOrRd",na.value="grey90",
                    guide=guide_legend(title="Wasting (%)",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.5,nrow=1))
  
# wasting prevalence side bar plot
wpbar <- sidebar +
  aes(y=wastprev,fill=wpcat) +
  labs(x = "",y="Overall Prevalence (%)",title="Wasting (%)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,22.5),
                     breaks=seq(0,20,by=5),labels=seq(0,20,by=5)) +
  geom_hline(yintercept = seq(0,20,by=5),color='white',size=0.3)
  
  
# combined plot
wpgrid <- grid.arrange(wphm, wpbar, nrow = 1, ncol = 2, widths=c(100,20))
ggsave(filename="wasting-study-inventory-heatmap-prev.pdf",plot = wpgrid,device='pdf',width=10,height=9)



#-----------------------------------
# measurement heat map
#-----------------------------------

nhm <- hm +
  aes(fill=ncat) +
  labs(x="Age in months",y="",title="N children measured by month of age") +
  scale_fill_brewer(palette = "Greens",na.value="grey90",
                    guide=guide_legend(title="Number of Measurements",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.5,nrow=1))

nbar <- sidebar +
  aes(y=nmeas/1000,fill=wpcat) +
  labs(x = "",y="Child-Months (x1000)",title="Sample size") +
  scale_y_continuous(expand=c(0,0),limits=c(0,125),
                     breaks=seq(0,120,by=20),labels=seq(0,120,by=20)) +
  geom_hline(yintercept = seq(0,120,by=20),color='white',size=0.3) 


ngrid <- grid.arrange(nhm, nbar, nrow = 1, ncol = 2, widths=c(100,20))
ggsave(filename="wasting-study-inventory-heatmap-n2.pdf",plot = ngrid,device='pdf',width=10,height=9)


#-----------------------------------
# measurement heat map with wasting
# prevalence
#-----------------------------------
ngridbig <- grid.arrange(nhm,nbar,wpbar,nrow=1,ncol=3,widths=c(100,20,20))
ggsave(filename="wasting-study-inventory-heatmap-nbig2.pdf",plot = ngridbig,device='pdf',width=12,height=9)


#-----------------------------------
# giant panel of both heat maps
#-----------------------------------
hmbiggest <- grid.arrange(nhm,nbar,wphm,wpbar,nrow=1,ncol=4,widths=c(100,20,100,20))
ggsave(filename="wasting-study-inventory-heatmap-n-prev2.pdf",plot = hmbiggest,device='pdf',width=20,height=9)

#-----------------------------------
# anonymized heatmap for WHO
#-----------------------------------
nhma <- nhm + aes(y=anonym) + ylab("")
wphma <- wphm + aes(y=anonym) + ylab("")
hmwho <- grid.arrange(nhma,nbar,wphma,wpbar,nrow=1,ncol=4,widths=c(100,20,100,20))
ggsave(filename="wasting-study-inventory-heatmap-WHOanonymous2.pdf",plot = hmwho,device='pdf',width=18,height=9)







#-----------------------------------
# presentation plot- prevalence only among monthly cohorts
#-----------------------------------

# heat map plot scheme
d_month <- dp[dp$measure_freq=="Monthly Measurements",]

hm_month <- ggplot(d_month,aes(x=age,y=studycountry)) +
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),
                     breaks=1:24,labels=1:24)+
  #one unit on x-axis is equal to one unit on y-axis.
  #equal aspect ratio x and y axis
  coord_equal()+
  #set base size for all font elements
  theme_grey(base_size=10)+
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
  ) +
  aes(fill=wpcat) +
  labs(x="Age in months",y="",title="Wasting prevalence by month of age") +
  scale_fill_brewer(palette = "YlOrRd",na.value="grey90",
                    guide=guide_legend(title="Wasting (%)",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.5,nrow=1))
 
hm_month

