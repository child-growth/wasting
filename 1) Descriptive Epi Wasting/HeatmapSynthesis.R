# load packages
rm(list=ls())
library(knitr)
library(markdown)
library(rmarkdown)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)

setwd("U:/Perminant files/R scripts/Risk factor analysis Rally 4b")
source("4b_RiskFactor_function.R")

setwd("U:/data/Rally4b_data")



# multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}










load("gmsn_inc.Rdata")
load("cntt_inc.Rdata")
load("gbsc_inc.Rdata")
load("cmin_inc.Rdata")
load("phua_inc.Rdata")
load("tzc2_inc.Rdata")
load("cmc_inc.Rdata")
load("ee_inc.Rdata")
load("irc_inc.Rdata")
load("tdc_inc.Rdata")
load("rspk_inc.Rdata")
load("mled_inc.Rdata")


#Add one severe wasting event to content peru so graph color works 
cntt_inc$WHZ[cntt_inc$WHZ== -2.9] <- -3.1



#Define set of short ID's I care about
datasets <- list(
  gmsn_inc, 
  cntt_inc, 
  gbsc_inc, 
  cmin_inc_brazil, 
  cmin_inc_guinea_bissau, 
  cmin_inc_peru, 
  cmin_inc_bangladesh,   
  phua_inc, 
  tzc2_inc, 
  cmc_inc, 
  ee_inc, 
  irc_inc,
  tdc_inc,
  rspk_inc, 
  mled_inc_bangladesh,
  mled_inc_brazil,             
  mled_inc_india,
  mled_inc_nepal,
  mled_inc_pakistan,           
  mled_inc_peru,
  mled_inc_southafrica, 
  mled_inc_tanzania)


#Make list of summary tables
tablelist <- list(
  gmsn_inc_table, 
  cntt_inc_table, 
  gbsc_inc_table, 
  cmin_inc_table_brazil, 
  cmin_inc_table_guinea_bissau, 
  cmin_inc_table_peru, 
  cmin_inc_table_bangladesh,   
  phua_inc_table, 
  tzc2_inc_table, 
  cmc_inc_table, 
  ee_inc_table, 
  irc_inc_table,
  tdc_inc_table,
  rspk_inc_table, 
  mled_inc_table_bangladesh,
  mled_inc_table_brazil,             
  mled_inc_table_india,
  mled_inc_table_nepal,
  mled_inc_table_pakistan,           
  mled_inc_table_peru,
  mled_inc_table_southafrica,
  mled_inc_table_tanzania)




#Define the corresponding study names
studynames <- c("GMS-Nepal (Growth Monitoring Study, Nepal)",
                "CNTT (Evaluation and Control of Neglected Mucosal Enteric Infections in Childhood)",
                "GBSC (Longitudinal study of Bovine Serum Concentrate in Guatemala)",
                "CMIN (Child Malnutrition and Infection Network): Brazil",
                "CMIN (Child Malnutrition and Infection Network): Guinea Bissau",
                "CMIN (Child Malnutrition and Infection Network): Peru",
                "CMIN (Child Malnutrition and Infection Network): Bangladesh",
                "PHUA (Infant Growth in Peru)",
                "TZC2 (Tanzania Child 2)",
                "CMC Vellore Birth Cohort 2002",
                "EE (Pakistan Study of Biomarkers for Environmental Enteropathy)",
                "IRC (Vellore Crypto Study)",
                "TDC (Vellore Bottled Water BC)",
                "RSPK (Respiratory Pathogens Birth Cohort)",
                "MAL-ED Study: Bangladesh",
                "MAL-ED Study: Brazil",
                "MAL-ED Study: India",
                "MAL-ED Study: Nepal",
                "MAL-ED Study: Pakistan",
                "MAL-ED Study: Peru",
                "MAL-ED Study: South Africa",
                "MAL-ED Study: Tanzania"
)




heatmap(datasets[[2]])

library(tidyverse)
plot_df_list<-list()
for(i in 1:length(datasets)){
  temp<- as.data.frame(datasets[[i]])
  #plot_df_list[[i]]<- subset(temp, select=c(COUNTRY, STUDYID,WHZ, SUBJID, AGEDAYS))
  plot_df_list[[i]]<- temp[, colnames(temp) %in% c("COUNTRY", "STUDYID","WHZ", "SUBJID", "AGEDAYS")] #%>% select(COUNTRY, STUDYID,WHZ, SUBJID, AGEDAYS)
}
d<-NULL
for(i in 1:length(datasets)){
  d<-rbind(d,plot_df_list[[i]])
}  


#Heatmap
  theme_set(theme_bw())
  
  #generate whz categories
  d$wastcat <- 0
  d$wastcat[d$WHZ<(-2)] <- -1
  d$wastcat[d$WHZ<(-3)] <- -2
  
  #Sum an ad-hoc total wasting score to rank children by  
  d <- d %>% group_by(STUDYID, SUBJID) %>% mutate(wastscore=sum(wastcat, na.rm=T)) %>% ungroup()
  table(d$wastscore)
  
  #Add level for missingness
  d$wastcat[is.na(d$WHZ)] <- 1
  d$wastcat <- as.factor(d$wastcat)
  d$SUBJID <- as.factor(d$SUBJID)
  table(d$wastcat)
  
  #make ordered childnum by amount of wasting
  d <- d %>% group_by(STUDYID, COUNTRY) %>%
    arrange(wastscore) %>%  
    mutate(SUBJID = factor(SUBJID, unique(SUBJID)),
           childnum=as.numeric(SUBJID)) 
  head(as.data.frame(d))
  
  #Create a child age in months variable
  d$agemonths<-floor(d$AGEDAYS/30)
  
  
  if(sum(d$wastcat== -2) > 0){
    levels(d$wastcat)<-c("Severely wasted", "Wasted", "Not wasted")
    cbPalette <- c("#56B4E9", "#E69F00", "#efefef")
  }else{
    levels(d$wastcat)<-c( "Wasted", "Not wasted")
    cbPalette <- c( "#E69F00", "#efefef")   
  }
  
  #Create unique country/study combination
  d$STUDYID<-gsub("^k.*?-","",d$STUDYID)
  d$COUNTRY[d$COUNTRY=="TANZANIA, UNITED REPUBLIC OF"] <- "TANZANIA"
  d$country_cohort<-paste0(d$STUDYID, "\n", d$COUNTRY)
  length(unique(d$country_cohort))
  #Group by region
  table(d$COUNTRY)

  d$region <- NA
  d$region[d$COUNTRY=="TANZANIA"|d$COUNTRY=="SOUTH AFRICA"|d$COUNTRY=="GUINEA-BISSAU"]<-2
  d$region[d$COUNTRY=="BANGLADESH"|d$COUNTRY=="PAKISTAN"|d$COUNTRY=="INDIA"|d$COUNTRY=="NEPAL"]<-1
  d$region[d$COUNTRY=="GUATEMALA"|d$COUNTRY=="PERU"|d$COUNTRY=="BRAZIL"]<-3

  d <- d %>% arrange(region)


  
  myplots<-list()
  for(i in 1:22)
    local({
      i <- i
      temp <-d[d$country_cohort==unique(d$country_cohort)[i],] %>% arrange(wastscore)  %>% mutate(childnum = factor(childnum, unique(childnum)))
      p1 <- ggplot(temp,aes(x=agemonths,y=childnum)) + 
        geom_tile(aes(fill=wastcat)) + scale_fill_manual(values=cbPalette) +
        xlab("") + ylab("") +
      ggtitle(unique(d$country_cohort)[i]) +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.title = element_text(size = 10)) 
    
      myplots[[i]] <<- p1  # add each plot into plot list
    })
  
  
  


  # 
  # #Extract Legend 
  # p_leg <- ggplot(temp,aes(x=agemonths,y=childnum)) + 
  #   geom_tile(aes(fill=wastcat)) + scale_fill_manual(values=cbPalette) +
  #   xlab("") + ylab("") +
  #   ggtitle(unique(d$country_cohort)[i]) +
  #   theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),axis.ticks=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank(),
  #         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #         plot.title = element_text(size = 10)) 
  # g_legend<-function(a.gplot){ 
  #   tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  #   legend <- tmp$grobs[[leg]] 
  #   return(legend)} 
  # 
  # legend <- g_legend(p_leg) 
  #  
  # 
  # 

  multiplot(myplots[[1]], 
            myplots[[12]], 
            myplots[[16]], 
            myplots[[2]],
            myplots[[13]],
            myplots[[17]],
            myplots[[3]],
            myplots[[14]],
            myplots[[18]],
            myplots[[4]],
            myplots[[15]],
            myplots[[19]],
            myplots[[5]],
            plot.new(),
            myplots[[20]],
            myplots[[6]],
            plot.new(),
            myplots[[21]],
            myplots[[7]],
            plot.new(),
            myplots[[22]],
            myplots[[8]],
            plot.new(),
            plot.new(),
            myplots[[9]],
            plot.new(),
            plot.new(),
            myplots[[10]],
            plot.new(),
            plot.new(),
            myplots[[11]],
            plot.new(),
            plot.new(),
            cols=11)
            
            
            multiplot(myplots[[1]], 
                      myplots[[2]], 
                      myplots[[3]], 
                      myplots[[4]],
                      myplots[[5]],
                      myplots[[6]],
                      myplots[[7]],
                      myplots[[8]],
                      myplots[[9]],
                      myplots[[10]],
                      myplots[[11]],
                      plot.new(),
                      cols=4)
            
            
            
            
            multiplot(myplots[[12]], 
                      myplots[[13]], 
                      myplots[[14]], 
                      myplots[[15]],
                      cols=2)
            
            
            
            multiplot(myplots[[16]], 
                      myplots[[17]], 
                      myplots[[18]], 
                      myplots[[19]],
                      myplots[[20]],
                      myplots[[21]],
                      myplots[[22]],
                      cols=4)
            
            

  
  # temp <-d[d$country_cohort=="Peru Huascar PERU",]  %>% arrange(wastscore)
  # head(temp)
  # 
  # ggplot(temp,aes(x=agemonths,y=childnum)) + 
  #   geom_tile(aes(fill=wastcat)) + scale_fill_manual(values=cbPalette) +
  #   xlab("") + ylab("") +
  #   ggtitle(unique(d$country_cohort)[i]) +
  #   theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),axis.ticks=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank(),legend.position="none",
  #         plot.title = element_text(size = 10))
  # 
  # temp <- temp %>% mutate(childnum = factor(childnum, unique(childnum)))
  # 
  # 
  
