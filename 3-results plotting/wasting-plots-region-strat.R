


#-----------------------------------
# Stunting webinar plots
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
source("C:/Users/andre/Documents/HBGDki/Stunting/1-outcomes/0_st_basefunctions.R")

load("C:/Users/andre/Documents/HBGDki/Results/Wasting_descriptive_epi_results.Rdata")

setwd("C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures/")



fit.cont.rma=function(data,age,yi,vi,ni,nlab){
  data=filter(data,agecat==age)
  
  fit <- rma(yi=data[[yi]], vi=data[[vi]], method="REML")
  
  out = data %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(data[[ni]][agecat==age])) %>%
    mutate(agecat=age,est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
           nmeas.f=paste0("N=",format(sum(data[[ni]]),big.mark=",",scientific=FALSE),
                          " ",nlab),
           nstudy.f=paste0("N=",nstudies," studies"))
  return(out)
}

# estimate random effects, format results
region.rma <- function(d, region="Asia", measure="prev", agecat.list=list("Birth","3 months","6 months","9 months","12 months","18 months","24 months"), resdf=NULL){
  
  
  if(measure=="prev"){
    res=lapply(agecat.list,function(x) 
      fit.rma(data=d[d$region==region,],ni="nmeas", xi="nxprev",age=x,measure="PR",nlab="children"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100,lb=lb*100,ub=ub*100)
    res$agecat=factor(res$agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months"))
    res$ptest.f=sprintf("%0.0f",res$est)
  }
  if(measure=="cuminc"|measure=="rec60"|measure=="pers_wast"){
    res=lapply(agecat.list,function(x)
      fit.rma(data=d[d$region==region,],ni="N", xi="ncases",age=x,measure="PR",nlab=" at-risk"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100, lb=lb*100, ub=ub*100)

    res$ptest.f=sprintf("%0.0f",res$est)
  }
  
  if(measure=="IR"|measure=="recIR"|measure=="IRnobirth"|measure=="recIRnobirth"){
    res=lapply(list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"),function(x)
      fit.rma(data=d[d$region==region,], ni="ptar", xi="ncase",age=x,measure="IR",nlab=" person-days"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    
    res$pt.f=paste0("N=",format(res$nmeas,big.mark=",",scientific=FALSE),
                       " person-days")
    res$ptest.f=sprintf("%0.02f",res$est*1000)
  }
  
  if(measure=="rec"){
    res=lapply(agecat.list,function(x) 
                          fit.rma(data=d[d$region==region,],ni="N", xi="n",age=x,measure="PR",
                                  nlab="children"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100,lb=lb*100,ub=ub*100)

    
    res$ptest.f=sprintf("%0.1f",res$est)
  }
  if(measure=="duration"){
    d$var<-((d$Mean - d$Lower.95.CI)/1.96)^2
    res=lapply(agecat.list,function(x) 
      fit.cont.rma(data=d[d$region==region,],yi="Mean", vi="var", ni="N",age=x, nlab="children"))
    res=as.data.frame(do.call(rbind, res))
  }
  
  res <- data.frame(measure, region, res)
  resdf <- bind_rows(resdf, res)
  return(resdf)
}


res <- region.rma(prev.data$prev.cohort)
res <- region.rma(prev.data$prev.cohort, region="Africa", resdf=res)
res <- region.rma(prev.data$prev.cohort, region="Latin America", resdf=res)

# res <- region.rma(ci.data$ci.cohort, measure="cuminc", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
# res <- region.rma(ci.data$ci.cohort, region="Africa", measure="cuminc", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
# res <- region.rma(ci.data$ci.cohort, region="Latin America", measure="cuminc", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)

res <- region.rma(ir.data$ir.cohort, measure="IR", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
res <- region.rma(ir.data$ir.cohort, region="Africa", measure="IR", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
res <- region.rma(ir.data$ir.cohort, region="Latin America", measure="IR", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)

res <- region.rma(rec.data60$ci.cohort, measure="rec60", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)
res <- region.rma(rec.data60$ci.cohort, region="Africa", measure="rec60", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)
res <- region.rma(rec.data60$ci.cohort, region="Latin America", measure="rec60", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)

res <- region.rma(ir.data.nobirth$ir.cohort, measure="IRnobirth", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
res <- region.rma(ir.data.nobirth$ir.cohort, region="Africa", measure="IRnobirth", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
res <- region.rma(ir.data.nobirth$ir.cohort, region="Latin America", measure="IRnobirth", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)

# res <- region.rma(rec.ir.data.nobirth$ir.cohort, measure="recIRnobirth", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
# res <- region.rma(rec.ir.data.nobirth$ir.cohort, region="Africa", measure="recIRnobirth", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
# res <- region.rma(rec.ir.data.nobirth$ir.cohort, region="Latin America", measure="recIRnobirth", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)

# res <- region.rma(poolhaz, measure="vel", agecat.list=list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"), resdf=res)
# res <- region.rma(poolhaz, region="Africa", measure="vel", agecat.list=list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"), resdf=res)
# res <- region.rma(poolhaz, region="Latin America", measure="vel", agecat.list=list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"), resdf=res)

res <- region.rma(perswast.data$pers.cohort, measure="pers_wast", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)
res <- region.rma(perswast.data$pers.cohort, region="Africa", measure="pers_wast", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)
res <- region.rma(perswast.data$pers.cohort, region="Latin America", measure="pers_wast", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)


head(res)

res$region <- factor(res$region, levels=c("Asia","Africa","Latin America"))

res$measure_name <- NA
res$measure_name[res$measure=="prev"] <- "Prevalence"
res$measure_name[res$measure=="cuminc"] <- "Cumulative incidence"
res$measure_name[res$measure=="incprop"] <- "Incidence proportion"
res$measure_name[res$measure=="IR"] <- "Incidence rate"
res$measure_name[res$measure=="IRnobirth"] <- "Incidence rate - no birth"
res$measure_name[res$measure=="recIR"] <- "Recovery incidence rate"
res$measure_name[res$measure=="recIRnobirth"] <- "Recovery incidence rate - no birth"
res$measure_name[res$measure=="vel"] <- "LAZ change"
res$measure_name <- factor(res$measure_name, levels=c("Prevalence", "Cumulative incidence", "Incidence proportion", "Recovery",  "LAZ change"))

res$agecat <- gsub("months", "mo.", res$agecat)
res$agecat <- factor(res$agecat, levels=unique(res$agecat))


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



res <- res %>% rename(Region=region)





# legend<-ggplot(res[res$measure=="prev",],aes(y=est,x=agecat))+
#   geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = 4) +
#   geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = 3) +
#   scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
#   xlab("Age category")+ ylab("%")+ ggtitle("Prevalence") +
#   theme(legend.position="right", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
#   
# ggsave(legend, file="region_legend.png", width=10, height=4)


unique(res$measure)

a <- 3
b <- 2.5
  
p1 <- ggplot(res[res$measure=="prev",],aes(y=est,x=agecat))+
    geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
    geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
    scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
    xlab("Age category")+ ylab("%")+ ggtitle("A) Prevalence") +
    theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
# p2 <- ggplot(res[res$measure=="cuminc",],aes(y=est,x=agecat))+
#   geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
#   geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
#   scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
#   xlab("Age category")+ ylab("%")+ ggtitle("Cumulative incidence") +
#   theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p2 <- ggplot(res[res$measure=="IR",],aes(y=est*1000,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb*1000, ymax=ub*1000, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  coord_cartesian(ylim=c(0, 7.5)) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("IR per 1000\nperson-days")+ ggtitle("B) Incidence rate") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p2
p3 <- ggplot(res[res$measure=="IRnobirth",],aes(y=est*1000,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb*1000, ymax=ub*1000, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  coord_cartesian(ylim=c(0, 3)) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("IR per 1000\nperson-days")+ ggtitle("D) Incidence rate - no birth wasting") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p3
# p5 <- ggplot(res[res$measure=="recIR",],aes(y=est*1000,x=agecat))+
#   geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
#   geom_linerange(aes(ymin=lb*1000, ymax=ub*1000, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
#   coord_cartesian(ylim=c(0, 45)) +
#   scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
#   xlab("Age category")+ ylab("%")+ ggtitle("A) Recovery rate") +
#   theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
# p5






p4 <- ggplot(res[res$measure=="rec60",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("%")+ ggtitle("C) Recovery within 60 days") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p4


multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL, title="", 
                      fontsize = 12, fontfamily = "Helvetica") {
  require(grid)
  
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
  
  if (nchar(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (nchar(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    
    # Make each plot, in the correct location
    if (nchar(title)>0) {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily))
    }
    
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


jpeg("C:/Users/andre/Dropbox/HBGDki figures/Berlin wasting figures/region_strat_descriptive_epi_wasting.jpeg", width = 10, height = 4, units = 'in', res = 400)
  multiplot( p1, p4,  p2, p3, cols=2, title="Region-stratified descriptive statistics of wasting")
dev.off()



