


rm(list=ls())




source("U:/R scripts/Wast_incidence_functions.R")
setwd("U:/data/WastIncDatasets")

# library(mgcv)
# library(caret)
library(tidyverse)
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")




load("U:/data/Compiled Datasets/CompiledLongData.Rdata")
d<-d[,-c(1:2)]
colnames(d)
dlong<-d

load("U:/data/Compiled Datasets/WastInc0-24.Rdata")
colnames(d)

d<-d %>% ungroup() %>% mutate(MWTKG_cat = factor(ntile(MWTKG,4)), MHTCM_cat = factor(ntile(MHTCM,4))) %>% subset(., select=c(STUDYID, SUBJID, COUNTRY, MWTKG, MHTCM, MWTKG_cat, MHTCM_cat)) %>% ungroup() %>%
  group_by(STUDYID, COUNTRY) %>% mutate(study_MWTKG_cat = factor(ntile(MWTKG,4)), study_MHTCM_cat = factor(ntile(MHTCM,4)))

table(d$STUDYID, d$study_MWTKG_cat)
table(d$STUDYID, d$MWTKG_cat)

d<-left_join(dlong, d, by=c("STUDYID","SUBJID"))
#remove grant identifiers
d$STUDYID<- gsub("^k.*?-" , "", d$STUDYID)
d1<-d %>% filter(AGEDAYS < 24*30.25 & !is.na(MWTKG_cat))
d2<-d %>% filter(AGEDAYS < 24*30.25 & !is.na(MHTCM_cat))

p3<-ggplot(aes(x=AGEDAYS, y=WHZ, group=MWTKG_cat, colour=MWTKG_cat), data=d1) +
  geom_smooth()  + scale_color_brewer(palette = "Oranges", direction= -1) + ggtitle("WHZ trajectory stratified\nby maternal weight") + theme(panel.border = element_blank(), strip.background = element_blank())

p4<-ggplot(aes(x=AGEDAYS, y=WHZ, group=MHTCM_cat, colour=MHTCM_cat), data=d2) +
  geom_smooth()  + scale_color_brewer(palette = "Oranges", direction= -1) + ggtitle("WHZ trajectory stratified\nby maternal height") + theme(panel.border = element_blank(), strip.background = element_blank())

p2<-ggplot(aes(x=AGEDAYS, y=HAZ, group=MWTKG_cat, colour=MWTKG_cat), data=d1) +
  geom_smooth()  + scale_color_brewer(palette = "Oranges", direction= -1) + ggtitle("HAZ trajectory stratified\nby maternal weight") + theme(panel.border = element_blank(), strip.background = element_blank())

p1 <- ggplot(aes(x=AGEDAYS, y=HAZ, group=MHTCM_cat, colour=MHTCM_cat), data=d2) +
  geom_smooth()  + scale_color_brewer(palette = "Oranges", direction= -1) + ggtitle("HAZ trajectory stratified\nby maternal height") + theme(panel.border = element_blank(), strip.background = element_blank())



d2$country_cohort <- paste0(d2$STUDYID,"\n", d2$COUNTRY.x)
p5<-ggplot(aes(x=AGEDAYS, y=HAZ, group=study_MHTCM_cat, colour=study_MHTCM_cat), data=d2) +
  geom_smooth() + facet_wrap(~country_cohort, scales="free") + scale_color_manual(palette = tableau10)  + 
  ggtitle("Cohort-specific HAZ trajectory stratified by maternal height") + 
  scale_color_discrete(guide = guide_legend(reverse=TRUE), 
                      name="Maternal\nheight\nquartile",
                      labels=c("Q1", "Q2", "Q3", "Q4"))
p5

d1$country_cohort <- paste0(d1$STUDYID,"\n", d1$COUNTRY.x)
p6<-ggplot(aes(x=AGEDAYS, y=WHZ, group=study_MWTKG_cat, colour=study_MWTKG_cat), data=d1) +
  geom_smooth() + facet_wrap(~country_cohort, scales="free")  + scale_color_manual(palette = tableau10)  + 
  ggtitle("Cohort-specific WHZ trajectory stratified by maternal weight") + theme(panel.border = element_blank(), strip.background = element_blank()) + 
  scale_color_discrete(guide = guide_legend(reverse=TRUE), 
                      name="Maternal\nweight\nquartile",
                      labels=c("Q1", "Q2", "Q3", "Q4"))
p6

p7<-ggplot(aes(x=AGEDAYS, y=WHZ, group=study_MHTCM_cat, colour=study_MHTCM_cat), data=d2) +
  geom_smooth() + facet_wrap(~country_cohort, scales="free")  + scale_color_manual(palette = tableau10)  + 
  ggtitle("Cohort-specific WHZ trajectory stratified by maternal weight") + theme(panel.border = element_blank(), strip.background = element_blank()) +
  scale_color_discrete(guide = guide_legend(reverse=TRUE), 
                      name="Maternal\nheight\nquartile",
                      labels=c("Q1", "Q2", "Q3", "Q4"))
p7




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


table(d1$country_cohort, d1$study_MWTKG_cat)


setwd("U:/results")
#library(mgcv)
pdf("Child growth trajectories stratified by maternal anthropometry.pdf", width=10, height=10)
#multiplot(p1,p3,p2,p4, cols=2)
p5
p6
p7
dev.off()