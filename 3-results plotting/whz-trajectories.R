




rm(list = ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Wasting_inc_data.RData")
load("U:/Data/Wasting/Wasting_inc_noRec_data.RData")

library(kableExtra)
library(ROCR)


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


#Subset to monthly
d <- d %>% filter(measurefreq == "monthly")

d <- calc.monthly.agecat(d)
d <- d %>% filter(!is.na(agecat))

d <- d %>% arrange(studyid, subjid, agedays) %>%
  group_by(studyid, subjid, agecat) %>% 
  summarize(whz=mean(whz)) %>%
  mutate(measid=seq_along(subjid))
table(d$agecat)

d <- d %>% mutate(lagwhz=lag(whz), vel=whz-lagwhz, 
                  wastinc=1*(whz < (-2) & (lagwhz >= (-2) | measid==1)),
                  wastrec=1*(whz >= (-2) & lagwhz < (-2)))


summary(d$vel)
d$velcat <- cut(d$vel, breaks=c(-10,-2,-1,0,1,2,10))
d$velcat <- cut(d$vel, breaks=c(-10,-0.5,0.5,10))

table(d$velcat)

d$whz2 <- d$whz
d$whz2[d$wastinc!=1 & d$wastrec!=1] <- NA
d$inccat <- NA
d$inccat[d$wastinc==1] <- "inc"
d$inccat[d$wastrec==1] <- "rec"
d$inccat <- factor(d$inccat, c("inc","rec"))


df <- d[d$studyid=="ki1000108-CMC-V-BCS-2002",]


cols <- c(cred,corange,"gray70","gray70",cteal,cblue)
cols <- c(cteal, "gray70", corange)

# Individual trajectories by age
p <- ggplot(df, aes(x=agecat, y=whz, group=subjid, color=velcat) ) +
  #facet_wrap(~studyid)+
  geom_line(alpha=0.3) +
  geom_hline(aes(yintercept=-2),linetype="dashed") +
  scale_color_manual(values=cols,
                     guide=guide_legend(title="WHZ change:", 
                                        override.aes = list(alpha=1),
                                        ncol=2,nrow=2
                     ))+
  xlab("Age in months") +
  ylab("WHZ") +
  #coord_cartesian(ylim=c(0, 4.5),xlim=c(0,11))+
  #scale_y_continuous(breaks=0:4,labels=log10labs) +
  #scale_x_continuous(breaks=0:11) +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    axis.ticks.y=element_line(color="gray40")
  )

p



p <- ggplot(df, aes(x=measid, y=whz, group=subjid, color=velcat) ) +
  geom_line(alpha=0.3) +
  geom_point(aes(y=whz2, fill=inccat),alpha=0.3) +
  scale_fill_manual(values=c(cred,cblue))+
  geom_hline(aes(yintercept=-2),linetype="dashed") +
  scale_color_manual(values=cols,
                     guide=guide_legend(title="WHZ change:", 
                                        override.aes = list(alpha=1),
                                        ncol=2,nrow=2
                     ))+
  xlab("Measurement #") +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    axis.ticks.y=element_line(color="gray40")
  )

p





p <- ggplot(d, aes(x=measid, y=whz, group=subjid, color=velcat) ) +
  facet_wrap(~studyid) +
  geom_line(alpha=0.3) +
  geom_point(aes(y=whz2, color=inccat),alpha=0.3) +
  scale_fill_manual(values=c(corange,cteal))
  geom_hline(aes(yintercept=-2),linetype="dashed") +
  scale_color_manual(values=cols,
                     guide=guide_legend(title="WHZ change:", 
                                        override.aes = list(alpha=1),
                                        ncol=2,nrow=2
                     ))+
  xlab("Measurement #") +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    axis.ticks.y=element_line(color="gray40")
  )

p