

library(tidyverse)

d <- readRDS("U:/data/jvt3.rds")

d$AGEDTH <- as.numeric(d$AGEDTH)
table(!is.na(d$AGEDTH)) #2025 child deaths
summary(d$AGEDTH)

#Check if age of death is recorded multiple times for each child who died
d %>% group_by(SUBJID) %>% filter(!is.na(AGEDTH)) %>% summarize(N=n())

#Looks like it is recorded once per subject


#Check that the age of death is later than the oldest measurement on each child
d <- d %>% group_by(SUBJID) %>% mutate(maxAge=max(AGEDAYS), deathErrorFlag= AGEDTH < AGEDAYS & !is.na(AGEDTH))

table(d$deathErrorFlag[!is.na(d$AGEDTH)])

#Uh oh -5 children have measurements after there recorded age of death
d$AGEDAYS[d$deathErrorFlag]
d$AGEDTH[d$deathErrorFlag]

d[d$deathErrorFlag,]

#make indicator for any death
d <- d %>% group_by(SUBJID) %>% mutate(died= any(!is.na(AGEDTH))) %>%
               arrange(SUBJID, AGEDAYS)

#subset data set to children who died
df_death <- d %>% filter(died) %>% subset(., select=c(SUBJID, AGEDAYS, WHZ, AGEDTH, died))

head(df_death, 20)


#Check if anthropometry is measured when the age of death is recorded
df_death$WHZ[!is.na(df_death$AGEDTH)]

#Plot WHZ trajectories before death 
ggplot(df_death) + geom_line(aes(x=AGEDAYS, y=WHZ, group=SUBJID), alpha=0.2) 


#Smoothed WHZ trajectories before death and compare to those who didn't die
ggplot(d) + geom_smooth(aes(x=AGEDAYS, y=WHZ)) +
  facet_wrap(~died)



