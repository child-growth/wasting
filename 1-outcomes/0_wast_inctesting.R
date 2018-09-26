

rm(list=ls())

source("C:/Users/andre/Documents/HBGDki/Wasting/1-outcomes/0_wast_incfunctions.R")


load("C:/Users/andre/Dropbox/HBGDki replication/Mock data/testdata.Rdata")
 #load("C:/Users/andre/Dropbox/HBGDki replication/Mock data/testdata2.Rdata")
 #load("C:/Users/andre/Dropbox/HBGDki replication/Mock data/testdata3.Rdata")
#d <- d %>% filter(subjid==3)
d$country <- "null"
d$studyid <- "a"
washout=60
dropBornWasted=F


# test <- WastIncCalc(d)
# 
# head(test)
# 
# #colnames(test) <- toupper(colnames(test))
# test2<-WastIncTable(test)
# 
# test2$means[test2$means$statistic=="Prevalence\nof\nwasting",]

df <- WastIncCalc(d)
sum(df$wast_inc)
sum(df$wast_rec)
sum(df$pt_wast)
sum(df$pt_wast_rec)

sum(df$wast_inc[df$agedays <= 6*30.4167])
sum(df$wast_rec[df$agedays <= 6*30.4167])


df2 <- WastIncCalc(d, dropBornWasted=T)
sum(df2$wast_inc, na.rm=T)
sum(df2$wast_rec, na.rm=T)
sum(df2$pt_wast, na.rm=T)
sum(df2$pt_wast_rec, na.rm=T)

sum(df2$wast_inc[df2$agedays <= 6*30.4167], na.rm=T)
sum(df2$wast_rec[df2$agedays <= 6*30.4167], na.rm=T)

#need to write function in a way that summarizesnstatistics like instunting



#Simulate multiple cohorts
d1 <- d
d2 <- d
d3 <- d
d4 <- d

d1$studyid <-"a"
d2$studyid <-"b"
d3$studyid <-"c"
d4$studyid <-"d"

d <- rbind(d1,d2,d3,d4)

#Calculate incidence 
d_nobirth <- d %>% group_by(studyid, country) %>% do(WastIncCalc(., dropBornWasted=T))
d <- d %>% group_by(studyid, country) %>% do(WastIncCalc(.))


#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)

#mean whz
whz.data <- summary.whz(d)

#Cumulative inc
d <- calc.ci.agecat(d)
ci.data <- summary.ci(d)

#Cumulative inc, no birth
d_nobirth <- calc.ci.agecat(d_nobirth)
ci.data.nobirth <- summary.ci(d_nobirth)

#Recovery cumulative inc
rec.data <- summary.ci(d, recovery = T)

#Incidence rate
ir.data <- summary.ir(d)

#Incidence rate - no birth wasting
ir.data.nobirth <- summary.ir(d_nobirth)

#Recovery incidence rate
rec.ir.data <- summary.ir(d, recovery = T)

#Duration
dur.data <- summary.dur(d) 


save(prev.data, whz.data, ci.data, ci.data.nobirth, rec.data, ir.data, ir.data.nobirth, rec.ir.data, dur.data, file="C:/Users/andre/Documents/HBGDki/Results/Wasting_descriptive_epi_results.Rdata")


