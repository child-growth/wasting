

rm(list=ls())

source("C:/Users/andre/Documents/HBGDki/Wasting/1-outcomes/0_wast_incfunctions.R")


load("C:/Users/andre/Dropbox/HBGDki replication/Mock data/testdata.Rdata")
# load("C:/Users/andre/Dropbox/HBGDki replication/Mock data/testdata2.Rdata")
# load("C:/Users/andre/Dropbox/HBGDki replication/Mock data/testdata3.Rdata")

# test <- WastIncCalc(d)
# 
# head(test)
# 
# #colnames(test) <- toupper(colnames(test))
# test2<-WastIncTable(test)
# 
# test2$means[test2$means$statistic=="Prevalence\nof\nwasting",]


#need to write function in a way that summarizesnstatistics like instunting

d$country <- "null"

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
d <- d %>% group_by(studyid, country) %>% do(WastIncCalc(.))

#Prevalence
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)

#Cumulative inc
d <- calc.ci.agecat(d)
ci.data <- summary.ci(d)

#Recovery cumulative inc
rec.data <- summary.ci(d, recovery = T)

#Incidence rate


#Duration






load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_prev.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_cuminc.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_incprop.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rec_interim.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/pool_vel.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rf_res.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rec24.RData")
# load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_laz.RData")
