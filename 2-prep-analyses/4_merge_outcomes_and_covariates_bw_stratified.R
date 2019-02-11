

rm(list=ls())
library(tidyverse)
library(reshape2)
library(caret)

setwd("U:/UCB-SuperLearner/Wasting rallies/")


load("wast_prev_rf.Rdata")
d1 <- d
load("wast_cuminc_rf.Rdata")
d2 <- d
load("wast_cuminc_nobirth_rf.Rdata")
d3 <- d
load("wast_rec_rf.Rdata")
d4 <- d
load("pers_wast_rf.Rdata")
d5 <- d
load("stuntwast_mort.Rdata")
d6 <- d
load("stuntwast_morbidity.Rdata")
d7 <- as.data.frame(d)
for(i in c("co_occurence","pers_wasted624", "ever_wasted06","ever_swasted06","pers_wasted06","ever_stunted06",
           "ever_wasted06_noBW","ever_underweight06", "ever_co06")){
  d7[,i] <- factor(d7[,i])
}
load("waz_vel_rf_outcomes.RData")
d8 <- vel_waz

d <- bind_rows(d1, d2)
d <- bind_rows(d, d3)
d <- bind_rows(d, d4)
d <- bind_rows(d, d5)
d <- bind_rows(d, d6)
d <- bind_rows(d, d7)
d <- bind_rows(d, d8)

d <- d %>% filter(!is.na(birthwt)) 

save(d, file="bw_strat_rf_data.Rdata")




specify_rf_analysis <- function(A=names(adjustment_sets), Y, file,  W=NULL, V= c("agecat","studyid","country"), id="id", adj_sets=adjustment_sets){
  
  analyses <- expand.grid(A=A,Y=Y, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  analyses$id <- id
  analyses$strata <- list(V)
  if(is.null(W)){analyses$W <- adj_sets[analyses$A]}else{
    analyses$W <- list(W)
  }
  names(analyses$W) <- NULL
  analyses$file <- file
  
  return(analyses)
}


Avars <- c( "sex",  "brthmon", "month", names(adjustment_sets))


bw_strat_analysis <- specify_rf_analysis(A=Avars, Y=c("wasted","swasted"), V= c("agecat","studyid","country", "birthwt"), file="bw_strat_rf_data.Rdata")


