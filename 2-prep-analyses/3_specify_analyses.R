
#---------------------------------------------
#Adjustment sets
#---------------------------------------------
rm(list=ls())
setwd("U:/ucb-superlearner/Wasting rallies/")
load("adjustment_sets_list.Rdata")

#---------------------------------------------
#Adjustment specifying function
#---------------------------------------------


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


#---------------------------------------------
# Specify the binary analyses
#---------------------------------------------

Avars <- c( "sex",  "brthmon", "month", names(adjustment_sets))


prev <- specify_rf_analysis(A=Avars, Y=c("wasted","swasted"), file="wast_prev_rf.Rdata")

cuminc <- specify_rf_analysis(A=c( "sex",               "mage",          "mhtcm",         "mwtkg",        
                                   "mbmi",          "single",        "fage",          "fhtcm",       
                                   "nrooms",      "nchldlt5",    "nhh",              
                                   "hhwealth_quart", "brthmon", "parity",   "meducyrs", 
                                   "feducyrs", "hfoodsec"),
                              Y="ever_wasted", file="wast_cuminc_rf.Rdata")

cuminc_nobirth <- specify_rf_analysis(A=c( "gagebrth",      "birthwt",    
                                           "birthlen",       "vagbrth",       "hdlvry",    
                                           "enstunt", 
                                           "trth2o", "cleanck", "impfloor",  
                                           "impsan", "safeh20",
                                           "perdiar6", "perdiar24", 
                                           "predfeed3", "exclfeed3", "predfeed6", "exclfeed6", "predfeed36", "exclfeed36",
                                           "predexfd6", "earlybf", "month"),
                                      Y="ever_wasted", file="wast_cuminc_nobirth_rf.Rdata")


rec <- specify_rf_analysis(A=Avars, id="subjid", Y="wast_rec90d", file="wast_rec_rf.Rdata")
pers_wast <- specify_rf_analysis(A=Avars, Y="pers_wast", file="pers_wast_rf.Rdata")


#bind together datasets
analyses <- rbind(prev, cuminc, cuminc_nobirth, rec, pers_wast)
table(analyses$file)

#Save analysis specification
#setwd("C:/Users/andre/Documents/HBGDki/Results/")
save(analyses, file="wasting_adjusted_binary_analyses.rdata")


#Make unadjusted analysis set
analyses$W <- NULL
save(analyses, file="wasting_unadjusted_binary_analyses.rdata")