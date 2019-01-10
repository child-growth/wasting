
#---------------------------------------------
#Adjustment sets
#---------------------------------------------
rm(list=ls())
setwd("U:/UCB-SuperLearner/Wasting rallies")
load("mortality_adjustment_sets_list.Rdata")

#---------------------------------------------
#Adjustment specifying function
#---------------------------------------------


specify_rf_analysis <- function(A, Y, file,  W=NULL, V= c("studyid","country"), id="id", adj_sets=adjustment_sets_mortality){
  
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

Avars <- c("ever_wasted06",
           "ever_swasted06",
           "pers_wasted06",
           "ever_stunted06",
           "ever_sstunted06",
           "ever_wasted024",
           "ever_swasted024",
           "pers_wasted024",
           "ever_stunted024",
           "ever_sstunted024",
           "ever_wasted06_noBW",
           "ever_swasted06_noBW",
           "ever_wasted024_noBW",
           "ever_swasted024_noBW",
           "ever_underweight06",
           "ever_sunderweight06",
           "ever_underweight024",
           "ever_sunderweight024",
           "ever_co06",
           "ever_co024")






mortality <- specify_rf_analysis(A=Avars, Y=c("dead"), file="stuntwast_mort.Rdata")



Avars_morbidity <- c("ever_wasted06",
                     "ever_swasted06",
                     "pers_wasted06",
                     "ever_stunted06",
                     "ever_wasted06_noBW",
                     "ever_swasted06_noBW",
                     "ever_underweight06",
                     "ever_co06")

morbidity <- specify_rf_analysis(A=Avars_morbidity,
                                 Y=c("co_occurence", "pers_wasted624"), file="stuntwast_morbidity.Rdata")

#bind together datasets
analyses_mortality <- rbind(mortality, morbidity)
table(analyses_mortality$file)

#Save analysis specification
save(analyses_mortality, file="mortality_adjusted_analyses.rdata")


#Make unadjusted analysis set
analyses$W <- NULL
save(analyses_mortality, file="mortality_unadjusted_analyses.rdata")



