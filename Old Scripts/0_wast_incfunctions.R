


#----------------------------------------------
# markt the start of wasted of not wasted episodes 
#----------------------------------------------

mark_episodes <- function(d){
  
  d$wasting_episode = ifelse(d$AGEDAYS==min(d$AGEDAYS) & d$wast==0, "Not Wasted", d$wasting_episode)
  d$wasting_episode = ifelse(d$AGEDAYS==min(d$AGEDAYS) & d$wast==1, "Wasted", d$wasting_episode)
  d$born_wast_inc= 0
  d$wasting_episode = na.locf(d$wasting_episode, fromLast=F)#Last observation carried forward
  
  return(d)
}

mark_sevepisodes <- function(d){
  
  d$born_sevwast_inc= ifelse(d$AGEDAYS==min(d$AGEDAYS) & d$sevwast==1,1,0)
  d$sevwasting_episode = ifelse(d$AGEDAYS==min(d$AGEDAYS) & d$sevwast==0, "Not Severe Wasted", d$sevwasting_episode)
  d$sevwasting_episode = ifelse(d$AGEDAYS==min(d$AGEDAYS) & d$sevwast==1, "Born Severe Wasted", d$sevwasting_episode)
  d$sevwasting_episode = na.locf(d$sevwasting_episode, fromLast=F)
  d$sevwasting_episode_lag=lag(d$sevwasting_episode)
  
  return(d)
}

#----------------------------------------------
#create functions for rolling sum windows 
#----------------------------------------------

roll_sum_fun <-  function(v, len){ sapply(1:(length(v)),function(x){sum(v[(x+1):(x+len+1)], na.rm=T)})}
lag_sum_fun <-  function(v, len){ sapply(1:(length(v)),function(x){ifelse((x-1-len)<0,
                                                                          sum(v[0:(x-1)], na.rm=T),
                                                                          sum(v[(x-1-len):(x-1)], na.rm=T))})}
#function to always round 0.5 up
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


#----------------------------------------------
# Mean and 95% CI function 
#----------------------------------------------
mean95CI <- function(Y, id=rep(1:length(Y)), persontime=NULL, proportion=F, percent=F, count=F){
  
  if(proportion==F){
    if(count==T){
      IR.CI <- pois.exact(Y, pt = persontime, conf.level = 0.95)[3:5] 
      mean_ci <- data.frame(N=Y, Mean=IR.CI[1], SD=NA, Robust.SE=NA ,  Lower.95.CI=IR.CI[2] ,  Upper.95.CI=IR.CI[3] )
      colnames(mean_ci) <- c("N","Mean","SD","Robust SE", "Lower 95%CI", "Upper 95%CI") 
    }else{
      if(!is.na(mean(Y[complete.cases(Y)]))){
        mudat <- data.frame(id = id, Y = Y)
        mudat <- mudat[complete.cases(mudat), ]
        n.sub <- dim(mudat)[1]
        fit <- glm(Y ~ 1, family = gaussian, data = mudat)
        vcovCL <- sandwichSE(mudat, fm = fit, cluster = mudat$id)
        rfit <- coeftest(fit, vcovCL)
        lb <- rfit[1, 1] - 1.96 * rfit[1, 2]
        ub <- rfit[1, 1] + 1.96 * rfit[1, 2]
        mean_ci <- matrix(c(n.sub, rfit[1, 1], sd(mudat$Y), rfit[1, 
                                                                 2], lb, ub), nrow = 1, ncol = 6)
        colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", 
                               "Upper 95%CI")
        
      }else{
        mean_ci <- data.frame(N=NA, Mean=NA, SD=NA, `Robust SE`=NA, `Lower 95%CI`=NA, `Upper 95%CI`=NA)
        colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", "Upper 95%CI")  
      }
    }
  }else{
    
    require(binom)
    # Find the number of obs
    n = length(Y[!is.na(Y)])
    if(percent==T){
      CR.res<-binom.confint(sum(Y/100, na.rm = T), n, method="exact")
    }else{
      CR.res<-binom.confint(sum(Y, na.rm = T), n, method="exact")
    }
    mean_ci <- data.frame(N=n, Mean=CR.res[4], SD=NA, `Robust SE`=NA, `Lower 95%CI`=CR.res[5], `Upper 95%CI`=CR.res[6])
    colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", "Upper 95%CI")  
  }
  return(mean_ci)
}



#Function to calculate the robust SEs
sandwichSE <- function (dat, fm, cluster) 
{
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  if (is.factor(cluster)) {
    cluster <- droplevels(cluster)
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}


#----------------------------------------------
#create function to calc unstrat and age stat incidence
#----------------------------------------------
WastIncCalc<-function(d, washout=60, dropBornWasted=F){
  require(tidyverse)
  require(zoo)  
  
  colnames(d) <- toupper(colnames(d))
  
  #Filter out extreme or missing whz values
  d <- d %>%  ungroup() %>% filter(!is.na(WHZ)) %>%
    filter(WHZ > (-5) & WHZ < 5)
  
  #Remove duplicate ages
  ndropped <- nrow(d[duplicated(cbind(d$SUBJID, d$AGEDAYS)), ])
  d <- d[!duplicated(cbind(d$SUBJID, d$AGEDAYS)), ]
  if(ndropped>0) cat("\n-----------------------------------\n",ndropped," observations dropped due to duplicate ages\n_----------------------------------\n")
  
  
  #Create visit variable
  #(This replaces the visit variables, but some studies have missing 
  #visit data)
  d <- d %>% group_by(SUBJID) %>% mutate(VISITNUM = rank(AGEDAYS))
  
  
  #Extract required columns and save others to merge back in later
  othercolumns <- d %>% subset(., select= -c(WHZ, VISITNUM)) 
  d <- d %>% subset(., select= c(SUBJID, WHZ, AGEDAYS, VISITNUM)) 
  
  
  #generate wasting and severe wasting indicators
  d$wast= ifelse(d$WHZ < (-2),1,0)
  d$sevwast= ifelse(d$WHZ < (-3),1,0)
  
  # #Generate variables for length of period in days between prior observation and current observations
  # #and the next observations and current observations. Also generate variables for if child changed from
  # #not wasted to wasted (or severe wasted) between the last observation and the current observation.
  d <- d %>%
    arrange(SUBJID, AGEDAYS) %>%
    group_by(SUBJID) %>%
    mutate(
      agelag=lag(AGEDAYS),
      wastlag=lag(wast),
      sevwastlag=lag(sevwast),
      midpoint_age = AGEDAYS - (AGEDAYS - agelag)/2,
      wastchange = wast - lag(wast),
      sevwastchange = sevwast - lag(sevwast),
      delta_age = AGEDAYS-agelag,
      firstmeasure = AGEDAYS==min(AGEDAYS)
    ) %>%
    as.data.frame()
  
  d$agelag[d$firstmeasure] <- 0
  d$wastlag[d$firstmeasure] <- 0
  d$sevwastlag[d$firstmeasure] <- 0
  d$wastchange[d$firstmeasure] <- d$wast[d$firstmeasure]
  d$sevwastchange[d$firstmeasure] <- d$sevwast[d$firstmeasure]
  d$midpoint_age[is.na(d$midpoint_age)] <- d$AGEDAYS[is.na(d$midpoint_age)]/2
  d$delta_age[d$firstmeasure] <-d$AGEDAYS[d$firstmeasure]
  
  #Length of each observation period
  d <- d %>% group_by(SUBJID) %>% 
    mutate(
      next_midpoint = lead(midpoint_age)
    )
  
  #Assume 30 day period after final measurement (so midpoint will be 15 days after final measure)
  d$next_midpoint[is.na(d$next_midpoint)] <- d$AGEDAYS[is.na(d$next_midpoint)] + 15
  d$period_length <- (d$next_midpoint - d$midpoint_age)
  
  
  
  N <- nrow(d)
  d$washout_period_lead <- d$washout_period_lag <- rep(T, N)
  d$future_sevwast <- d$future_wast <-  d$past_sevwast <- d$past_wast <- rep(0, N)
  
  for(i in 1:washout){
    
    d <- d %>% group_by(SUBJID) %>%
      mutate(
        wast_lag_i  = lag(wast, i),
        sevwast_lag_i  = lag(sevwast, i),
        days_lag_i = abs(lag(midpoint_age, i) - midpoint_age),
        wast_lead_i  = lead(wast, i),
        sevwast_lead_i  = lead(sevwast, i),
        days_lead_i = abs(lead(midpoint_age, i) - midpoint_age)
      )
    
    d$washout_period_lag[d$days_lag_i > washout] <- F
    d$washout_period_lead[d$days_lead_i > washout] <- F
    
    d$past_wast[d$wast_lag_i==1 & d$washout_period_lag==T] <- 1
    d$past_sevwast[d$sevwast_lag_i==1 & d$washout_period_lag==T] <- 1              
    d$future_wast[d$wast_lead_i==1 & d$washout_period_lead==T] <- 1
    d$future_sevwast[d$sevwast_lead_i==1 & d$washout_period_lead==T] <- 1
    
    #Stop for loop if all current leading and lagging observations are beyond washout period
    if(min(d$days_lead_i, na.rm=T) & min(d$days_lag_i, na.rm=T) > washout) break
  }
  
  d <- d %>% 
    subset(., select= -c(washout_period_lag, washout_period_lead, 
                         wast_lag_i, sevwast_lag_i, days_lag_i, 
                         wast_lead_i, sevwast_lead_i, days_lead_i)) %>% 
    ungroup() %>% as.data.frame()
  
  head(d,30)
  
  #---------------------------------------------------------
  #Calculate wasting and wasting recovery incidence and risk
  #---------------------------------------------------------
  d$wast_rec_inc <- d$sevwast_rec_inc <- d$wast_inc <- d$sevwast_inc <- rep(0, N)
  d$sevwast_rec_risk <- d$wast_rec_risk <- d$sevwast_risk <- d$wast_risk <-  rep(0, N)
  
  
  d$wast_inc[d$wastchange==1 & d$past_wast==0] <- 1 #Wasting incidence if at risk of wasting and change in status between prior and current observation
  d$wast_rec_inc[d$wastchange== -1 & d$future_wast==0] <- 1 #Recovery from wasting if status change to not wasted and no new wasting in the future washout period
  
  d$wast_inc[d$wastchange!=1 | d$past_wast==1] <- 0 
  d$wast_rec_inc[d$wastchange!= -1 | d$future_wast==1] <- 0  
  
  #Remove incidences of wasting if there has not been recovery from prior wasting episode
  #Is there a cleaner way of preventing recording the incidences earlier?
  table(d$wast_inc)
  
  d <- d %>% group_by(SUBJID) %>% 
    mutate(sum_wast_inc=cumsum(wast_inc),
           sum_wast_rec=cumsum(wast_rec_inc))
  for(i in 1:nrow(d)){
    if(d$wast_inc[i]==1 & (d$sum_wast_inc[i]-d$sum_wast_rec[i] > 1)){
      d$wast_inc[i] <- 0 
      d <- d %>% group_by(SUBJID) %>% 
        mutate(sum_wast_inc=cumsum(wast_inc),
               sum_wast_rec=cumsum(wast_rec_inc))        
    }
    if(d$wast_rec_inc[i]==1 & (d$sum_wast_inc[i]-d$sum_wast_rec[i] < 0)){
      d$wast_rec_inc[i] <- 0 
      d <- d %>% group_by(SUBJID) %>% 
        mutate(sum_wast_inc=cumsum(wast_inc),
               sum_wast_rec=cumsum(wast_rec_inc))  
    }
  }
  table(d$wast_inc)
  
  
  d <- subset(d, select = -c(sum_wast_inc,sum_wast_rec))
  #Make sure there isn't double recovery
  #Indicate length of incident episodes
  d$wasting_episode <- rep(NA, nrow(d))
  d$wasting_episode[d$wast_inc==1] <- "Wasted"
  d$wasting_episode[d$wast_rec_inc==1] <- "Not Wasted"
  
  
  d <- d %>% group_by(SUBJID) %>%  arrange(AGEDAYS)
  
  #Have to mark first observations as wasted or not wasted if dropBornWasted=F
  if(dropBornWasted==F){
    
    d <- d %>% group_by(SUBJID) %>%
      do(mark_episodes(.))
    
    # d <- d %>% group_by(SUBJID) %>% 
    #   mutate(wasting_episode = ifelse(AGEDAYS==min(AGEDAYS) & wast==0, "Not Wasted", wasting_episode),
    #          wasting_episode = ifelse(AGEDAYS==min(AGEDAYS) & wast==1, "Wasted", wasting_episode),
    #          born_wast_inc= 0,
    #          wasting_episode = na.locf(wasting_episode, fromLast=F)) %>% #Last observation carried forward 
    #   ungroup()
  }else{
    d <- d %>% group_by(SUBJID) %>% 
      mutate(wasting_episode = ifelse(AGEDAYS==min(AGEDAYS) & wast==0, "Not Wasted", wasting_episode),
             wasting_episode = ifelse(AGEDAYS==min(AGEDAYS) & wast==1, "Born Wasted", wasting_episode),
             wast_inc = ifelse(wasting_episode=="Born Wasted",0, wast_inc),
             born_wast_inc= ifelse(AGEDAYS==min(AGEDAYS) & wasting_episode=="Born Wasted",1,0),
             wasting_episode = na.locf(wasting_episode, fromLast=F)) %>% #Last observation carried forward 
      ungroup()      
  }
  
  #Indicate risk of wasting or recovery 
  d$wast_risk[(d$wasting_episode=="Not Wasted" & d$past_wast==0) | d$wast_inc==1] <- 1 
  d$wast_rec_risk[(d$wasting_episode!="Not Wasted" & d$wast_inc!=1) | d$wast_rec_inc==1] <- 1 
  
  
  
  #Calculate duration of wasting episodes
  d <- d %>%  group_by(SUBJID) %>%
    mutate(episode_ID = cumsum(born_wast_inc+wast_inc+wast_rec_inc) + 1) %>% #Create unique episode ID
    ungroup() %>% group_by(SUBJID, episode_ID) %>%
    mutate(incident_age = min(midpoint_age),
           maxage=max(AGEDAYS))
  
  #Calculate duration for each episode
  #Note: this code implicitly censors the final epsiode as the lead(incident_age) will be NA
  d_episode <- d %>% 
    subset(., select=c(SUBJID,
                       episode_ID, incident_age)) %>%
    group_by(SUBJID, episode_ID) %>%
    slice(1) %>% ungroup() %>% group_by(SUBJID) %>%
    mutate(duration=lead(incident_age)-incident_age) %>%
    subset(., select= -incident_age)
  
  d <- left_join(d, d_episode, by=c("SUBJID","episode_ID"))
  
  
  
  #Variable for duration of only wasting episodes
  d$wasting_duration <- NA
  d$wasting_duration[d$wasting_episode=="Wasted"] <- d$duration[d$wasting_episode=="Wasted"]
  
  #View duration calculations
  # df <- d %>% subset(., select=c(SUBJID, AGEDAYS, WHZ, wast_rec_inc, wasting_episode, episode_ID, duration))
  # View(df)
  
  #---------------------------------------------------------
  #Calculate severe wasting and severe wasting recovery incidence and risk
  #---------------------------------------------------------    
  
  #Mark severe wasting changes
  d$sevwast_falter <- NA
  d$sevwast_falter[d$sevwastchange==1 & d$past_sevwast==0] <- 1
  d$sevwast_falter[d$sevwastchange!=1 | d$past_sevwast==1] <- 0
  
  d$born_sevwast_inc<-0
  d$sevwasting_episode <- rep(NA, N)
  d$sevwasting_episode[d$sevwast_falter==1] <- "Severe Wasted"
  d$sevwasting_episode[d$wast_rec_inc==1] <- "Not Severe Wasted"
  
  
  #Have to mark first observations as wasted or not wasted if dropBornWasted=F
  if(dropBornWasted==T){
    
    d <- d %>% group_by(SUBJID) %>%
      do(mark_sevepisodes(.))
    
  }else{
    d <- d %>% group_by(SUBJID) %>% 
      mutate(
        sevwasting_episode = ifelse
        (AGEDAYS==min(AGEDAYS) & sevwast==0, "Not Severe Wasted", sevwasting_episode),
        sevwasting_episode = ifelse(AGEDAYS==min(AGEDAYS) & sevwast==1, "Severe Wasted", sevwasting_episode),
        sevwasting_episode = na.locf(sevwasting_episode, fromLast=F),
        sevwasting_episode_lag=lag(sevwasting_episode)) %>% #Last observation carried forward 
      ungroup()     
  }
  
  #Indicate incidence of severe wasting and recovery
  d$sevwasting_episode_lag[is.na(d$sevwasting_episode_lag)]<-"Not Severe Wasted"
  d$sevwast_inc <- d$sevwast_rec_inc <- 0
  d$sevwast_inc[d$sevwasting_episode=="Severe Wasted" & (d$sevwasting_episode_lag=="Not Severe Wasted")] <- 1
  d$sevwast_rec_inc[d$sevwasting_episode=="Not Severe Wasted" & (d$sevwasting_episode_lag!="Not Severe Wasted")] <- 1
  
  
  #Create unique severe wasting episode IDs
  d <- d %>%  group_by(SUBJID) %>%
    mutate(sev_episode_ID = cumsum(sevwast_inc+born_sevwast_inc+sevwast_rec_inc) + 1) %>% #Create unique episode ID
    ungroup() 
  
  #Mark risk of severe wasting and severe wasting recovery 
  d$sevwast_risk[(d$sevwasting_episode!="Severe Wasted" & d$sevwasting_episode!="Born Severe Wasted" & d$past_sevwast==0) | d$sevwast_inc==1] <- 1 
  d$sevwast_rec_risk[((d$sevwasting_episode=="Severe Wasted" | d$sevwasting_episode=="Born Severe Wasted") & d$sevwast_inc!=1) | d$sevwast_rec_inc==1] <- 1 
  
  
  #Calculate duration of severe wasting episodes
  d <- d  %>% group_by(SUBJID, sev_episode_ID) %>%
    mutate(sev_incident_age = min(midpoint_age),
           sev_maxage=max(AGEDAYS))
  
  
  
  d_sev_episode <- d %>% 
    subset(., select=c(SUBJID, sev_episode_ID, sev_incident_age)) %>%
    group_by(SUBJID, sev_episode_ID) %>%
    slice(1) %>% ungroup() %>% group_by(SUBJID) %>%
    mutate(sevduration=lead(sev_incident_age)-sev_incident_age) %>%
    subset(., select= -sev_incident_age)
  
  d <- left_join(d, d_sev_episode, by=c("SUBJID","sev_episode_ID"))
  
  #Set duration of any censored episode to NA
  d <- d %>% group_by(SUBJID) %>%
    mutate(sevduration = ifelse(sev_maxage==max(sev_maxage), NA, sevduration)) %>% 
    ungroup()
  
  
  #Variable for duration of only severe wasting episodes
  d$sevwasting_duration <- NA
  if(dropBornWasted==F){
    d$sevwasting_duration[d$sevwasting_episode=="Severe Wasted"] <- d$sevduration[d$sevwasting_episode=="Severe Wasted"]
  }else{
    d$sevwasting_duration[d$sevwasting_episode=="Severe Wasted" | d$sevwasting_episode=="Born Severe Wasted"] <- d$sevduration[d$sevwasting_episode=="Severe Wasted" | d$sevwasting_episode=="Born Severe Wasted"]
  }
  
  
  #Calculate 30,60, 90 day recovery and faltering into severe wasting
  d$period_30d <- d$period_60d <- d$period_90d <- T
  d$wast_rec30d <- d$wast_rec60d <- d$wast_rec90d <- NA
  d$wast_rec30d[d$wast_inc==1]  <- d$wast_rec60d[d$wast_inc==1] <- d$wast_rec90d[d$wast_inc==1] <- 0
  d$sevwast_inc30d <- d$sevwast_inc60d <- d$sevwast_inc90d <- NA
  d$sevwast_inc30d[d$wast_inc==1] <- d$sevwast_inc60d[d$wast_inc==1] <- d$sevwast_inc90d[d$wast_inc==1] <- 0
  for(i in 1:90){
    d <- d %>% group_by(SUBJID) %>%
      mutate(
        rec_inc_lead_i = lead(wast_rec_inc, i),
        sev_inc_lead_i = lead(sevwast_inc, i),
        days_lead_i = abs(lead(midpoint_age, i) - midpoint_age)
      )
    
    d$period_30d[d$days_lead_i > 30] <- F
    d$period_60d[d$days_lead_i > 60] <- F
    d$period_90d[d$days_lead_i > 90] <- F
    
    d$wast_rec30d[d$wast_inc==1 & d$period_30d & d$rec_inc_lead_i==1] <- 1
    d$wast_rec60d[d$wast_inc==1 & d$period_60d & d$rec_inc_lead_i==1] <- 1
    d$wast_rec90d[d$wast_inc==1 & d$period_90d & d$rec_inc_lead_i==1] <- 1
    
    d$sevwast_inc30d[d$wast_inc==1 & d$period_30d & d$sev_inc_lead_i==1] <- 1
    d$sevwast_inc60d[d$wast_inc==1 & d$period_60d & d$sev_inc_lead_i==1] <- 1
    d$sevwast_inc90d[d$wast_inc==1 & d$period_90d & d$sev_inc_lead_i==1] <- 1
    
    d$wast_rec30d[d$wast_inc==1 & d$period_30d & d$rec_inc_lead_i==0 & d$wast_rec30d!=1] <- 0
    d$wast_rec60d[d$wast_inc==1 & d$period_60d & d$rec_inc_lead_i==0 & d$wast_rec60d!=1] <- 0
    d$wast_rec90d[d$wast_inc==1 & d$period_90d & d$rec_inc_lead_i==0 & d$wast_rec90d!=1] <- 0
    
    d$sevwast_inc30d[d$wast_inc==1 & d$period_30d & d$sev_inc_lead_i==0 & d$sevwast_inc30d!=1] <- 0
    d$sevwast_inc60d[d$wast_inc==1 & d$period_60d & d$sev_inc_lead_i==0 & d$sevwast_inc60d!=1] <- 0
    d$sevwast_inc90d[d$wast_inc==1 & d$period_90d & d$sev_inc_lead_i==0 & d$sevwast_inc90d!=1] <- 0
    
    #Stop for loop if all current leading observations are beyond 90 days
    if(min(d$days_lead_i, na.rm=T) > 90) break
  }
  
  
  d$incident_time_into_period <- d$delta_age/2
  
  #calculate person time for each incidence outcome
  d$pt_wast <- d$delta_age * d$wast_risk - d$incident_time_into_period * d$wast_inc
  d$pt_sevwast <- d$delta_age * d$sevwast_risk - d$incident_time_into_period * d$sevwast_inc
  d$pt_wast_rec <- d$delta_age * d$wast_rec_risk - d$incident_time_into_period * d$wast_rec_inc
  d$pt_sevwast_rec <- d$delta_age * d$sevwast_rec_risk - d$incident_time_into_period * d$sevwast_rec_inc
  
  #Drop intermediate variables
  d <- subset(d, select = -c(agelag, wastlag, sevwastlag, midpoint_age, wastchange, sevwastchange, past_wast, past_sevwast,
                             future_wast, future_sevwast,  sevwast_falter, sevwasting_episode_lag, sev_incident_age, sev_maxage,
                             sevduration, rec_inc_lead_i, sev_inc_lead_i, days_lead_i, period_30d,period_60d,period_90d, next_midpoint,
                             incident_time_into_period, delta_age
  )) %>%
    ungroup() %>% as.data.frame()
  if(dropBornWasted==T){
    d <- subset(d, select = -c(born_wast_inc, born_sevwast_inc)) %>%
      ungroup() %>% as.data.frame()      
  }
  
  #merge back in other columns
  d <- merge(d, othercolumns, by=c("SUBJID", "AGEDAYS"))
  
  #rename columns to match other functions
  d <- d %>% rename(wast_rec = wast_rec_inc,
                    sevwast_rec = sevwast_rec_inc) 
  
  colnames(d) <- tolower(colnames(d))
  
  return(d)
}  





#----------------------------------------------
#Function to calculate summary tables
#----------------------------------------------

WastIncTable<-function(d, strat=T, agecats=c(6*30.4167, 12*30.4167, 18*30.4167, 24*30.4167), agecat_rownames=c("0-6 months","6-12 months", "12-18 months", "18-24 months")){
  
  
  if(strat==T){
    d$agecat <- as.factor(findInterval(d$AGEDAYS, agecats, rightmost.closed=F))
  }  
  
  summary<-WastIncSummary(d, strat=strat)
  tab<-summary[[1]]
  means<-data.frame(strata=rep("Overall",nrow(summary[[2]])), summary[[2]])
  
  if(strat==T){
    
    strattab<-stratmeans<-NULL
    for(i in 1:(length(agecats))){
      temp<-WastIncSummary(d[d$agecat==(i-1),], strat=F)
      strattab<-rbind(strattab, temp[[1]])
      stratmeans<-rbind(stratmeans, data.frame(strata=rep(agecat_rownames[i],nrow(temp[[2]])), temp[[2]]))
    }
    stratmeans<-stratmeans[!is.na(stratmeans$Mean),]
    
    tab<-rbind(tab, strattab)
    means<-rbind(means, stratmeans)
    rownames(tab)<-c("Overall",agecat_rownames)
    
  }
  
  tf<-tab_format(tab)
  
  
  
  return(list(tab1=tf[[1]], tab2=tf[[2]], tab3=tf[[3]], tab=tf[[4]], means=means))
  
  
}



#----------------------------------------------
#Function to calculate summary statistics
#----------------------------------------------

WastIncSummary<-function(d, strat=F){
  require(epitools)
  
  d <- d %>% arrange(SUBJID, AGEDAYS)
  
  #Average number of measurements per child
  child_nmeas <- d %>% group_by(SUBJID) %>%
    summarize(num_measurements=n()) %>% ungroup() %>% 
    summarize(num_children=n(),
              num_measurements=mean(num_measurements))
  
  
  #Overall sum and means
  Incidence_df <- d %>% 
    summarize(      
      prev_wast= mean(WHZ < -2, na.rm=T) * 100,
      prev_sevwast= mean(WHZ < -3, na.rm=T) * 100,
      anywast= ifelse(sum(wast) > 0 ,1,0) * 100,
      anysevwast= ifelse(sum(sevwast) > 0 ,1,0) * 100,
      persontime=sum(pt_wast, na.rm=T),
      sev_persontime=sum(pt_sevwast, na.rm=T),
      recovery_persontime=sum(pt_wast_rec, na.rm=T),
      sevrecovery_persontime=sum(pt_sevwast_rec, na.rm=T),
      wast_ep=sum(wast_inc, na.rm=T),
      sevwast_ep=sum(sevwast_inc, na.rm=T),
      wast_rec_ep=sum(wast_rec, na.rm=T),
      sevwast_rec_ep=sum(sevwast_rec, na.rm=T),
      recoveries30d=sum(wast_rec30d==1, na.rm=T),
      recoveries60d=sum(wast_rec60d==1, na.rm=T),
      recoveries90d=sum(wast_rec90d==1, na.rm=T),
      sevwast30d=sum(sevwast_inc30d==1, na.rm=T),
      sevwast60d=sum(sevwast_inc60d==1, na.rm=T),
      sevwast90d=sum(sevwast_inc90d==1, na.rm=T),
      no_recoveries30d=sum(wast_rec30d==0, na.rm=T),
      no_recoveries60d=sum(wast_rec60d==0, na.rm=T),
      no_recoveries90d=sum(wast_rec90d==0, na.rm=T),
      no_sevwast30d=sum(sevwast_inc30d==0, na.rm=T),
      no_sevwast60d=sum(sevwast_inc60d==0, na.rm=T),
      no_sevwast90d=sum(sevwast_inc90d==0, na.rm=T)
    ) %>% 
    mutate(  wastIR=wast_ep/persontime * 1000,
             sevwastIR=sevwast_ep/sev_persontime * 1000,
             wastrecIR=wast_rec_ep/recovery_persontime * 1000,
             sevwastrecIR=sevwast_rec_ep/sevrecovery_persontime * 1000,
             perc_wastrec_30d= sum(recoveries30d)/(sum(recoveries30d)+sum(no_recoveries30d))*100,
             perc_wastrec_60d= sum(recoveries60d)/(sum(recoveries60d)+sum(no_recoveries60d))*100,
             perc_wastrec_90d= sum(recoveries90d)/(sum(recoveries90d)+sum(no_recoveries90d))*100,
             perc_sevwastinc_30d= sum(sevwast30d)/(sum(sevwast30d)+sum(no_sevwast30d)*100),
             perc_sevwastinc_60d= sum(sevwast60d)/(sum(sevwast60d)+sum(no_sevwast60d)*100),
             perc_sevwastinc_90d= sum(sevwast90d)/(sum(sevwast90d)+sum(no_sevwast90d)*100)
    ) %>%
    as.data.frame()
  
  #Calculate average episode lengths
  episode_duration <- d %>% ungroup() %>%
    filter(wast_inc==1) %>% #drop non-wasting periods
    subset(., select=c(SUBJID,agecat,wasting_duration)) %>% 
    as.data.frame()
  
  # #Calculate mean total duration of wasting per child
  # note that this is among all children, not just kids who
  # experience wasting, so kids who are never wasted have duration
  # of 0, not NA.
  duration <- episode_duration %>%
    group_by(SUBJID) %>%
    summarize(
      total_duration= sum(wasting_duration, na.rm=T)
    )
  
  
  
  #Create taable of summary statistics
  tab <- data.frame(child_nmeas, Incidence_df[1:4], total_duration, average_duration, Incidence_df[-c(1:4, 13:24)])
  
  #Calculate means
  means <- NULL
  try(
    means <- rbind(
      #longitudinal prevalence
      mean95CI(Y=(d$WHZ < -2), id=d$SUBJID, proportion=T, percent=F),
      mean95CI(Y=(d$WHZ < -3), id=d$SUBJID, proportion=T, percent=F),
      #duration
      mean95CI(Y=episode_duration$wasting_duration, id=episode_duration$SUBJID, proportion=F, percent=F),
      #incidence rates
      mean95CI(tab$wast_ep, persontime=tab$persontime, count=T) * c(1,rep(1000,5)),
      mean95CI(tab$sevwast_ep, persontime=tab$sev_persontime, count=T) * c(1,rep(1000,5)),
      mean95CI(tab$wast_rec_ep, persontime=tab$recovery_persontime, count=T) * c(1,rep(1000,5)),
      mean95CI(tab$sevwast_rec_ep, persontime=tab$sevrecovery_persontime, count=T) * c(1,rep(1000,5)),
      #percent recovery
      mean95CI(Y=d$wast_rec30,  proportion=T, percent=F),
      mean95CI(Y=d$wast_rec60,  proportion=T, percent=F),
      mean95CI(Y=d$wast_rec90,  proportion=T, percent=F),
      mean95CI(Y=d$sevwast_inc30d,  proportion=T, percent=F),
      mean95CI(Y=d$sevwast_inc60d,  proportion=T, percent=F),
      mean95CI(Y=d$sevwast_inc90d,  proportion=T, percent=F)))
  
  means <- data.frame(statistic =   c("Prevalence\nof\nwasting",
                                      "Prevalence\nof\nsevere\nwasting",
                                      "Average\nduration\nof\nwasting",
                                      "Wasting\nincidence\nrate",
                                      "Severe\nwasting\nincidence\nrate",
                                      "Wasting\nrecovery\nincidence\nrate",
                                      "Severe\nwasting\nrecovery\nincidence\nrate",
                                      "Percent\nwasting\nrecovered\nin 30 days",
                                      "Percent\nwasting\nrecovered\nin 60 days",
                                      "Percent\nwasting\nrecovered\nin 90 days",
                                      "Percent\nfalter to\nsevere\nwasting\nin 30 days",
                                      "Percent\nfalter to\nsevere\nwasting\nin 60 days",
                                      "Percent\nfalter to\nsevere\nwasting\nin 90 days"),means)
  
  return(list(tab, means))
  #}
}









#----------------------------------------------
#Function to format summary tables
#----------------------------------------------
tab_format <- function(tab){
  
  
  tab<-as.data.frame(tab)
  tab$`Child age stratification` <- rownames(tab)
  tab1 <- tab %>% 
    subset(., select=c(
      `Child age stratification`,
      num_children,
      num_measurements,
      prev_wast,
      prev_sevwast,
      anywast,
      anysevwast,
      #total_duration,
      average_duration
    )) %>%
    rename( `Number of children` = num_children,
            `Average number of measurements per child` = num_measurements,
            `Prevalence of wasting across all measurements` =  prev_wast,
            `Prevalence of severe wasting across all measurements` = prev_sevwast,
            `Proportion of children who were ever wasted`= anywast,
            `Proportion of children who were ever severely wasted` = anysevwast,
            #`Total duration of wasting episodes (days)` = total_duration,
            `Average duration of wasting episodes (days)` = average_duration)
  
  tab2 <- tab %>% 
    subset(., select=c(
      `Child age stratification`,
      num_children,
      num_measurements,
      wast_ep,
      sevwast_ep,
      persontime,
      sev_persontime,
      wastIR,
      sevwastIR
    )) %>%
    rename( `Number of children` = num_children,
            `Average number of measurements per child` = num_measurements,
            `Number of wasting episodes` = wast_ep,
            `Number of severe wasting episodes` = sevwast_ep,
            `No. of days at risk of wasting` = persontime,
            `No. of days at risk of severe wasting` = sev_persontime,
            `Wasting incidence rate per 1000 days` = wastIR,
            `Severe wasting incidence rate per 1000 days` = sevwastIR
    )
  
  
  tab3 <- tab %>% 
    subset(., select=c(
      `Child age stratification`,
      num_children,
      num_measurements,
      wast_rec_ep,
      recovery_persontime,
      wastrecIR,
      perc_wastrec_30d,       
      perc_wastrec_60d,
      perc_wastrec_90d
    )) %>%
    rename( `Number of children` = num_children,
            `Average number of measurements per child` = num_measurements,
            `Number of recoveries from wasting` = wast_rec_ep,
            `No. of days at eligible for wasting recovery` = recovery_persontime,
            `Wasting recovery incidence rate per 1000 days` = wastrecIR,
            `Percent of wasting episodes recovered from in 30 days` = perc_wastrec_30d,       
            `Percent of wasting episodes recovered from in 60 days` = perc_wastrec_60d,
            `Percent of wasting episodes recovered from in 90 days` = perc_wastrec_90d)
  return(list(tab1,tab2,tab3, tab))
}










