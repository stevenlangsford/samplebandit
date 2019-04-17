rm(list=ls())
#Assumes you have complete obs.df, just rehydrates it to get samples representing inferred beliefs about the options. Note agent is aware of noise settings, so if your're restoring an old session these must match to make any sense.

getsamples <- function(calcobs.df,ordobs.df){ #casewise dispatch: if calcobs or ordobs or both are 0, the calls are different. Apparently you can't just set an array to 0 dimensions and expect it to be ignored. Maybe there's a better workaround than this?
    if(verbose)print("getsamples")
    myiter=1000; #somewhat rougher than 2000! Probably ok for dev exploration but for real results crank this up a little?
    
    if(nrow(calcobs.df)==0 & nrow(ordobs.df)==0){
        if(verbose)print("starting getsamples: priors")
        datalist <- list(n_features=max(stim.df$feature),
                         n_options=max(stim.df$option),
                         n_trials=max(stim.df$trial))
        fit <- stan(file="models/no_obs.stan",data=datalist,
                    iter=myiter,
                    chains=4)
        if(verbose)print("exit getsamples no-obs")
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    if(nrow(calcobs.df)==0){
        if(verbose)print("starting getsamples: ordonly")
        datalist=list(n_features=max(stim.df$feature),
                      n_options=max(stim.df$option),
                      n_trials=max(stim.df$trial),
                      n_ordobs=nrow(ordobs.df),
                      ordoption1=as.array(ordobs.df$opt1),
                      ordoption2=as.array(ordobs.df$opt2),
                      ord_whichfeature=as.array(ordobs.df$feature),
                      ord_whichtrial=as.array(ordobs.df$trial),
                      ordobs_obsvalue=as.array(ordobs.df$obs)
                      )
        fit <- stan(file="models/ordobs_only.stan",data=datalist,
                    iter=myiter,
                    chains=4)
        if(verbose)print("exit getsamples ordonly")
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    if(nrow(ordobs.df)==0){
        if(verbose)print("starting getsamples: calconly")
        datalist=list(n_features=max(stim.df$feature),
                      n_options=max(stim.df$option),
                      n_trials=max(stim.df$trial),
                      n_calcobs=nrow(calcobs.df),
                      calcobs_whichoption=as.array(calcobs.df$option),
                      calcobs_whichfeature=as.array(calcobs.df$feature),
                      calcobs_whichtrial=as.array(calcobs.df$trial),
                      calcobs_obsvalue=as.array(calcobs.df$obs))
        
        fit <- stan(file="models/calcobs_only.stan",data=datalist,
                    iter=myiter,
                    chains=4)
        if(verbose)print("exit getsamples calconly")
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    ##full (both kinds of obs available)
    if(verbose)print("starting getsamples: full")
    datalist=list(
        n_features=max(stim.df$feature),
        n_options=max(stim.df$option),
        n_trials=max(stim.df$trial),
        n_calcobs=nrow(calcobs.df),
        calcobs_whichoption=as.array(calcobs.df$option),
        calcobs_whichfeature=as.array(calcobs.df$feature),
        calcobs_whichtrial=as.array(calcobs.df$trial),
        calcobs_obsvalue=as.array(calcobs.df$obs),
        n_ordobs=nrow(ordobs.df),
        ordoption1=as.array(ordobs.df$opt1),
        ordoption2=as.array(ordobs.df$opt2),
        ord_whichfeature=as.array(ordobs.df$feature),
        ord_whichtrial=as.array(ordobs.df$trial),
        ordobs_obsvalue=as.array(ordobs.df$obs)
    )

    fit <- stan(file="models/seqcalcord.stan",data=datalist,
                iter=myiter,
                chains=4)
    if(verbose)print("exit getsamples full")
    return(as.data.frame(extract(fit,permuted=TRUE)))
}

##MAIN
calcobs.df <- data.frame() #or wherever this comes from
ordobs.df <- data.frame()

samples <- getsamples(calcobs.df,ordobs.df) #tada.
