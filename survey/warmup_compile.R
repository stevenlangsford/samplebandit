##Still don't understand the rules of when stan (re)compiles and when it doesn't have to. But try this 'warmup' thing, maybe that'll work.
##Runs one each of no obs, ordonly, calconly, full, just so you've run/compiled everything once. Then you should be good to go (although might need to start a new session?)

verbose=FALSE

getsamples <- function(calcobs.df,ordobs.df,calcnoise=.5,ordnoise=.5){ #casewise dispatch: if calcobs or ordobs or both are 0, the calls are different. Apparently you can't just set an array to 0 dimensions and expect it to be ignored. Maybe there's a better workaround than this?
    if(verbose)print("getsamples")
    myiter=1000; #somewhat rougher than 2000! Probably ok for dev exploration but for real results crank this up a little?
    
    if(nrow(calcobs.df)==0 & nrow(ordobs.df)==0){
        if(verbose)print("starting getsamples: priors")
        datalist <- list(n_features=max(stim.df$feature),
                         n_options=max(stim.df$option),
                         n_trials=max(stim.df$trial),
                         calcnoise=calcnoise,
                         ordnoise=ordnoise)
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
                      ordobs_obsvalue=as.array(ordobs.df$obs),
                      calcnoise=calcnoise,
                      ordnoise=ordnoise
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
                      calcobs_obsvalue=as.array(calcobs.df$obs),
                      calcnoise=calcnoise,
                      ordnoise=ordnoise)
        
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
        ordobs_obsvalue=as.array(ordobs.df$obs),
        calcnoise=calcnoise,
        ordnoise=ordnoise
    )

    fit <- stan(file="models/seqcalcord.stan",data=datalist,
                iter=myiter,
                chains=4)
    if(verbose)print("exit getsamples full")
    return(as.data.frame(extract(fit,permuted=TRUE)))
}

add_calcobs <- function(timestep=0,
                        whichoption=base::sample(1:3,1),
                        whichfeature=base::sample(1:2,1),
                        whichtrial=1,
                        old_calcobs=data.frame()
                        ){
    if(verbose)print("add_calcobs")
    ans <- 
        rbind(old_calcobs,data.frame(timestep=timestep,
                                     trial=whichtrial,
                                     option=whichoption,
                                     feature=whichfeature,
                                     obs=rnorm(1,stim.df%>%
                                                 filter(feature==whichfeature,option==whichoption,trial==whichtrial)%>%
                                                 select(value)%>%as.numeric,calcobsnoise))
              )
    if(verbose)print("exit add_calcobs")
    return(ans)
}

add_ordobs <- function(timestep=0,
                       whichoption1=base::sample(1:3,1),
                       whichoption2=base::sample(1:3,1),
                       whichfeature=base::sample(1:2,1),
                       whichtrial=1,
                       old_ordobs=data.frame()
                       ){
    if(verbose)print("add_ordobs")
    truediff <-
        stim.df%>%filter(trial==whichtrial,feature==whichfeature,option==whichoption1)%>%select(value)%>%as.numeric-
        stim.df%>%filter(trial==whichtrial,feature==whichfeature,option==whichoption2)%>%select(value)%>%as.numeric
    ans <- (rbind(old_ordobs,data.frame(timestep=timestep,trial=whichtrial,opt1=whichoption1,opt2=whichoption2,feature=whichfeature,obs=rnorm(1,truediff,ordobsnoise))))
    if(verbose)print("exit add_ordobs")
    return(ans)
}

##MAIN
calcobs.df <- data.frame()
ordobs.df <- data.frame()
noobs <- getsamples(calcobs.df,ordobs.df)
rm(noobs);gc();

calcobs.df <- data.frame()
ordobs.df <- add_ordobs()
ordonly <- getsamples(calcobs.df,ordobs.df)
rm(ordonly);gc();

calcobs.df <- add_calcobs()
ordobs.df <- data.frame()
calconly <- getsamples(calcobs.df,ordobs.df)
rm(calconly);gc();

calcobs.df <- add_calcobs()
ordobs.df <- add_ordobs()
full <- getsamples(calcobs.df,ordobs.df)
rm(full);gc();

##ok now you should have compiled the model(s).
