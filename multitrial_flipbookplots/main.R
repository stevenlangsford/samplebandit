rm(list=ls())
##Meta setup: stim and goals for this run. Main objects left in the environment are samples (final beliefs) and calcobs.df, ordobs.df (record of how you got there.)
##more specific run setup appears at MAIN below.
ploteverything <- TRUE #flipbook plots appearing in plots/runname
writecsvs <- TRUE# calcobs and ordobs, also saved in /plots.
recordtrajectory <- TRUE #slow, but super informative. Records to decision_trajectory.df
verbose <- FALSE #breadcrumb trail of print statements... why can't R give line numbers on errors?
runname <- "rtraj_delta_pt2" #used in path/filename for saved stuff
decision_trajectory.df <- data.frame()

source("stim_setup/context_stimsetup.R") #everything here assumes stim.df exists, is visible in global env. stimsetup also loads libraries.
if(verbose)print("got stim")


if(ploteverything){
    dir.create(paste0("plots/",runname))
    for(i in 1:max(stim.df$trial)){
        dir.create(paste0("plots/",runname,"/trial",i))
    }
    source("vis.R")
}

starttime <- Sys.time() #this is not the best-practice way to time stuff.

##Helper functions: the things that actually do the work. These often refer to stim.df, created in stimsetup.R & just left in the environment...

get_wideests <- function(samples){ #re-formats samples so you can cbind ests (as means) to widestim.df
    if(verbose)print("start get_wideests")
    ests <- data.frame()
    for(trialid in 1:max(stim.df$trial)){
        ests <- rbind(ests,
                      data.frame(
                          est_a1=samples%>%select(starts_with(paste0("features.",trialid,".1.1")))%>%summarize_all(mean)%>%as.numeric,
                          est_a2=samples%>%select(starts_with(paste0("features.",trialid,".1.2")))%>%summarize_all(mean)%>%as.numeric,
                          est_b1=samples%>%select(starts_with(paste0("features.",trialid,".2.1")))%>%summarize_all(mean)%>%as.numeric,
                          est_b2=samples%>%select(starts_with(paste0("features.",trialid,".2.2")))%>%summarize_all(mean)%>%as.numeric,
                          est_c1=samples%>%select(starts_with(paste0("features.",trialid,".3.1")))%>%summarize_all(mean)%>%as.numeric,
                          est_c2=samples%>%select(starts_with(paste0("features.",trialid,".3.2")))%>%summarize_all(mean)%>%as.numeric,
                          est_avalue =
                              mean(map2((samples%>%select(starts_with(paste0("features.",trialid,".1.1"))))[,1],
                              (samples%>%select(starts_with(paste0("features.",trialid,".1.2"))))[,1],
                              function(a,b){combineFeatures(c(a,b))})%>%as.numeric),
                          est_bvalue =
                              mean(map2((samples%>%select(starts_with(paste0("features.",trialid,".2.1"))))[,1],
                              (samples%>%select(starts_with(paste0("features.",trialid,".2.2"))))[,1],
                              function(a,b){combineFeatures(c(a,b))})%>%as.numeric),
                          est_cvalue =
                              mean(map2((samples%>%select(starts_with(paste0("features.",trialid,".3.1"))))[,1],
                              (samples%>%select(starts_with(paste0("features.",trialid,".3.2"))))[,1],
                              function(a,b){combineFeatures(c(a,b))})%>%as.numeric) #combineFeatures uglified for map so n_features would be unconstrained. But n=2 baked in here. bituvva mess
                      )%>%mutate(currentbest=which(c(est_avalue,est_bvalue,est_cvalue)==max(est_avalue,est_bvalue,est_cvalue)[1]))%>%as.data.frame
                      )
    }#nd for each trialid
    if(verbose)print("exit get_wideests")
    return(ests)
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

imagine_calcobs <- function(samples,whichtrial,whichoption,whichfeature){
    if(verbose)print("imagine_calcobs")
    ans <- (data.frame(timestep=NA,#exists so this will match the format of add_calcobs, but imagination doesn't have a time.
                       trial=whichtrial,
                       option=whichoption,
                       feature=whichfeature,
                       obs=base::sample(samples[,paste0("features.",whichtrial,".",whichoption,".",whichfeature)],1)))
    if(verbose)print("exit imagine_calcobs")
    return(ans)
}

imagine_ordobs <- function(samples,whichtrial,opt1,opt2,whichfeature){
    if(verbose)print("imagine_ordobs")
    ans <- (data.frame(timestep=NA,#exists to match the format of add_ordobs
                       trial=whichtrial,
                       opt1=opt1,
                       opt2=opt2,
                       feature=whichfeature,
                       obs=base::sample(samples[,paste0("features.",whichtrial,".",opt1,".",whichfeature)],1)-
                           base::sample(samples[,paste0("features.",whichtrial,".",opt2,".",whichfeature)],1)))
    if(verbose)print("exit imagine_ordobs")
    return(ans)
}

single_p_changemind <- function(samples,whichtrial){
    if(verbose)print("single_p_changemind")
    expectation <- samples%>%select(starts_with(paste0("estval.",whichtrial)))%>%gather(id,value)%>%group_by(id)%>%summarize_all(mean)%>%arrange(desc(value))
    bestchoice <- expectation$id[1]%>%as.character
    
    est.df <- samples%>%select(starts_with(paste0("estval.",whichtrial)))
    est.df$max=sapply(1:nrow(est.df),function(i){max(est.df[i,])})#what's the tidyverse way? pmax needs named args, but the names change?
    est.df$chose <- est.df[,bestchoice]
    est.df$regret <- with(est.df,chose-max)
    
    p_changemind <- 1-sum(est.df$regret==0)/nrow(est.df) #chance you have not chosen max
    if(verbose)print("exit single_p_changemind")
    return(p_changemind)
}
batch_p_changemind <- function(samples){
    if(verbose)print("batch_p_changemind")
    ans <- data.frame(trial=1:max(stim.df$trial),pchangemind=sapply(1:max(stim.df$trial),function(x){single_p_changemind(samples,x)}))
    if(verbose)print("exit batch_p_changemind")
    return(ans)
}

single_evi <- function(samples,whichtrial){
    if(verbose)print("single_evi")
    expectation <- samples%>%select(starts_with(paste0("estval.",whichtrial)))%>%gather(id,value)%>%group_by(id)%>%summarize_all(mean)%>%arrange(desc(value))
    bestchoice <- expectation$id[1]%>%as.character
    
    est.df <- samples%>%select(starts_with(paste0("estval.",whichtrial)))
    est.df$max=sapply(1:nrow(est.df),function(i){max(est.df[i,])})#what's the tidyverse way? pmax needs named args, but the names change?
    est.df$chose <- est.df[,bestchoice]
    est.df$regret <- with(est.df,chose-max)
    
    ##    p_changemind <- 1-sum(est_confidence$regret==0)/nrow(est_confidence) #chance you have not chosen max:
    rm(samples);gc();#doesn't this happen anyway when the fn exits? The env is orphaned and vanishes, right?
    if(verbose)print("exit single_evi")
    return(-mean(est.df$regret)) #This expected-regret is the expected value of (perfect) information, right? (the value of 1-more-obs will be somewhat less!)   
}

batch_evi <- function(samples){
    if(verbose)print("batch_evi");
    ans <- data.frame(trial=1:max(stim.df$trial),evi=sapply(1:max(stim.df$trial),function(i){single_evi(samples,i)}))
    if(verbose)print("exit batch_evi")
    return(ans)
}

batch_imagineobs <- function(samples,calcobs.df,ordobs.df){
    if(verbose)print("batch_imagineobs")
    candidate_obs <- data.frame()

    current_evi <- batch_evi(samples)%>%arrange(trial) #arrange should be redundant, but safer to always be explicit?

    ##Can test one obs per trial each fit (trials assumed independent). So, cycle through each possible obs, applying that same imagined-obs to every trial at once, record the est obs value, then clear everything and repeat on the next obs-type. Results accumulate in candidate_obs: when you're done, search this for the best obs type for each trial.
    
    for(anoption in 1:max(stim.df$option)){
        for(afeature in 1:max(stim.df$feature)){
            
            imagined_calcobs.df <- data.frame(); 

            for(atrial in 1:max(stim.df$trial)){
                imagined_calcobs.df <- rbind(imagined_calcobs.df,
                                             imagine_calcobs(samples,atrial,anoption,afeature))
            }
            postobs_evi <- batch_evi(getsamples(rbind(calcobs.df,imagined_calcobs.df), ordobs.df))
            imagined_calcobs.df$value <- current_evi$evi-postobs_evi$evi #Evi is expected value of perfect information. So info gained is "how much evi drops"(?)
            candidate_obs <- rbind(candidate_obs,
                                   imagined_calcobs.df%>%
                                   rename(option1=option)%>%
                                   mutate(option2=NA,type="calc")%>%
                                   select(type,trial,option1,option2,feature,obs,value))
            rm(postobs_evi); gc();#Maybe if you learned to profile code you could actually see if there are effective places to do this rather than flailing?
        }
    }

    ##All over again but for imagined ordobs.
    for(anoption2 in 2:max(stim.df$option)){
        for(anoption1 in 1:anoption2){
            if(anoption1==anoption2)next;
            
            for(afeature in 1:max(stim.df$feature)){
                
                imagined_ordobs.df <- data.frame(); 

                for(atrial in 1:max(stim.df$trial)){
                    imagined_ordobs.df <- rbind(imagined_ordobs.df,
                                                imagine_ordobs(samples,atrial,anoption1,anoption2,afeature))
                }
                postobs_evi <- batch_evi(getsamples(calcobs.df, rbind(ordobs.df,imagined_ordobs.df)))
                imagined_ordobs.df$value <- current_evi$evi-postobs_evi$evi #Evi is expected value of perfect information. So info gained is "how much evi drops"(?)
                candidate_obs <- rbind(candidate_obs,
                                       imagined_ordobs.df%>%rename(option1=opt1,option2=opt2)%>%mutate(type="ord")%>%select(type,trial,option1,option2,feature,obs,value)                                       )
                rm(postobs_evi); gc();#More flailing?
            }
        }
    }
    if(verbose)print("exit batch_imagineobs")
    return(candidate_obs)
}#end batch imagine

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
obs_per_trial <- 7#Every trial gets the same number of obs. Would be really nice to halt trials that hit some criterion, how nasty would it be to retrofit that?
obs_per_imaginationcycle <- 10 #Kinda expensive, but also not clear if 10 is enough?

calcobs.df <- data.frame() #first run of getsamples with no obs just returns the priors. Must make sure the no-obs .stan files match the full one!
ordobs.df <- data.frame()

for(obsrep in 1:obs_per_trial){
    if(verbose)print(paste("Start of observation round",obsrep))#derpy progress bar
    if(exists("samples")){rm(samples);gc();}#This is just pure superstition at this point. Oh well.
    samples <- getsamples(calcobs.df,ordobs.df)

                                        #At this point samples are the current beliefs, and the obs.df are how you got there. Record this info, then work out what obs you want next.

    ## Record decision trajectory info. (quite time expensive, especially re-doing evi and pchangemind. Skip if not needed.)
    if(recordtrajectory){
        get_last_observation_string <- function(trialnumber){
            if(verbose)print(paste("get_last_observation_string",trialnumber))
            if(obsrep==1)return("priors")

            if(nrow(calcobs.df%>%filter(trial==trialnumber,timestep==max(timestep))%>%as.data.frame)==0){
                lastcalc <- data.frame(timestep=0)
                lastcalcstring="priors"
            }else{
                lastcalc <- calcobs.df%>%filter(trial==trialnumber,timestep==max(timestep))%>%as.data.frame
                lastcalcstring <- paste0("type:calc,",paste0(paste0(names(lastcalc%>%select(-timestep,-trial)),":"),lastcalc%>%select(-timestep,-trial),collapse=","))
            }

            if(nrow(ordobs.df%>%filter(trial==trialnumber,timestep==max(timestep))%>%as.data.frame)==0){
                lastord <- data.frame(timestep=0)
                lastordstring="priors"
            }else{
                lastord <- ordobs.df%>%filter(trial==trialnumber,timestep==max(timestep))%>%as.data.frame
                lastordstring <- paste0("type:ord,",paste0(paste0(names(lastord%>%select(-timestep,-trial)),":"),lastord%>%select(-timestep,-trial),collapse=","))
            }

            lastobs <- if(lastcalc$timestep>lastord$timestep){lastcalcstring}else{lastordstring}#should never be equal except at 0, when 
            if(verbose)print("exit get_last_observation_string")
            return(lastobs)
        }
        if(verbose)print("adding to decision trajectory")
        decision_trajectory.df <- rbind(decision_trajectory.df,
                                        cbind(widestim.df,
                                              data.frame(timestep=obsrep,
                                                         last_observation=sapply(widestim.df$trialID,function(i){get_last_observation_string(i)})),
                                              get_wideests(samples),
                                              batch_evi(samples)%>%left_join(batch_p_changemind(samples))%>%select(-trial)
                                              )
                                        )
        if(verbose)print("done adding to decision trajectory")
    } ##end if recording decision trajectory, on with the show. Next, imagine some possible observations.


    if(exists("candidate_obs")){rm(candidate_obs);gc()}#even more superstitious than the samples one, 'cause candidate_obs df is likely quite a bit smaller.
    candidate_obs <- data.frame()
    for(imagine in 1:obs_per_imaginationcycle){
        if(verbose)print(paste("into imagination cycle",imagine))
        candidate_obs <- rbind(candidate_obs,batch_imagineobs(samples=samples,calcobs.df=calcobs.df,ordobs.df=ordobs.df))#recompiles kill your speed, but go away after the first run? How does this work really? Something to beware of if ssh-ing into a beefier computer: might need a minibatch warmup run.
            if(verbose)print("done imagination cycle")
    }
    ##select the best observation for each trial and make it. Rinse and repeat.

    ##There's a lot of interesting info being thrown away when you reduce to the best obs. Distribution of ests contributing to the best mean, also info about what's going on in all the unchosen observations. Maybe save this stuff somewhere?
        if(verbose)print("getting bestobs")
    bestobs <- candidate_obs%>%group_by(trial,type,option1,option2,feature)%>%summarize(mean_value=mean(value))%>%ungroup()%>%group_by(trial)%>%mutate(rmv=rank(-mean_value))%>%ungroup()%>%filter(rmv==1)%>%select(-rmv)%>%as.data.frame#note 'value' in candidate obs refers to evi change, so max value is what you want

    rm(candidate_obs); gc(); #flailtown.
    
    for(i in 1:nrow(bestobs)){
        if(bestobs$type[i]=="calc"){
            calcobs.df <- add_calcobs(timestep=obsrep,
                                      whichoption=bestobs[i,"option1"],
                                      whichfeature=bestobs[i,"feature"],
                                      whichtrial=bestobs[i,"trial"],
                                      old_calcobs=calcobs.df)
        }
        if(bestobs$type[i]=="ord"){
            ordobs.df <- add_ordobs(timestep=obsrep,
                                    whichoption1=bestobs[i,"option1"],
                                    whichoption2=bestobs[i,"option2"],
                                    whichfeature=bestobs[i,"feature"],
                                    whichtrial=bestobs[i,"trial"],
                                    old_ordobs=ordobs.df)
        }
    }#end add an observation to each trial
    if(verbose)print("done adding observations")
    if(ploteverything){
        if(verbose)print("plotting everything")
        for(atrial in 1:max(stim.df$trial)){
            ggsave(belief_snapshot(samples,atrial),file=paste0("plots/",runname,"/trial",atrial,"/obs",obsrep,".png"),width=10)
        }
    }
    
}#end for each obsrep in obs-per-trial

if(writecsvs){
    write.csv(calcobs.df,file=paste0("plots/",runname,"calcobs.csv"))
    write.csv(ordobs.df,file=paste0("plots/",runname,"ordobs.csv"))
    if(recordtrajectory)write.csv(decision_trajectory.df,file=paste0("plots/",runname,"/",runname,"_trajectory.csv"))
}
finishtime <- Sys.time()
print("Done in")
print(finishtime-starttime)#if iter=1000: 16.74403 mins for 3 trials, 5 obs, 10 imagination cycles. 22.97946 mins for 5 trials 5 obs 10 cycles, 1.3/1.4 hours for 10 trials 7 obs 10 cycles.
