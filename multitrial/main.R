rm(list=ls())


source("stim_setup/rnd_stimsetup.R") #everything here assumes stim.df exists, is visible in global env. stimsetup also loads libraries.
#source("checkplots.R")
starttime <- Sys.time()

##add obs functions refer to stim.df, created in stimsetup.R & just left in the environment :-(
add_calcobs <- function(whichoption=base::sample(1:3,1),
                        whichfeature=base::sample(1:2,1),
                        whichtrial=1,
                        old_calcobs=data.frame()
                        ){
    return(
        rbind(old_calcobs,data.frame(trial=whichtrial,
                                     option=whichoption,
                                     feature=whichfeature,
                                     obs=rnorm(1,stim.df%>%
                                                 filter(feature==whichfeature,option==whichoption,trial==whichtrial)%>%
                                                 select(value)%>%as.numeric,calcobsnoise))
              ))
}

add_ordobs <- function(whichoption1=base::sample(1:3,1),
                       whichoption2=base::sample(1:3,1),
                       whichfeature=base::sample(1:2,1),
                       whichtrial=1,
                       old_ordobs=data.frame()
                       ){
    truediff <-
        stim.df%>%filter(trial==whichtrial,feature==whichfeature,option==whichoption1)%>%select(value)%>%as.numeric-
        stim.df%>%filter(trial==whichtrial,feature==whichfeature,option==whichoption2)%>%select(value)%>%as.numeric
    return(rbind(old_ordobs,data.frame(trial=whichtrial,opt1=whichoption1,opt2=whichoption2,feature=whichfeature,obs=rnorm(1,truediff,ordobsnoise))))
}

imagine_calcobs <- function(samples,whichtrial,whichoption,whichfeature){
    return(data.frame(trial=whichtrial,
               option=whichoption,
               feature=whichfeature,
               obs=base::sample(samples[,paste0("features.",whichtrial,".",whichoption,".",whichfeature)],1)))
}

imagine_ordobs <- function(samples,whichtrial,opt1,opt2,whichfeature){
    return(data.frame(trial=whichtrial,
                      opt1=opt1,
                      opt2=opt2,
                      feature=whichfeature,
                      obs=base::sample(samples[,paste0("features.",whichtrial,".",opt1,".",whichfeature)],1)-
                          base::sample(samples[,paste0("features.",whichtrial,".",opt2,".",whichfeature)],1)))
}

batch_evi <- function(samples){
    single_evi <- function(samples,whichtrial){
        expectation <- samples%>%select(starts_with(paste0("estval.",whichtrial)))%>%gather(id,value)%>%group_by(id)%>%summarize_all(mean)%>%arrange(desc(value))
        bestchoice <- expectation$id[1]%>%as.character

#        browser()
        
        est.df <- samples%>%select(starts_with(paste0("estval.",whichtrial)))
        est.df$max=sapply(1:nrow(est.df),function(i){max(est.df[i,])})#what's the tidyverse way? pmax needs named args, but the names change?
        est.df$chose <- est.df[,bestchoice]
        est.df$regret <- with(est.df,chose-max)

        ##    p_changemind <- 1-sum(est_confidence$regret==0)/nrow(est_confidence) #chance you have not chosen max:

    return(-mean(est.df$regret)) #This expected-regret is the expected value of (perfect) information, right? (the value of 1-more-obs will be somewhat less!)   
}

    return(
        data.frame(trial=1:max(stim.df$trial),evi=sapply(1:max(stim.df$trial),function(i){single_evi(samples,i)}))
        )
}

batch_imagineobs <- function(samples,calcobs.df,ordobs.df){
    candidate_obs <- data.frame(trial=c(),
                                type=c(),# calc or ord
                                option1=c(), #observe this
                                option2=c(), #NA if type==calc
                                feature=c(),
                                value=c()
                                )

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
                                       imagined_calcobs.df%>%rename(option1=option)%>%mutate(option2=NA,type="calc")%>%select(type,trial,option1,option2,feature,obs,value)                                       )
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
            }
        }
    }
 return(candidate_obs)
}#end batch imagine

getsamples <- function(calcobs.df,ordobs.df){ #casewise dispatch: if calcobs or ordobs or both are 0, the calls are different. Apparently can't just set an array to 0 dimensions and expect it to be ignored. Maybe there's a better workaround than this?

    myiter=1000; #somewhat rougher than 2000! Probably ok for dev exploration but for real results crank this up a little?
    
    if(nrow(calcobs.df)==0 & nrow(ordobs.df)==0){
        datalist <- list(n_features=max(stim.df$feature),
                         n_options=max(stim.df$option),
                         n_trials=max(stim.df$trial))
        fit <- stan(file="models/no_obs.stan",data=datalist,
                    iter=myiter,
                    chains=4)
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    if(nrow(calcobs.df)==0){
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
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    if(nrow(ordobs.df)==0){
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
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    #full (both kinds of obs available)
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
    return(as.data.frame(extract(fit,permuted=TRUE)))
}

##MAIN
calcobs.df <- data.frame() #init with no obs
ordobs.df <- data.frame()

initsamples <- getsamples(calcobs.df,ordobs.df)

candidate_obs <- batch_imagineobs(samples=initsamples,calcobs.df=calcobs.df,ordobs.df=ordobs.df)#recompiles kill your time, but go away after the first run? How does this work really?

##todo: select the best observation for each trial and make it. Rinse and repeat. (Would be nice to set a halting criterion and only pursue trials that haven't met it yet? How nasty would it be to retrofit that?)
##note value in candidate obs refers to evi change, so max value is what you want (I think, check!)

finishtime <- Sys.time()

print("Done in")
print(finishtime-starttime)
