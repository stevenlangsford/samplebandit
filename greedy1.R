source("stimsetup.R")
source("checkplots.R")

est_value_of_information <- function(samples){
expectation <- samples%>%select(starts_with("estval"))%>%gather(id,value)%>%group_by(id)%>%summarize_all(mean)%>%arrange(desc(value))

bestchoice <- expectation$id[1]%>%as.character#%>%substr(.,8,8)%>%as.numeric

est_confidence <- samples%>%select(starts_with("estval"))%>%mutate(max=pmax(estval.1,estval.2,estval.3))
est_confidence$chose <- est_confidence[,bestchoice]
est_confidence$regret <- with(est_confidence,chose-max)

p_changemind <- 1-sum(est_confidence$regret==0)/nrow(est_confidence) #chance you have not chosen max:

return(-mean(est_confidence$regret)) #with perfect information, regret goes to zero: expected value of information is mean regret. 
#Ok, but that's the value of perfect information. The value of "one more observation" is somewhat less? *And different for the different types of obs?*
}

#add obs functions refer to stim.df, created in stimsetup.R & just left in the environment :-(
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
            return(rbind(old_ordobs,data.frame(opt1=whichoption1,opt2=whichoption2,feature=whichfeature,obs=rnorm(1,truediff,ordobsnoise))))
}

imagine_calcobs <- function(whichoption=base::sample(1:3,1),
                        whichfeature=base::sample(1:2,1),
                        whichtrial=1,
                        old_calcobs=data.frame(),
                        samples #gonna just pull one est sample and pretend that's the next obs. Legit?
                        ){
        return(
        rbind(old_calcobs,data.frame(trial=whichtrial,
                                        option=whichoption,
                                        feature=whichfeature,
                                        obs=samples[base::sample(1:nrow(samples),1),paste0("features.",whichoption,".",whichfeature)]
            ))
        )
}

imagine_ordobs <- function(whichoption1=base::sample(1:3,1),
                       whichoption2=base::sample(1:3,1),
                       whichfeature=base::sample(1:2,1),
                       whichtrial=1,
                       old_ordobs=data.frame(),
                       samples
                       ){
    obs1 <- samples[base::sample(1:nrow(samples),1),paste0("features.",whichoption1,".",whichfeature)]
     obs2 <- samples[base::sample(1:nrow(samples),1),paste0("features.",whichoption2,".",whichfeature)]
    
    return(rbind(old_ordobs,data.frame(opt1=whichoption1,opt2=whichoption2,feature=whichfeature,obs=obs1-obs2)))
}

getsamples <- function(calcobs.df,ordobs.df){ #casewise dispatch: if calcobs or ordobs or both are 0, the calls are different. Apparently can't just set an array to 0 dimensions and expect it to be ignored. Maybe there's a better workaround than this?
    if(nrow(calcobs.df)==0 & nrow(ordobs.df)==0){
        datalist <- list(n_features=n_features,n_options=n_options)
        fit <- stan(file="no_obs.stan",data=datalist,
                    iter=2000,
                    chains=4)
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    if(nrow(calcobs.df)==0){
    datalist=list(n_features=n_features,
                  n_options=n_options,
                  n_ordobs=nrow(ordobs.df),
                  ordoption1=as.array(ordobs.df$opt1),
                  ordoption2=as.array(ordobs.df$opt2),
                  ord_whichfeature=as.array(ordobs.df$feature),
                  ordobs_obsvalue=as.array(ordobs.df$obs)
                  )
    fit <- stan(file="ordobs_only.stan",data=datalist,
                iter=2000,
                chains=4)
    return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    if(nrow(ordobs.df)==0){
        datalist=list(n_features=n_features,
                      n_options=n_options,
                      n_calcobs=nrow(calcobs.df),
                      calcobs_whichoption=as.array(calcobs.df$option),
                      calcobs_whichfeature=as.array(calcobs.df$feature),
                      calcobs_obsvalue=as.array(calcobs.df$obs))
        
        fit <- stan(file="calcobs_only.stan",data=datalist,
                    iter=2000,
                    chains=4)
        return(as.data.frame(extract(fit,permuted=TRUE)))
    }
    datalist=list(
        n_features=n_features,
        n_options=n_options,
        n_calcobs=nrow(calcobs.df),
        calcobs_whichoption=as.array(calcobs.df$option),
        calcobs_whichfeature=as.array(calcobs.df$feature),
        calcobs_obsvalue=as.array(calcobs.df$obs),
        n_ordobs=nrow(ordobs.df),
        ordoption1=as.array(ordobs.df$opt1),
        ordoption2=as.array(ordobs.df$opt2),
        ord_whichfeature=as.array(ordobs.df$feature),
        ordobs_obsvalue=as.array(ordobs.df$obs)
    )
    
    fit <- stan(file="seqcalcord.stan",data=datalist,
                iter=2000,
                chains=4)
    return(as.data.frame(extract(fit,permuted=TRUE)))
}

##MAIN
calcobs.df <- data.frame() #init with no obs
ordobs.df <- data.frame()
reps_per_est <- 10 #should really bump this up. 10 ok for dev I guess.
obs_to_decision <- 5 #whole point is to terminate based on evi! Todo. :-)
obscounter <- 1; #set for while loop.
while(obscounter <= obs_to_decision){ #for loop acts weird with restoration? Is while actually better, or did you just miswrite/misread here?
    if(exists("samples")){rm(samples);gc();}#Does this even help? #Still seems to die.
    ##If you die, you can recover by starting again from the stim and the last set of observations.
    ##Even if you don't die, the last set of obs are probably interesting?
    path <- "greedy_flipbook/obsdump/"
    saved_obscount <- as.numeric(strsplit(tail(list.files(path,pattern="ord"),1)," ")[[1]][[2]]) #check if you can restore a partial run
    if(saved_obscount>obscounter){
        ordobs.df <- data.frame()
        try({ordobs.df<<-read.csv(paste0(path,tail(list.files(path,pattern="ord"),1)))})#These are saved at the tail of the loop.
        
        calcobs.df <- data.frame()
        try({calcobs.df <<- read.csv(paste0(path,tail(list.files(path,pattern="calc"),1)))})
        obscounter=saved_obscount+1
    }
    
samples <- getsamples(calcobs.df,ordobs.df) #must remain current

current_infoval=est_value_of_information(samples)

calcestobsval=data.frame(); #this cumulation thing is an antipattern?
ordestobsval=data.frame();

for(i in 1:reps_per_est){
    for(anoption in 1:n_options){
        for(afeature in 1:n_features){
            if(exists("imagined_beliefs")){rm(imagined_beliefs);gc();} #Still not sure if this helps. Flailing.
            imagined_beliefs <- getsamples(
                imagine_calcobs(whichoption=anoption,
                            whichfeature=afeature,
                            whichtrial=1, #unused, this is going to bite you?
                            old_calcobs=calcobs.df,
                            samples),
                ordobs.df)
            calcestobsval <- rbind(calcestobsval,data.frame(
                                                   option=anoption,
                                                   feature=afeature,
                                                   new_evi=est_value_of_information(imagined_beliefs))
                                                     )
            }
        }
    if(exists("imagined_beliefs")){rm(imagined_beliefs);gc();} #Still not sure if this helps. Flailing.

    ##same again for ordobs:
    for(opt1 in 2:n_options){
        for(opt2 in 1:(opt1-1)){
            for(afeature in 1:n_features){
                if(opt1==opt2)next;#self-comparison is not useful.
                if(exists("imagined_beliefs")){rm(imagined_beliefs);gc();} #Still not sure if this helps. Flailing.
                imagined_beliefs <- getsamples(calcobs.df,
                                               imagine_ordobs(whichoption1=opt1,
                                                              whichoption2=opt2,
                                                              whichfeature=afeature,
                                                              whichtrial=1,#unused
                                                              old_ordobs=ordobs.df,
                                                              samples
                                                              ))
                ordestobsval <- rbind(ordestobsval,data.frame(
                                                       opt1=opt1,
                                                       opt2=opt2,
                                                       feature=afeature,
                                                       new_evi=est_value_of_information(imagined_beliefs)))
                
            }
        }
    }#end for all possible ordobs
    if(exists("imagined_beliefs")){rm(imagined_beliefs);gc();} #Still not sure if this helps. Totally flailing.
    
    print(paste("rep",i,"ok in round",obscounter))
}#end for rep in reps per est


bestcalc <-
    calcestobsval%>%group_by(option,feature)%>%summarize(expected_newevi=mean(new_evi))%>%ungroup()%>%arrange(expected_newevi)%>%top_n(-1,expected_newevi)
bestord <- 
    ordestobsval%>%group_by(opt1,opt2,feature)%>%summarize(expected_newevi=mean(new_evi))%>%ungroup()%>%arrange(expected_newevi)%>%top_n(-1,expected_newevi)

    ##actually make the observation!
    if(bestcalc$expected_newevi<bestord$expected_newevi){
        calcobs.df <- add_calcobs(whichoption=bestcalc$option,
                                  whichfeature=bestcalc$feature,
                                  whichtrial=1,
                                  old_calcobs=calcobs.df)
    }else{
        ordobs.df <- add_ordobs(whichoption1=bestord$opt1,
                                whichoption2=bestord$opt2,
                                whichfeature=bestord$feature,
                                whichtrial=1,
                                old_ordobs=ordobs.df)
    }

bestobs_string <-
    paste(if(bestcalc$expected_newevi<bestord$expected_newevi){c("calc",sapply(bestcalc[1,],signif,3))}else{c("ord",sapply(bestord[1,],signif,3))},collapse=":")
    
    ggsave(next_obs_plot(samples,calcestobsval,ordestobsval,paste("current ", signif(current_infoval,3), "best obs:", bestobs_string)),file=paste("greedy_flipbook/greedy",lpad(obscounter,3),".png"),width=10)

    write.csv(stim.df,file="greedy_flipbook/obsdump/stim.csv")
    write.csv(calcobs.df,file=paste("greedy_flipbook/obsdump/calcobs",obscounter,".csv"),row.names=FALSE)
    write.csv(ordobs.df,file=paste("greedy_flipbook/obsdump/ordobs",obscounter,".csv"),row.names=FALSE)
}#end for obscounter in obs to decision
