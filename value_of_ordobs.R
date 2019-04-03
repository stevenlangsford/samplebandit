rm(list=ls())#to delete
source("stimsetup.R")#note setup sets seed.

lpad <- function(i,n=3){ #used to name flipbook files so alphabetical order matches numerical order.
    if(nchar(as.character(i))<n){
        return (paste0(paste0(rep(0,n-nchar(as.character(i))),collapse=""),i))
    }
    return(as.character(i));
}

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

check_values <- function(samples){#assumes stimvalues.df exists in the environment (created in stimsetup)
estvals <- samples%>%select(starts_with("estval"))%>%gather(optionID,estval)
return(
    ggplot(estvals,aes(x=estval,color=optionID))+geom_density()+
    geom_vline(data=stimvalues.df,aes(xintercept=value,color=paste0("estval.",option)))+ggtitle("Values")
    )
}

check_features <- function(samples){
features <- samples%>%select(starts_with("features."))%>%gather(id,est)%>%
    mutate(option=sapply(id,function(x){as.numeric(substr(x,10,10))}),
           feature=sapply(id,function(x){as.numeric(substr(x,12,12))}))

return(ggplot(features,aes(x=est,color=as.factor(option)))+geom_density()+
    facet_wrap(.~feature)+
    geom_vline(data=stim.df,aes(xintercept=value,color=as.factor(option)))+ggtitle("features"))
}

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


##MAIN
total_obs <- 25 #eventually want to decide whether to observe or choose, but for now just watch trajectories over this many observations.
##keep a running df of all the obs so far

calcobs.df <- data.frame(); #not used for now: testing ordobs.            
ordobs.df <- data.frame();

##init to zero: special no-obs round just refers to priors. 
datalist <- list(n_features=n_features,n_options=n_options)xo

fit <- stan(file="no_obs.stan",data=datalist,
            iter=2000,
            chains=4)
samples <- as.data.frame(extract(fit,permuted=TRUE))


##Keep a running df of value of information.
infohistory.df <- data.frame(n_obs=0, evi=est_value_of_information(samples))
sop.plot <- (check_values(samples)+check_features(samples))/
    (ggplot(infohistory.df,aes(x=n_obs,y=evi))+geom_point()+geom_line()+xlim(c(0,total_obs))+ylim(c(0,infohistory.df$evi[1]+.05)))
ggsave(sop.plot,file=paste0("flipbook/step",lpad(0),".png"),width=10)



for(i in 1:total_obs){
#    calcobs.df <- add_calcobs(old_calcobs=calcobs.df)
    ## datalist=list(n_features=n_features,
    ##               n_options=n_options,
    ##               n_calcobs=nrow(calcobs.df),
    ##               calcobs_whichoption=as.array(calcobs.df$option),#as.array because otherwise arrays of length one convert to scalars & break stan's typing.
    ##               calcobs_whichfeature=as.array(calcobs.df$feature),
    ##               calcobs_obsvalue=as.array(calcobs.df$obs))
    ordobs.df <- add_ordobs(old_ordobs=ordobs.df)
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

    rm(samples); gc();#avoid memory segfault?
    
    samples <- as.data.frame(extract(fit,permuted=TRUE))
    infohistory.df <- rbind(infohistory.df,
                            data.frame(n_obs=nrow(ordobs.df)+nrow(calcobs.df),
                                       evi=est_value_of_information(samples)))

    sop.plot <- (check_values(samples)+check_features(samples))/
        (ggplot(infohistory.df,aes(x=n_obs,y=evi))+geom_point()+geom_line()+xlim(c(0,total_obs))+ylim(c(0,infohistory.df$evi[1]+.05)))
    ggsave(sop.plot,file=paste0("flipbook/step",lpad(i),".png"),width=10)
}
