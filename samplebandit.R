library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(theme_light())

#rm(list=ls())
set.seed(1)

##still devtesting: let's just get some estimates for one option and a given string of calc and ord obs.

##stim setup
n_trials <- 1 #current .stan is limited to one trial at a time! must be 1.
n_options <- 3
n_features <- 2

##ppnt setup: stan is 'aware' of these, so they should match.
calcobsnoise <- 0.5
ordobsnoise <- 0.5 #Should be higher than calcnoise? By how much? Or possibly an err rate on a discrete var, howes style, if you like pain.

getOption <- function(){
    rnorm(n_features,0,1) #Fun to split up into different distributions for different features?
}
combineFeatures <- function(features){
    return(features[1]+features[2]);#must match features_to_value in .stan model
}

stim.df <- data.frame()
stimvalues.df <- data.frame();

for(atrial in 1:n_trials){
    for(anoption in 1:n_options){
        stim.df <- rbind(stim.df,data.frame(value=getOption(),feature=1:n_features,option=anoption,trial=atrial));
        stimvalues.df <- rbind(stimvalues.df,data.frame(trial=atrial,
                                                        option=anoption,
                                                        value=combineFeatures(tail(stim.df,2)$value)))
    }
}
    
#test: whack down a couple observations of each feature?
calcobs.df <- data.frame();
ordobs.df <- data.frame();

#some calcobs:
for(anobs in 1:1){#super-crude: this-many-calcobs-each-option, for devtesting.
    
for(mytrial in 1:n_trials){
    mystim <- filter(stim.df,trial==mytrial)

    for(myoption in 1:n_options){
        for(myfeature in 1:n_features){
            calcobs.df <- rbind(calcobs.df,data.frame(trial=mytrial,
                                              option=myoption,
                                              feature=myfeature,
                                              obs=rnorm(1,mystim%>%filter(feature==myfeature,option==myoption)%>%select(value)%>%as.numeric,calcobsnoise))
                            )
        }
    }

}

}#end some calcobs

##some ordobs:
for(anobs in 1:10){
    
    for(atrial in 1:n_trials){
    opt1=1
    opt2=1
    while(opt1==opt2){
    opt1 <- base::sample(1:n_options,1)
    opt2 <- base::sample(1:n_options,1)
    }#ban self comparison
    myfeature <- base::sample(1:n_features,1)

        truediff <-
            stim.df%>%filter(trial==atrial,feature==myfeature,option==opt1)%>%select(value)%>%as.numeric-
            stim.df%>%filter(trial==atrial,feature==myfeature,option==opt2)%>%select(value)%>%as.numeric
        ordobs.df <- rbind(ordobs.df,data.frame(opt1=opt1,opt2=opt2,feature=myfeature,obs=rnorm(1,truediff,ordobsnoise)))
    }
}

datalist = list(
    n_features=n_features,
    n_options=n_options,
    n_calcobs=nrow(calcobs.df),
    calcobs_whichoption=calcobs.df$option,
    calcobs_whichfeature=calcobs.df$feature,
    calcobs_obsvalue=calcobs.df$obs,

    n_ordobs=nrow(ordobs.df),
    ordoption1=ordobs.df$opt1,
    ordoption2=ordobs.df$opt2,
    ord_whichfeature=ordobs.df$feature,
    ordobs_obsvalue=ordobs.df$obs)

fit <- stan(file="seqcalcord.stan",
            data=datalist,
            iter=2000,
            chains=4)

samples <- as.data.frame(extract(fit,permuted=TRUE))

check_values <- function(){
estvals <- samples%>%select(starts_with("estval"))%>%gather(optionID,estval)

print(
    ggplot(estvals,aes(x=estval,color=optionID))+geom_density()+
    geom_vline(data=stimvalues.df,aes(xintercept=value,color=paste0("estval.",option)))+ggtitle("est vs true values")
    )
}
check_features <- function(){
features <- samples%>%select(starts_with("features."))%>%gather(id,est)%>%
    mutate(option=sapply(id,function(x){as.numeric(substr(x,10,10))}),
           feature=sapply(id,function(x){as.numeric(substr(x,12,12))}))

print(ggplot(features,aes(x=est,color=as.factor(option)))+geom_density()+
    facet_wrap(.~feature)+
    geom_vline(data=stim.df,aes(xintercept=value,color=as.factor(option)))+ggtitle("feature values est vs truth"))
}

check_ordobs <- function(){
truediffs.df <- data.frame()
for(atrial in 1:n_trials){
    for(afeature in 1:n_features){
        for(apair in with(ordobs.df,unique(paste(opt1,opt2)))){
            o1 <- as.numeric(substr(apair,1,1))
            v1 <- stim.df%>%filter(trial==atrial,feature==afeature,option==o1)%>%select(value)%>%as.numeric
            o2 <- as.numeric(substr(apair,3,3))
            v2 <- stim.df%>%filter(trial==atrial,feature==afeature,option==o2)%>%select(value)%>%as.numeric
            truediffs.df <- rbind(truediffs.df,
                                  data.frame(trial=atrial,feature=afeature,o1=o1,o2=o2,diff=v1-v2))
        }
    }
}

print(ggplot(ordobs.df,aes(color=paste(opt1,opt2),x=obs))+geom_density()+facet_wrap(.~feature)+
    geom_vline(data=truediffs.df,aes(xintercept=diff,color=paste(o1,o2)))+ggtitle("OBSERVED vs true differences")
    )
}

#check_values()
#check_features()
#check_ordobs()

expectation <- samples%>%select(starts_with("estval"))%>%gather(id,value)%>%group_by(id)%>%summarize_all(mean)%>%arrange(desc(value))

bestchoice <- expectation$id[1]%>%as.character%>%substr(.,8,8)%>%as.numeric
winmargin <- expectation$value[1]-expectation$value[2]

p_mindchange <-samples%>%select(expectation$id[1:2])
p_mindchange$shouldchange=p_mindchange[,2]>p_mindchange[,1]
p_mindchange$earnings <- p_mindchange[,2]-p_mindchange[,1]

sum(p_mindchange$shouldchange)/nrow(p_mindchange) #best guess at p(change)
#expected value of changing your mind * chance you will change your mind. Does this make sense as the value of further information?
mean(p_mindchange$earnings[p_mindchange$shouldchange])*(sum(p_mindchange$shouldchange)/nrow(p_mindchange))
#That's the value of perfect information though? The value of "one more observation" is somewhat less than that? (And different for the different types of information?)

