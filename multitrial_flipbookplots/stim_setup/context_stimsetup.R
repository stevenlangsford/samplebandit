library(tidyverse)
library(patchwork)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(theme_light())


set.seed(4)

##stim setup
n_options <- 3
n_features <- 2

##ppnt setup: stan is 'aware' of these, so they should match.
calcobsnoise <- 0.5
ordobsnoise <- 0.5 #Should be higher than calcnoise? By how much? Or possibly an err rate on a discrete var, howes style, if you like pain.

## getOption <- function(){
##     rnorm(n_features,0,1) #Fun to split up into different distributions for different features?
## }

combineFeatures <- function(features){
    return(features[1]+features[2]);#must match features_to_value in .stan model
}

stim.df <- data.frame()
stimvalues.df <- data.frame();

add_stim <- function(targ,alt,decoy){   
    mystim.df <- data.frame()
    for(anoption in 1:n_options){
        mystim.df <- rbind(mystim.df,data.frame(value=NA,
                                feature=1:n_features,
                                option=anoption,
                                trial=NA))
    }
    
    mystim.df$value <- c(targ,alt,decoy)
    mystim.df$trial <- ifelse(nrow(stim.df)==0,1,max(stim.df$trial)+1)
    stim.df <<- rbind(stim.df,mystim.df)
}

#options 1 and 2 are the balanced target and alternative (default is UR quad, try changing this? Matters because priors center at 0,0)
targ <- c(0,1)
alt <- c(1,0)
delta <- .2 #displaces the decoy

attraction1 <- c(-delta,1-delta)
attraction2 <- c(1-delta,-delta)
compromise <- c(.5,.5)
similarity1 <- c(delta,1-delta)
similarity2 <- c(1-delta,delta)

add_stim(targ,alt,attraction1)
add_stim(targ,alt,attraction2)
add_stim(targ,alt,compromise)
add_stim(targ,alt,similarity1)
add_stim(targ,alt,similarity2)

add_stim(targ,alt,attraction1)#2nd rep 'cause why not? stim are cheap, right?
add_stim(targ,alt,attraction2)
add_stim(targ,alt,compromise)
add_stim(targ,alt,similarity1)
add_stim(targ,alt,similarity2)


##Done setting features: get option values.
for(atrial in 1:max(stim.df$trial)){
    for(anoption in 1:n_options){
        myvalue <- 
            stim.df%>%filter(option==anoption,trial==atrial)%>%select(value)%>%summarize_all(sum)%>%as.numeric #Note skipped combineFeatures!
        
        stimvalues.df <- rbind(stimvalues.df,data.frame(trial=atrial,
                                                        option=anoption,
                                                        value=myvalue))
    }
}

widestim.df <- data.frame(#Started off with tall format stim.df, but wide format seems nice for reporting decision trajectories? Maybe one of these is dumb, but also maybe both are fine?
    trialID = 1:max(stim.df$trial),
    a1 = sapply(1:max(stim.df$trial),function(i){stim.df%>%filter(trial==i,option==1,feature==1)%>%select(value)%>%as.numeric}),
    a2 = sapply(1:max(stim.df$trial),function(i){stim.df%>%filter(trial==i,option==1,feature==2)%>%select(value)%>%as.numeric}),
    b1 = sapply(1:max(stim.df$trial),function(i){stim.df%>%filter(trial==i,option==2,feature==1)%>%select(value)%>%as.numeric}),
    b2 = sapply(1:max(stim.df$trial),function(i){stim.df%>%filter(trial==i,option==2,feature==2)%>%select(value)%>%as.numeric}),
    c1 = sapply(1:max(stim.df$trial),function(i){stim.df%>%filter(trial==i,option==3,feature==1)%>%select(value)%>%as.numeric}),
    c2 = sapply(1:max(stim.df$trial),function(i){stim.df%>%filter(trial==i,option==3,feature==2)%>%select(value)%>%as.numeric})
)

print("Note stim setup set the seed.")
