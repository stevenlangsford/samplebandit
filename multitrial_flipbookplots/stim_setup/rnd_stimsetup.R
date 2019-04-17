library(tidyverse)
library(patchwork)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(theme_light())

#rm(list=ls())
set.seed(123)

##stim setup
n_trials <- 3
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
