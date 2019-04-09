library(tidyverse)
library(patchwork)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(theme_light())

#rm(list=ls())
set.seed(4)

##stim setup
n_trials <- 1 #current .stan is limited to one trial at a time! must be 1.
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

for(atrial in 1:n_trials){
    for(anoption in 1:n_options){
        stim.df <- rbind(stim.df,data.frame(value=NA,
                                            feature=1:n_features,
                                            option=anoption,
                                            trial=atrial));
    }
}

##SET CUSTOM STIM VALUES HERE
##stim.df:
##   value feature option trial
## 1    NA       1      1     1
## 2    NA       2      1     1
## 3    NA       1      2     1
## 4    NA       2      2     1
## 5    NA       1      3     1
## 6    NA       2      3     1

#options 1 and 2 are the balanced target and alternative (default is UR quad, try changing this? Matters because priors center at 0,0)
targ_alt <- c(0,1,1,0)
delta <- .1
attraction1 <- c(-delta,1-delta)
attraction2 <- c(1-delta,-delta)
compromise <- c(.5,.5)
similarity1 <- c(delta,1-delta)
similarity2 <- c(1-delta,delta)

stim.df$value <- c(targ_alt,compromise)
##Done setting features: get option values.
for(atrial in 1:n_trials){
    for(anoption in 1:n_options){

        myvalue <- 
            stim.df%>%filter(option==anoption,trial==atrial)%>%select(value)%>%summarize_all(sum)%>%as.numeric #Note skipped combineFeatures!
        
        stimvalues.df <- rbind(stimvalues.df,data.frame(trial=atrial,
                                                        option=anoption,
                                                        value=myvalue))
        print(stimvalues.df)
    }
}
print("Note stim setup set the seed.")
