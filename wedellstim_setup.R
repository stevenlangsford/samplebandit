library(tidyverse)
library(patchwork)
library(rstan)
library(shinystan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(theme_light())

rm(list=ls())#to remove.
set.seed(4);print("stimsetup set the seed");

##stim setup
n_options <- 3
n_features <- 2

combineFeatures <- function(features){
    return(features[1]*features[2]);#must match features_to_value in .stan model
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

##Wedell stim. Feature 1 is prob, feature 2 is payoff.
##targetA
a1 <- c(.83,12)
a2 <- c(.67,15)
a3 <- c(.5,20)
a4 <- c(.4,25)

##range, frequency, rangefrequency.
Ra1 <- c(.4,20)
Ra2 <- c(.5,18)
Ra3 <- c(.67,13)
Ra4 <- c(.83,10)

Fa1 <- c(.35,25)
Fa2 <- c(.45,20)
Fa3 <- c(.62,15)
Fa4 <- c(.78,12)

RFa1 <- c(.35,20)
RFa2 <- c(.45,18)
RFa3 <- c(.62,13)
RFa4 <- c(.78,10)

##targetB
b1 <- c(.3,33)
b2 <- c(.4,25)
b3 <- c(.5,20)
b4 <- c(.67,15)

##range, frequency, rangefrequency.
Rb1 <- c(.25,33)
Rb2 <- c(.35,25)
Rb3 <- c(.45,20)
Rb4 <- c(.62,15)

Fb1 <- c(.3,30)
Fb2 <- c(.4,20)
Fb3 <- c(.5,18)
Fb4 <- c(.67,13)

RFb1 <- c(.25,30)
RFb2 <- c(.35,20)
RFb3 <- c(.45,18)
RFb4 <- c(.62,13)

                                        #OK so what are the actual combinations of these things?

stop("y'aint done yet");


##Done setting features: get option values.
for(atrial in 1:max(stim.df$trial)){
    for(anoption in 1:n_options){
        myfeatures <- 
            stim.df%>%filter(option==anoption,trial==atrial)%>%arrange(feature)%>%select(value)%>%as.data.frame
        myvalue <- combineFeatures(myfeatures[,1])
        
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

rm(list=setdiff(ls(),c("stim.df","stimvalues.df","widestim.df","combineFeatures")))
