library(tidyverse)
rm(list=ls())

traj.df <- read.csv("plots/rtraj_delta_pt2/rtraj_delta_pt2_trajectory.csv")
source("stim_setup/context_stimsetup.R")

for(mytrial in 1:max(stim.df$trial)){

data.df <- traj.df%>%filter(trialID==mytrial)%>%mutate(#this might be a PB for worst code ever written? Do penance.
                                                  avalue=as.numeric(map2(a1,a2,function(a,b){combineFeatures(c(a,b))})),
                                                  bvalue=as.numeric(map2(b1,b2,function(a,b){combineFeatures(c(a,b))})),
                                                  cvalue=as.numeric(map2(c1,c2,function(a,b){combineFeatures(c(a,b))})))


    mystim=stim.df%>%filter(trial==mytrial)%>%mutate(feature=paste0("feature.",feature))%>%spread(feature,value)
    x11();
    print(
(ggplot()+geom_point(data=mystim,aes(x=feature.1,y=feature.2,color=paste0("option",option)),size=5)+guides(color=FALSE))/
(ggplot(data.df,aes(x=timestep))+
    geom_line(aes(y=avalue,color="option1"),size=2,linetype="longdash",alpha=.5)+
    geom_line(aes(y=est_avalue,color="option1"),linetype="dashed")+
    geom_point(aes(y=est_avalue,color="option1"))+
    geom_line(aes(y=bvalue,color="option2"),size=2,linetype="twodash",alpha=.5)+
    geom_line(aes(y=est_bvalue,color="option2"),linetype="dashed")+
    geom_point(aes(y=est_bvalue,color="option2"))+
    geom_line(aes(y=cvalue,color="option3"),size=2,linetype="dotdash",alpha=.5)+
    geom_line(aes(y=est_cvalue,color="option3"),linetype="dashed")+
 geom_point(aes(y=est_cvalue,color="option3")))
)
    }
