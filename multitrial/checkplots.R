lpad <- function(i,n=3){ #used to name flipbook files so alphabetical order matches numerical order.
    if(nchar(as.character(i))<n){
        return (paste0(paste0(rep(0,n-nchar(as.character(i))),collapse=""),i))
    }
    return(as.character(i));
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

next_obs_plot <- function(samples,calcestobsval,ordestobsval,bestobsstring=""){
    (check_values(samples)+(check_features(samples)+guides(color=FALSE)))/

    (ggplot(calcestobsval,aes(x=option,y=new_evi,color=as.factor(option)))+geom_violin(aes(fill=as.factor(option)),alpha=.1)+geom_point()+facet_wrap(.~feature)+guides(fill=FALSE,color=FALSE)+ylim(c(0,max(calcestobsval$new_evi+.1)))+ggtitle(bestobsstring)+
     
            ggplot(ordestobsval,aes(x=feature,group=feature,y=new_evi))+geom_violin()+geom_point()+facet_grid(opt1~opt2)+ylim(c(0,max(calcestobsval$new_evi+.1))#note calcesotbsval used in ylim: nice to match across plots, dangerous?
        ))}
    #/
     #   (ggplot(infohistory.df,aes(x=n_obs,y=evi))+geom_point()+geom_line()+xlim(c(0,total_obs))+ylim(c(0,infohistory.df$evi[1]+.05)))
    
