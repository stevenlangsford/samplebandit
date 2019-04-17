library(patchwork)
theme_set(theme_light())
##assuming stim.df is visible, as always? (stimsetup also loads patchwork)
##The numbers in samples$features.X.Y.Z refer to trial,option,feature in that order.
##similarly, estval.X.Y is trial, option.
belief_snapshot <- function(samples,whichtrial){

    mystim=stim.df%>%filter(trial==whichtrial)%>%mutate(feature=paste0("feature.",feature))%>%spread(feature,value)

    mybeliefs <- samples%>%select(starts_with(paste0("features.",whichtrial)))

    option1="option1"#awful hack to put literals inside aes_string. Tears of shame.
    option2="option2"
    option3="option3"
    estalpha=.1

    my_estvals <-
        samples%>%select(starts_with(paste0("estval.",whichtrial)))%>%gather%>%mutate(option=sapply(key,function(x){strsplit(x,"\\.")[[1]][[3]]}))
    my_truevals <- stimvalues.df%>%filter(trial==whichtrial)

    

    expectation <- samples%>%select(starts_with(paste0("estval.",whichtrial)))%>%gather(id,value)%>%group_by(id)%>%summarize_all(mean)%>%arrange(desc(value))
    bestchoice <- (expectation$id[1]%>%as.character%>%strsplit(.,"\\."))[[1]][[3]] #haha. Ouch? What a less ugly way?
    titletext <- paste("Current best:",bestchoice)
    
    if(exists("single_evi")){
        titletext <- paste(titletext,"evi:",signif(single_evi(samples,whichtrial),3))
    }
        
    return(
    (ggplot(mybeliefs)+
        geom_point(aes_string(x=paste0("features.",whichtrial,".1.1"),y=paste0("features.",whichtrial,".1.2"),color="option1"),alpha=estalpha)+
        geom_point(aes_string(x=paste0("features.",whichtrial,".2.1"),y=paste0("features.",whichtrial,".2.2"),color="option2"),alpha=estalpha)+
        geom_point(aes_string(x=paste0("features.",whichtrial,".3.1"),y=paste0("features.",whichtrial,".3.2"),color="option3"),alpha=estalpha)+
     geom_point(data=mystim,aes(x=feature.1,y=feature.2,color=paste0("option",option)),size=5)+
     xlab("feature 1")+
     ylab("feature 2")+
     xlim(c(-4,4))+#Setting limits mean plots align as you flipbook through them... but guessing the limits means they're gonna be bad sometimes.
     ylim(c(-3,5))+
     ggtitle(titletext)
    )/
    (ggplot(my_estvals,aes(x=value,color=paste0("option",option)))+geom_density()+guides(color=FALSE))+xlim(c(-5,5))+
    geom_vline(data=my_truevals,aes(xintercept=value,color=paste0("option",option)))
    )
}
