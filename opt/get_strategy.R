library(tidyverse)
source("expectations.R")

golden_path <- hash()

obs_budget <- 1

init <- seen_states[["NA_NA:NA_NA:NA_NA"]]



##"Things that are always true"
## obs_candidates <- hash()
## tc <- sapply(seen_states, function(x){
##     available <- x$available_obs
##     mybest <- x$bestobs
##     for (candidate in available){
##         if (is.null(obs_candidates[[candidate]])){
##             obs_candidates[[candidate]] <<- list(seen = 0, best = 0)
##         }
##         obs_candidates[[candidate]]$seen <<-
##             obs_candidates[[candidate]]$seen + 1

##         if (candidate == mybest){
##             obs_candidates[[candidate]]$best <<-
##                 obs_candidates[[candidate]]$best + 1
##         }
##     }
## })



## ##Not very useful.
## expectation.df <- data.frame()
## seen_candidates <- hash()
## ##Not sure why sapply iterates nicely for hash where walk and for don't?
## ##dummy var assignment for the sake of side effects seems very sinful.
## tc <- sapply(seen_states, function(x){
##     for (acandidate in x$available_obs){

##     }
##     expectation.df <<- rbind(expectation.df,
##                              data.frame(
##                                  id = x$mystringid,
##                                  value = x$myvalue,
##                                  choicefeatures = x$bestchoice_features,
##                                  hm_obs = 6 - str_count(x$mystringid, "NA"),
##                                  bestobs = x$bestobs_features
##                              ))
## })
## ##true but not very useful:
## ggplot(expectation.df, aes(x = choicefeatures, y = value)) +
##     geom_jitter() +
##     facet_grid(hm_obs~.)


## ggplot(expectation.df, aes(x = bestobs)) +
##     geom_bar() +
##     facet_grid(hm_obs~., scales = "free_y")
#table(expectation.df$bestobs) #for NA_NA, always observe prob.
