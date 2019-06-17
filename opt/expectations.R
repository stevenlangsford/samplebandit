library(tidyverse)
library(patchwork)
library(hash)

library(basetheme) ##trying it out.
basetheme("clean")
#theme_set(theme_light())
rm(list = ls())

prob_levels <- c("lowprob", "midprob", "highprob")
problevel_prob <- c(.4, .2, .4)
problevel_value <- c(.25, .5, .75) #What to sub in if you see this label?

payoff_levels <- c("lowpay", "midpay", "highpay")
paylevel_prob <- c(.1, .8, .1)
paylevel_value <- c(13, 20, 27)
prob_na_expected <- .5
pay_na_expected <- 20
value_na_na <- 10

##end setup
if (length(prob_levels) != length(problevel_prob) ||
    length(payoff_levels) != length(paylevel_prob) ||
    sum(problevel_prob) != 1 ||
    sum(paylevel_prob) != 1)stop("bad setup")

##easy reference
names(problevel_prob) <- prob_levels
names(paylevel_prob) <- payoff_levels
names(problevel_value) <- prob_levels
names(paylevel_value) <- payoff_levels

##memoize recursive constructor.
seen_states <- hash()

##erase position info by id-ing everything under a canonical sorted order.
infostring <- function(p1, v1, p2, v2, p3, v3){
    paste0(sort(c(paste(p1, v1, sep = "_"),
                  paste(p2, v2, sep = "_"),
                  paste(p3, v3, sep = "_"))),
           collapse = ":"
           )
}

infostate_constructor <- function(p1, p2, p3, v1, v2, v3){
    ##Assuming p & v are level-labels. One day they might be numeric args!
    ##in which case put the numbers here and swap in the coarsegrain label.
    ##numeric used to calc choice values, coarse labels generate obs children
    p1_numeric <- ifelse(is.na(p1), prob_na_expected, problevel_value[p1])
    p2_numeric <- ifelse(is.na(p2), prob_na_expected, problevel_value[p2])
    p3_numeric <- ifelse(is.na(p3), prob_na_expected, problevel_value[p3])
    v1_numeric <- ifelse(is.na(v1), pay_na_expected, paylevel_value[v1])
    v2_numeric <- ifelse(is.na(v2), pay_na_expected, paylevel_value[v2])
    v3_numeric <- ifelse(is.na(v3), pay_na_expected, paylevel_value[v3])
    
    ##My features in 1,2,3 order
    myfeatures <- list(c(p1, v1),
                       c(p2, v2),
                       c(p3, v3)
                       )
    ##My infostate string
    mystringid <- infostring(p1, v1, p2, v2, p3, v3)

    ##IF mystringid exists in hash, just use the memo version
    if (!is.null(seen_states[[mystringid]]))return(seen_states[[mystringid]]);

    ##My children on obs1, obs2, obs3 etc
    reachable_id <- c()
    reachable_probability <- c()
    reach_obs <- c()
    ##this is some bad copy-paste. How to collapse it?
    if (is.na(p1)){
        for (alevel in prob_levels){
            candidate <- infostate_constructor(
                alevel, p2, p3,
                v1, v2, v3
            )$mystringid
            if (!(candidate %in% reachable_id)){
            reachable_id <- c(reachable_id, candidate)
            reachable_probability <- c(reachable_probability,
                                       problevel_prob[alevel])
            reach_obs <- c(reach_obs, paste("[", p1, "]", v1))
            }
        }
    }
    if (is.na(p2)){
        for (alevel in prob_levels){
            candidate <- infostate_constructor(
                                  p1, alevel, p3,
                                  v1, v2, v3
            )$mystringid
            if (!(candidate %in% reachable_id)){
            reachable_id <- c(reachable_id, candidate)
            reachable_probability <- c(reachable_probability,
                                       problevel_prob[alevel])
            reach_obs <- c(reach_obs, paste("[", p2, "]", v2))
            }
        }
    }
    if (is.na(p3)){
        for (alevel in prob_levels){
            candidate <- infostate_constructor(
                                  p1, p2, alevel,
                                  v1, v2, v3
            )$mystringid
            if (!(candidate %in% reachable_id)){
            reachable_id <- c(reachable_id, candidate)
            reachable_probability <- c(reachable_probability,
                                       problevel_prob[alevel])
            reach_obs <- c(reach_obs, paste("[", p3, "]", v3))
            }
        }
    }
    ##values
    if (is.na(v1)){
        for (alevel in payoff_levels){
            candidate <- infostate_constructor(
                                  p1, p2, p3,
                                  alevel, v2, v3
            )$mystringid
            if (!(candidate %in% reachable_id)){
            reachable_id <- c(reachable_id, candidate)
            reachable_probability <- c(reachable_probability,
                                       paylevel_prob[alevel])
            reach_obs <- c(reach_obs, paste(p1, "[", v1, "]"))
            }
        }
    }
    if (is.na(v2)){
        for (alevel in payoff_levels){
            candidate <- infostate_constructor(
                                  p1, p2, p3,
                                  v1, alevel, v3
            )$mystringid
            if (!(candidate %in% reachable_id)){
            reachable_id <- c(reachable_id, candidate)
            reachable_probability <- c(reachable_probability,
                                       paylevel_prob[alevel])
            reach_obs <- c(reach_obs, paste(p2, "[", v2, "]"))
            }
        }
    }
    if (is.na(v3)){
        for (alevel in payoff_levels){
            candidate <- infostate_constructor(
                                  p1, p2, p3,
                                  v1, v2, alevel
            )$mystringid
            if (!(candidate %in% reachable_id)){
            reachable_id <- c(reachable_id, candidate)
            reachable_probability <- c(reachable_probability,
                                       paylevel_prob[alevel])
            reach_obs <- c(reach_obs, paste(p3, "[", v3, "]"))
            }
        }
    }

    ##My expected value on choose1, choose2, choose3
    ##assuming p and v are level labels:
    
    choose1_value <- ifelse( is.na(p1) && is.na(v1),
                            value_na_na,
                            p1_numeric * v1_numeric)
    choose2_value <- ifelse( is.na(p2) && is.na(v2),
                            value_na_na,
                            p2_numeric * v2_numeric)
    choose3_value <- ifelse( is.na(p3) && is.na(v3),
                            value_na_na,
                            p3_numeric * v3_numeric)
    myvalue <- max(choose1_value, choose2_value, choose3_value)

    bestchoice_index <- which(c(choose1_value,
                                choose2_value,
                                choose3_value) == myvalue)
    if (length(bestchoice_index) > 1){
        bestchoice_index <- base::sample(bestchoice_index, 1)
    }

    mychoice_features <- c(paste(p1, v1),
                           paste(p2, v2),
                           paste(p3, v3))[bestchoice_index]
    bestobs <- NA
    obs_expectedvalue <- list()
    
    if (str_count(mystringid, "NA") > 0){
    obs_rawvalue <- unlist(map2(reachable_id, reachable_probability,
                             function(x, y){
                                 seen_states[[x]]$myvalue * y
                             }))



    for (anobs in unique(reach_obs)){
        obs_expectedvalue[[anobs]] <- sum(obs_rawvalue[reach_obs == anobs])
    }

    bestobs <- which(obs_expectedvalue == max(unlist(obs_expectedvalue)))
    if (length(bestobs) > 1){
        bestobs <- base::sample(
                             which(obs_expectedvalue ==
                                 max(unlist(obs_expectedvalue))),
                             1)
        }
    bestobs <- names(obs_expectedvalue)[which.max(obs_expectedvalue)]
    }
#    if (runif(1, 0, 1)>.98)browser()
    ##Done. Return everything you might want to know about this state as a list
    ##Access mainly by canonical name mystringid in seen_states?
    ret <- list(myfeatures = myfeatures,
                mystringid = mystringid,
                mychildren = reachable_id,
                childprobs = reachable_probability,
                myvalue = myvalue,
                bestchoice_features = mychoice_features,
                bestobs_features = bestobs,
                available_obs = reach_obs,
                obs_expectedvalue = obs_expectedvalue)
    seen_states[[mystringid]] <- ret;
    return(ret)
}

#populate seen_states with everything
infostate_constructor(NA, NA, NA, NA, NA, NA)

obs_candidates <- hash()

tc <- sapply(seen_states, function(x){
    available <- x$available_obs
    value <- x$obs_expectedvalue
    mymax <- max(unlist(value))
    print(available)
    if (length(available) == 0)return()
    for (a in available){

        if (is.null(obs_candidates[[a]])){
            obs_candidates[[a]] <<- list(available = 0, isbest = 0)
        }
        obs_candidates[[a]]$available <<- obs_candidates[[a]]$available + 1
        if (value[a] == mymax){
            obs_candidates[[a]]$isbest <<- obs_candidates[[a]]$isbest + 1
        }
    }
})

##Not very useful.
expectation.df <- data.frame()
seen_candidates <- hash()
##Not sure why sapply iterates nicely for hash where walk and for don't?
##dummy var assignment for the sake of side effects seems very sinful.
tc <- sapply(seen_states, function(x){
    for (acandidate in x$available_obs){

    }
    expectation.df <<- rbind(expectation.df,
                             data.frame(
                                 id = x$mystringid,
                                 value = x$myvalue,
                                 choicefeatures = x$bestchoice_features,
                                 hm_obs = 6 - str_count(x$mystringid, "NA"),
                                 bestobs = x$bestobs_features
                             ))
})
## ##true but not very useful:
## ggplot(expectation.df, aes(x = choicefeatures, y = value)) +
##     geom_jitter() +
##     facet_grid(hm_obs~.)


## ggplot(expectation.df, aes(x = bestobs)) +
##     geom_bar() +
##     facet_grid(hm_obs~., scales = "free_y")
#table(expectation.df$bestobs) #for NA_NA, always observe prob.
bob <- seen_states[[names(seen_states)[50]]]
