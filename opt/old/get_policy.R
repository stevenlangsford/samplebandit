library(tidyverse)
library(hash)
rm(list = ls())

policy.df <- read.csv("policyinfo.csv", stringsAsFactors = FALSE)

seen_states <- hash()

recover_transitions <- function(astring){
astring <- strsplit(astring, "|", fixed = TRUE)
states <- astring[[1]][1] %>% strsplit("_") %>% .[[1]]
probs <-  astring[[1]][2] %>%
    strsplit(",") %>%
    .[[1]] %>%
    as.numeric
return(list(states, probs))
}

obs_value <- function(currentstate, obs_remaining){
available_obs <-
    policy.df %>%
    filter(viewstate == currentstate) %>%
    select(starts_with("view_obs")) %>%
    .[, . != "NA|1"]
#if (obs_remaining == 1)browser()
if (length(available_obs) == 0 || obs_remaining == 0){
    myoptions <- policy.df %>%
        filter(viewstate == currentstate) %>%
        select(starts_with("value"))
    ##silly formatting switch is for consistency with other recursion return path
    if (is.null(names(myoptions[which.max(myoptions)])))browser()
    ret <- list(action = names(myoptions[which.max(myoptions)]),
                value = as.numeric(myoptions[which.max(myoptions)]))
    if (is.null(ret$action))browser()
    return(ret)
} else {
    transitions <- map(available_obs, recover_transitions)#HERE'S THE PROBLEM
    bestvalue <- -Inf
    bestobs <- "none"
    for (i in 1:length(transitions)){
        myvalue <- sum(
            unlist(
                map(transitions[[i]][[1]], function(x){
                    ##Save state values as you go to avoid unnecessary recursion
                    ##State value might depend on the obs budget though,
                    ##so same info, different obs-remaining are distinct?
                    if (is.null(seen_states[[paste0(x, (obs_remaining - 1))]])){
                        myvalue <- obs_value(x, obs_remaining - 1)$value
                        seen_states[[paste0(x, (obs_remaining - 1))]] <- myvalue
                        return(myvalue)
                    }else{
                        return(seen_states[[paste0(x, (obs_remaining - 1))]])
                    }
            }
            )
            ) * transitions[[i]][[2]])
        if (myvalue > bestvalue){
            bestvalue <-  myvalue
            bestobs <- names(transitions)[i]
        }
    }
    ret <- list(action = bestobs, value = bestvalue)
    if (!is.null(ret$action))browser()
    return(ret)
    }
}

#Max depth test
##obs_value(currentstate = policy.df$viewstate[1], obs_remaining = 6)
obs_value(currentstate = policy.df$viewstate[13], obs_remaining = 1)
