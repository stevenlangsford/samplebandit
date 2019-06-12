library(tidyverse)
library(patchwork)
theme_set(theme_light())

set.seed(4)
rm(list = ls())
##Basic idea:
##Only interested in 'instructable' policies
##ie ones with neat English descriptions

##Reduce trials to 'states' with neat descriptions.
##Reduce actions on states to a small set with neat descriptions

##Get the value of each action in each state (given an environment)

##Collapse state descriptions as much as possible.
##target obs using the other feature: max, min, unknown
##max-or-min-or-unknown == any
##max-or-min == any known

##Part One: trials -> states
##Sort trial options into a consistent order by feature-info
##(doesn't matter how, just to erase option-position)
##Switch feature values for 'low-mid-high/NA' labels
##You are now in one of 4^6 states

##Allow custom sort: sort on prob first then payoff, knowns first unknowns last.
##Turn NA into a string, numerals are '<' strings in alphanumeric order.
'[.gamble' <- function(x, i){
    class(x) <- "list"
    x <- x[i]
    class(x) <- "gamble"
    x
}
'>.gamble' <- function(a, b){
    a <- a[[1]]
    b <- b[[1]]
    if (is.na(a[1]))a[1] <- "missing"
    if (is.na(a[2]))a[2] <- "missing"
    if (is.na(b[1]))b[1] <- "missing"
    if (is.na(b[2]))b[2] <- "missing"

    if (a[1] == "missing" && b[1] == "missing"){
        return(a[2] > b[2])
    }else{
        return(a[1] > b[1])
    }
}

'<.gamble' <- function(a, b){
    a <- a[[1]]
    b <- b[[1]]

    if (is.na(a[1]))a[1] <- "missing"
    if (is.na(a[2]))a[2] <- "missing"
    if (is.na(b[1]))b[1] <- "missing"
    if (is.na(b[2]))b[2] <- "missing"

    if (a[1] == "missing" && b[1] == "missing"){
        return(a[2] < b[2])
    }else{
        return(a[1] < b[1])
    }
}
'==.gamble' <- function(a, b){
    a <- a[[1]]
    b <- b[[1]]
    if (is.na(a[1]))a[1] <- "missing"
    if (is.na(a[2]))a[2] <- "missing"
    if (is.na(b[1]))b[1] <- "missing"
    if (is.na(b[2]))b[2] <- "missing"

    return(a[1] == b[1] &&
           a[2] == b[2])
}

##Feature values collapse to status-labels
prob_statelabel <- function(aprob){
    if (is.na(aprob))return("NA")
    if (aprob <= 0.4) return("low")
    if (aprob >= 0.6) return("high")
    return("med")
    }
payoff_statelabel <- function(apay){
    if (is.na(apay))return("NA")
    if (apay <= 13) return("low")
    if (apay >= 27) return("high")
    return("med")
}

getstate <- function(atrial){
    atrial <- sort(atrial) ##erase location, only care about info state
    paste(sapply(atrial, function(x){
        prob_statelabel(x[1])
    }),
    sapply(atrial, function(x){
        payoff_statelabel(x[2])
    }),
    collapse = ":"
    )
}

##Part Two: Actions
##obs_P/obs_V/stop_choose
##obs can be: Highest, Lowest, Unknown (in other feature, of course)

make_obs <- function(ground_truth,
                     agent_view,
                     to_observe = c("prob", "payoff")[1],
                     altfeature_preference = c("min", "max", "NA")
                     ){
    obs_index <- ifelse(to_observe == "prob", 1, 2)
    ref_index <- ifelse(to_observe == "prob", 2, 1)
    minmaxbar <- ifelse(altfeature_preference == "min", Inf, -Inf)
    candidate <- c()
    
     #for each option:
    for (i in 1:3){
        if (!is.na(agent_view[[i]][obs_index]))next #to-observe targ unknown
        if (altfeature_preference == "NA"){
            if (!is.na(agent_view[[i]][ref_index])){
                next
            }else{
                candidate <- c(candidate, i)
            }
        }
        if (altfeature_preference == "min"){
            if (is.na(agent_view[[i]][ref_index]))next
            if (agent_view[[i]][ref_index] == minmaxbar){
                canidate <- c(candidate, i)
            }
            if (agent_view[[i]][ref_index] < minmaxbar){
                candidate <- c(i)
                minmaxbar <- agent_view[[i]][ref_index]
                }
        }#min preference
        if (altfeature_preference == "max"){
            if (is.na(agent_view[[i]][ref_index]))next
            if (agent_view[[i]][ref_index] == minmaxbar){
                canidate <- c(candidate, i)
            }
            if (agent_view[[i]][ref_index] > minmaxbar){
                candidate <- c(i)
                minmaxbar <- agent_view[[i]][ref_index]
                }
        }#max preference
    }#for each option
    
    if (length(candidate) == 0)return(agent_view) #no matching obs
    if (length(candidate) > 1) candidate <- base::sample(candidate, 1)#ties rnd

    #Make the observation:
    agent_view[[candidate]][obs_index] <- ground_truth[[candidate]][obs_index]

    return(agent_view)
}

make_choice <- function(ground_truth,
                     agent_view,
                     decide_on = c("prob", "payoff", "norm_value", "human_value")[1],
                     target = c("max", "NA")[1] # NA good if known opts are bad
                     ){
    if (decide_on == "human_value") stop("TODO")

    values <- unlist(purrr::map(agent_view, prod))
    decide_index <- switch(decide_on,
                           "prob" = 1,
                           "payoff" = 2,
                           "value" = 3)
    
    for (i in 1:3){
        agent_view[[i]][3] <- values[i] #just an extra feature
    }
    
    maxvals <- c(max(sapply(agent_view, function(x){
        x[1]
    }),
    na.rm = TRUE),
    max(sapply(agent_view, function(x){
        x[2]
    }),
    na.rm = TRUE),
    max(values, na.rm = TRUE)
    )


    ##take the first match, but with random search order
    rndorder <- base::sample(1:3, 3)

    for (i in rndorder){
        if (target == "NA"){
            if (is.na(agent_view[[i]][decide_index])){
                anslist <- list(prod(ground_truth[[i]]))
                names(anslist) <- paste0(decide_on, target)
                return(anslist)
            }
        }
        if (target == "max"){
            if (is.na(agent_view[[i]][decide_index]))next
            if (agent_view[[i]][decide_index] == maxvals[decide_index]){
                anslist <- list(prod(ground_truth[[i]]))
                names(anslist) <- paste0(decide_on, target)
                return(anslist)
            }
        }
    }
                                        #washout
     return(list("no_match" = -1))
}


##MAIN
simtruth <- list(c(.5, 10), c(.2, 20), c(.1, 30))
myview <- list(c(NA, NA), c(.2, 20), c(NA, NA))
class(simtruth) <- "gamble"; class(myview) <- "gamble"

##Things you can do:
##Given a view and a ground truth, make an observation according to policy instructions: pick a feature to observe, and prefer the unknown that pairs with min, max, or NA in the *other* feature.
##Given a view and a ground truth, make a choice and get the value back: also according to policy, best-known-value, or max of a known feature type, or NA on either or both features.
##Given a ground truth, categorize it as a particular 'information state', erasing position info and coarsegraining features into high-mid-low.

##TODO: explore which policies in this language are best for each of the "information states".

##From each information state, get expected values of each make-a-choice policy
##observation types are connections between information states

