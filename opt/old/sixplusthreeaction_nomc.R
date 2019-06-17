library(tidyverse)
library(patchwork)
theme_set(theme_light())

set.seed(4)
rm(list = ls())

##Tuneable settings:
##Granularity of status-labels
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
##Distribution of feature-values in the world
get_prob <- function(){
    runif(1, 0, 1)
}
get_payoff <- function(){
    rnorm(1, 20, 7)
}

##End tuneable settings.
##Given these tunings, figure out some interesting probs:
prob_examples <- replicate(500000, get_prob())
payoff_examples <- replicate(500000, get_payoff())

expected_prob <- mean(prob_examples)
expected_payoff <- mean(payoff_examples)
expected_problabel <- table(sapply(prob_examples, prob_statelabel))
expected_problabel <- expected_problable / sum(expected_problable)
expected_payofflabel <- table(sapply(payoff_examples, payoff_statelabel))
expected_payofflabel <- expected_payofflabel / sum(expected_payofflabel)

double_na_value <- mean(prob_examples * payoff_examples)

##Helper functions: enforce format conventions, define actions/policies.
get_stim <- function(probs = c(NA, NA, NA),
                     payoffs = c(NA, NA, NA)
                     ){
    for (i in 1:length(probs)){
        if (is.na(probs[i]))probs[i] <- get_prob()
    }
    for (i in 1:length(payoffs)){
        if (is.na(payoffs[i]))payoffs[i] <- get_payoff()
    }
    
    mystim <- list(c(probs[1], payoffs[1]),
                   c(probs[2], payoffs[2]),
                   c(probs[3], payoffs[3]))
    class(mystim) <- "gamble"
    return(mystim)
}
get_view <- function(){
    ##format is (prob,payoff,position)
    ##infostate sorting only uses prob,payoff
    ##position comes along for the ride.
    myview <- list(c(NA, NA, 1),
                   c(NA, NA, 2),
                   c(NA, NA, 3)
                   )
    class(myview) <- "gamble"
    return(myview)
}
##Allow custom sort: sort on prob first then payoff, knowns first unknowns last.
##Turn NA into a string, numerals are "<" strings in alphanumeric order.
"[.gamble" <- function(x, i){
    class(x) <- "list"
    x <- x[i]
    class(x) <- "gamble"
    x
}
">.gamble" <- function(a, b){
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

"<.gamble" <- function(a, b){
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
"==.gamble" <- function(a, b){
    a <- a[[1]]
    b <- b[[1]]
    if (is.na(a[1]))a[1] <- "missing"
    if (is.na(a[2]))a[2] <- "missing"
    if (is.na(b[1]))b[1] <- "missing"
    if (is.na(b[2]))b[2] <- "missing"

    return(a[1] == b[1] &&
           a[2] == b[2])
}

get_state <- function(atrial){
    if (class(atrial) != "gamble"){
        stop(paste(toString(atrial), "is not class gamble"))
    }
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

observe <- function(truth, viewstate, whichoption, whichfeature){
    ##viewstate and truth are class 'gamble'.
    ##viewstate has to carry position info to ref into truth:
    ##whichoption refers to the sorted (position-blind) viewstate,
    ##check the extra 'position' feature to see which truth val this is
    if (class(truth) != "gamble" || class(viewstate) != "gamble"){
        stop(paste(toString(truth),
                   toString(viewstate),
                   whichfeature,
                   whichoption,
                   "not gambles, sort fails."))
    }

    viewstate[[whichoption]][whichfeature] <-
        truth[[viewstate[[whichoption]][3]]][whichfeature]

    return(sort(viewstate))
}

choose <- function(truth, viewstate, position){
    ##Position should refer to sorted viewstate position.
    ##Check position feature to see which option that is in truth
    return(prod(truth[[viewstate[[position]][3]]]))
}

##search scheme: take in a simtruth (drawn from the distribution of options)
##explore every possible path of observations on it
##recording the value of stop-and-choose as you go.

juice_a_view <- function(myview){

    mystate <- get_state(myview)

    if (paste(toString(simtruth), mystate) %in% results.df$simstateid){
        ##there are multiple observation paths to each state
        ##for each different ordering of the same obs
        ##you don't care where you came from, just what you know now
        ##so if you've been here before, you're done.
        return()
    }

    #low,mid,high variations
    obs11 <- c()#get_state(observe(simtruth, myview, 1, 1))
    obs12 <- c()#get_state(observe(simtruth, myview, 1, 2))
    obs21 <- c()#get_state(observe(simtruth, myview, 2, 1))
    obs22 <- c()#get_state(observe(simtruth, myview, 2, 2))
    obs31 <- c()#get_state(observe(simtruth, myview, 3, 1))
    obs32 <- c()#get_state(observe(simtruth, myview, 3, 2))

    results.df <<- rbind(results.df,
                         data.frame(
                             simstateid = paste(toString(simtruth), mystate),
                             simtruth = toString(simtruth),
                             viewstate = mystate,
                             value_choose1 = choose(simtruth, myview, 1),
                             value_choose2 = choose(simtruth, myview, 2),
                             value_choose3 = choose(simtruth, myview, 3),
                             view_obs11 = ifelse(obs11 == mystate, "NA", obs11),
                             view_obs12 = ifelse(obs12 == mystate, "NA", obs12),
                             view_obs21 = ifelse(obs21 == mystate, "NA", obs21),
                             view_obs22 = ifelse(obs22 == mystate, "NA", obs22),
                             view_obs31 = ifelse(obs31 == mystate, "NA", obs31),
                             view_obs32 = ifelse(obs32 == mystate, "NA", obs32))
                         )
    for (anoption in 1:3){
        for (afeature in 1:2){
            if (is.na(myview[[anoption]][afeature])
                ){
                juice_a_stim(simtruth,
                             observe(simtruth,
                                     myview,
                                     anoption,
                                     afeature) )
            }
        }
    }
}

##MAIN
results.df <- data.frame()##Values populated by juice_a_stim
for (atrial in 1:500){
##    print(atrial) ##bogan progress bar
    simtruth <- get_stim()
    myview <- get_view()
    juice_a_stim(simtruth, myview)
}

##write.csv(results.df, file = "policy_searchinfo_demo1.csv")
policy.df <- data.frame()
for (myview in unique(results.df$viewstate)){
    reachable.df <- results.df %>%
        filter(viewstate == myview) %>%
        select(starts_with("value_choose")) %>%
        summarize_all(mean) %>%
        mutate(viewstate = myview) %>%
        select(viewstate,
               value_choose1,
               value_choose2,
               value_choose3)#just to move viewstate to front, hah.
    
    nextstates <- results.df %>%
        filter(viewstate == myview) %>%
        select(starts_with("view_obs"))
    for (i in 1:6){
        guessnext <- table(nextstates[, i])
        guessnext <- guessnext[guessnext != 0] / sum(guessnext)
        reachable.df[, names(nextstates)[i]] <- paste(
            paste(names(guessnext), collapse = "_"),
            toString(guessnext), sep = "|")
    }
    policy.df <- rbind(policy.df, reachable.df)
}

write.csv(policy.df, file = "policyinfo.csv", row.names = FALSE)
