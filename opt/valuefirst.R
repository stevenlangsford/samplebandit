library(tidyverse)
theme_set(theme_light())
##setup params

##setup helpers
source("goldenline.R")
init <- seen_states[["NA_NA:NA_NA:NA_NA"]]

value_rnd_memo <- hash()
value_rnd <- function(infostate, n_obs){
    if (!is.null(value_rnd_memo[[paste(infostate, n_obs)]])){
        return(value_rnd_memo[[paste(infostate, n_obs)]])
    }
    mystate <- seen_states[[infostate]]
    if (n_obs == 0){
        value_rnd_memo[[paste(infostate, n_obs)]] <- mystate$value_now
        return(as.numeric(mystate$value_now))
    }
    
    possible_obs <- str_count(infostate, "NA")
    my_value <- 0
    for (childindex in 1:length(mystate$children)){
        if (length(mystate$children[[childindex]]) == 0)next; #known feature
        for (stateindex in 1:length(mystate$children[[childindex]]$state)){
            my_value <- my_value +
                (1 / possible_obs) * #chance taking this path (with rnd choice)
                value_rnd(mystate$children[[childindex]]$state[stateindex],
                          n_obs - 1) *
                mystate$children[[childindex]]$prob[stateindex]
        }
    }
    value_rnd_memo[[paste(infostate, n_obs)]] <- my_value
    return(as.numeric(my_value))
}#end value_rnd

value_opt_memo <- hash()
value_opt <- function(infostate, n_obs){
    if (!is.null(value_opt_memo[[paste(infostate, n_obs)]])){
        return(value_opt_memo[[paste(infostate, n_obs)]])
    }
    mystate <- seen_states[[infostate]]
    if (n_obs == 0){
        value_opt_memo[[paste(infostate, n_obs)]] <- mystate$value_now
        return(as.numeric(mystate$value_now))
    }
    
    possible_obs <- 1 #ok to just choose one opt line from the set.

    myaction <- best_action(infostate, n_obs)[1]#tirebreakers all optimal
    if (str_count(myaction, "choose") > 0){
        value_opt_memo[[paste(infostate, n_obs)]] <- mystate$value_now
        return(mystate$value_now)
    }
    my_value <- 0
    for (childindex in 1:length(mystate$children)){
        if (length(mystate$children[[childindex]]) == 0)next; #known feature

        if (mystate$children[[childindex]]$obs_description == myaction){
            for (stateindex in 1:length(mystate$children[[childindex]]$state)){
                my_value <- my_value +
                    (1 / possible_obs) * #chance taking this path (with opt choice)
                    value_opt(mystate$children[[childindex]]$state[stateindex],
                              n_obs - 1) *
                    mystate$children[[childindex]]$prob[stateindex]
            }
            break
        }
    }
    value_opt_memo[[paste(infostate, n_obs)]] <- my_value
    return(as.numeric(my_value))
}


 #'arb' is 'arbitrary subtree' (defined by some instructionset). Unlike rnd and opt, this is stochastic, gives different subtrees each time.
value_arb <- function(infostate, n_obs){
    mystate <- seen_states[[infostate]]
    if (n_obs == 0){
        return(as.numeric(mystate$value_now))
    }

    ##without this random strategies are too horrible
    myaction <- best_action(infostate, n_obs)[1]
    if (str_count(myaction, "choose") > 0){
        return(mystate$value_now)
    }
    possible_obs <- base::sample(1:str_count(infostate, "NA"), 1)
    obs_usable <- possible_obs

#    myaction <- best_action(infostate, n_obs)[1]

    my_value <- 0
    rnd_order <- sample(1:length(mystate$children), length(mystate$children))
    
    for (childindex in rnd_order){
        if (obs_usable == 0) break
        if (length(mystate$children[[childindex]]) == 0)next; #known feature
        obs_usable <- obs_usable - 1
        for (stateindex in 1:length(mystate$children[[childindex]]$state)){
            my_value <- my_value +
                (1 / possible_obs) * #chance taking this path (with arb choice)
                value_arb(mystate$children[[childindex]]$state[stateindex],
                          n_obs - 1) *
                mystate$children[[childindex]]$prob[stateindex]
        }
    }
    return(as.numeric(my_value))
}



##MAIN
value.df <- data.frame()

for (i in 0:6){
    value.df <- rbind(value.df,
                      data.frame(
                          n_obs = i,
                          opt = value_opt(init$mystringid, i),
                          rnd = value_rnd(init$mystringid, i)
                      )
                      )
}

## ggplot(value.df, aes(x = n_obs)) +
##     geom_line(aes(y = opt, color = "opt")) +
##     geom_line(aes(y = rnd, color = "rnd")) +
##     geom_point(aes(y = opt, color = "opt")) +
##     geom_point(aes(y = rnd, color = "rnd")) +
##     geom_line(aes(y = arb, color = "arb"))


arbreps.df <- data.frame()
hm_reps <- 10
for (i in 1:hm_reps){
    for (nobs in 0:6) {
        arbreps.df <- rbind(arbreps.df,
                            data.frame(n_obs = nobs,
                                       value = value_arb(init$mystringid, nobs)
                                       ))
    }
}

arbreps.df$group = rep(1:hm_reps, each = 7)

#ggplot(arbreps.df, aes(x = n_obs,y = value, group = group)) + geom_line()

ggsave(
ggplot(value.df, aes(x = n_obs)) +
    geom_line(aes(y = opt, color = "opt")) +
    geom_line(aes(y = rnd, color = "rnd")) +
    geom_point(aes(y = opt, color = "opt")) +
    geom_point(aes(y = rnd, color = "rnd")) +
    geom_line(data = arbreps.df,
              aes(x = n_obs, y = value, group = group, color="arb"), alpha= .4),
file = "strategy_comparison.png")
