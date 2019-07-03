library(tidyverse)
theme_set(theme_light())
##sections are: strategy evaluation, strategy definition, main.

##setup helpers
source("goldenline.R")
init <- seen_states[["NA_NA:NA_NA:NA_NA"]]

#strategy evaluation
get_choice_value <- function(anaction){
    #this is byzantine. Repent and promise to code better next time.
    myfeatures <- strsplit(anaction, " ")[[1]][-1]
    myprob <- ifelse(myfeatures[1] == "NA",
                     prob_na_expected,
                     problevel_value[myfeatures[1]]
                     )
    mypay <- ifelse(myfeatures[2] == "NA",
                    pay_na_expected,
                    paylevel_value[myfeatures[2]]
                    )
    return(myprob * mypay)
}

value_strategy <- function(n_obs, actionselectionfn){
    #assumes seen_states and init in env
    value_strategy_memo <- hash() #personal memoizer, resets each time.
value_strategy_worker <- function(infostate, n_obs, actionselectionfn){
    ##actionselectionfn should take args (infostate, n_obs)
    ##returns a string matching obs_description (eg "p NA highpay").
    ##'best_action' is an example (gives optimal returns)
    if (!is.null(value_strategy_memo[[paste(infostate, n_obs)]])){
        return(value_strategy_memo[[paste(infostate, n_obs)]])
    }
    mystate <- seen_states[[infostate]]
    if (n_obs == 0){
        value_strategy_memo[[paste(infostate, n_obs)]] <- mystate$value_now
        return(as.numeric(mystate$value_now))
    }

    myactions <- actionselectionfn(infostate, n_obs)
    possible_obs <- length(myactions)

    my_value <- 0

    for (myaction in myactions){
        ##if a choice action is available, always take it.
        if (str_count(myaction, "choose") > 0){
            my_value <- my_value + get_choice_value(myaction) * (1 / possible_obs)
        }
        for (childindex in 1:length(mystate$children)){
            if (length(mystate$children[[childindex]]) == 0)next; #known feature: no children.

            if (mystate$children[[childindex]]$obs_description == myaction){
                for (stateindex in 1:length(mystate$children[[childindex]]$state)){
                    my_value <- my_value +
                        (1 / possible_obs) * #chance taking this path (with strategy choice)
                        value_strategy_worker(mystate$children[[childindex]]$state[stateindex],
                                       n_obs - 1, actionselectionfn) *
                        mystate$children[[childindex]]$prob[stateindex]
                }
                break
            }
        }
    }
    if (my_value == 0)browser()
    value_strategy_memo[[paste(infostate, n_obs)]] <- my_value
    return(as.numeric(my_value))
}
    ret <- value_strategy_worker(init$mystringid, n_obs, actionselectionfn)
    return(ret)
}

##strategy definition
##todo: move best_action here?
all_actions <- function(myinfostring, n_obs){
    ##ignores n_obs, returns all legal actions on this infostring
    ##ie 'observe at random', anything goes.
    return(
        unique(unlist(map(seen_states[[myinfostring]]$children, function(x){
            x$obs_description
        })))
    )
}

prob_first <- function(myinfostring, n_obs){
    available_obs <-
        unique(
            unlist(
                map(seen_states[[myinfostring]]$children, function(x){
                    x$obs_description
                })))
    
    first_pref <- c()
    second_pref <- c()

    check_for_choice <- best_action(myinfostring,n_obs)
    for (anaction in check_for_choice){
        if (substr(anaction, 1, 1) == "c"){
            first_pref <- c(first_pref, anaction)
        }
    }
    for (anaction in available_obs){
        if (substr(anaction, 1, 1) == "p"){
            first_pref <- c(first_pref, anaction)
        }else{
            second_pref <- c(second_pref, anaction)
        }

    }
    if (length(first_pref) > 0) return(first_pref)
    if (length(second_pref) > 0) return(second_pref)
    warning(paste("prob_first washout", myinfostring, n_obs))#ever happens?
    return(available_obs)
}

pay_first <- function(myinfostring, n_obs){
    available_obs <-
        unique(
            unlist(
                map(seen_states[[myinfostring]]$children, function(x){
                    x$obs_description
                })))
    
    first_pref <- c()
    second_pref <- c()

    check_for_choice <- best_action(myinfostring,n_obs)
    for (anaction in check_for_choice){
        if (substr(anaction, 1, 1) == "c"){
            first_pref <- c(first_pref, anaction)
        }
    }
    for (anaction in available_obs){
        if (substr(anaction, 1, 1) == "v"){
            first_pref <- c(first_pref, anaction)
        }else{
            second_pref <- c(second_pref, anaction)
        }

    }
    if (length(first_pref) > 0) return(first_pref)
    if (length(second_pref) > 0) return(second_pref)
    warning(paste("pay_first washout", myinfostring, n_obs))#ever happens?
    return(available_obs)
}

option_first <- function(myinfostring, n_obs){
    available_obs <-
        unique(
            unlist(
                map(seen_states[[myinfostring]]$children, function(x){
                    x$obs_description
                })))
    
    first_pref <- c()
    second_pref <- c()

    check_for_choice <- best_action(myinfostring, n_obs)

    for (anaction in check_for_choice){
        if (substr(anaction, 1, 1) == "c"){
            first_pref <- c(first_pref, anaction)
        }
    }
    for (anaction in available_obs){
        explode <- str_split(anaction, " ")[[1]]
        if (explode[2] != "NA" || explode[3] != "NA"){
            first_pref <- c(first_pref, anaction)
        }else{
            second_pref <- c(second_pref, anaction)
        }

    }
    print(first_pref)
    print(second_pref)
    print("**")
    if (length(first_pref) > 0) return(base::sample(first_pref, length(first_pref)))
    if (length(second_pref) > 0) return(base::sample(second_pref, length(second_pref)))
    warning(paste("option_first washout", myinfostring, n_obs))#ever happens?
    return(available_obs)
}


##MAIN
value.df <- data.frame()
for (i in 0:6){
    value.df <- rbind(value.df,
                      data.frame(
                          n_obs = i,
                          value_opt = value_strategy(n_obs = i, best_action),
                          value_rnd = value_strategy(n_obs = i, all_actions),
                          value_probfirst = value_strategy(n_obs = i, prob_first),
                          value_payfirst = value_strategy(n_obs = i, pay_first),
                          value_optionfirst = value_strategy(n_obs = i, option_first)
                      )
                      )
}

value.df
ggsave(
ggplot(value.df, aes(x = n_obs)) +
    geom_line(aes(y = value_opt, color = "opt")) +
    geom_line(aes(y = value_rnd, color = "rnd")) +
    geom_line(aes(y = value_probfirst, color = "probfirst")) +
    geom_line(aes(y = value_payfirst, color = "payfirst")) +
    geom_line(aes(y = value_payfirst, color = "optionfirst"))
,
file = "simple_strategies.png")
