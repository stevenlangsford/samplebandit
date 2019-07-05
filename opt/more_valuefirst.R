library(tidyverse)
theme_set(theme_light())
##sections are: strategy evaluation, strategy definition, main.

##setup helpers
source("goldenline.R")
init <- seen_states[["NA_NA:NA_NA:NA_NA"]]

##strategy evaluation
get_choice_value <- function(anaction){
    ##this is byzantine. Repent and promise to code better next time.
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
    ##assumes seen_states and init in env
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
        if (is.na(my_value) || my_value  == 0)browser()
        value_strategy_memo[[paste(infostate, n_obs)]] <- my_value
        return(as.numeric(my_value))
    }
    ret <- value_strategy_worker(init$mystringid, n_obs, actionselectionfn)
    return(ret)
}

##strategy definition
##todo: move best_action here?

##complexity tier 0?
all_actions <- function(myinfostring, n_obs){
    ##ignores n_obs, returns all legal actions on this infostring
    ##ie 'observe at random', anything goes.
    return(
        unique(unlist(map(seen_states[[myinfostring]]$children, function(x){
            x$obs_description
        })))
    )
}

##complexity tier 1?
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
    if (length(first_pref) > 0) return(base::sample(first_pref, length(first_pref)))
    if (length(second_pref) > 0) return(base::sample(second_pref, length(second_pref)))
    warning(paste("option_first washout", myinfostring, n_obs))#ever happens?
    return(available_obs)
}

##complexity tier 2?

##conditional_action_getter <- function(conditionfn, preferenceifmet)
## possible_obstarget <- c()
## for (problevel in c(prob_levels, "NA")){
##     for (payofflevel in c(payoff_levels, "NA")){
##         if (problevel == "NA"){
##             possible_obstarget <- c(possible_obstarget,
##                                     paste("p", problevel,
##                                           payofflevel
##                                           )
##                                     )
##         }
##         if (payofflevel == "NA"){
##             possible_obstarget <- c(possible_obstarget,
##                                     paste("v", problevel,
##                                           payofflevel
##                                           )
##                                     )
##         }
##     }
## }

## pwrset <- function(set){
##     n <- length(set)
##     masks <- 2 ^ (1:n - 1)
##     lapply( 1:(2 ^ n - 1), function(u) set[ bitwAnd(u, masks) != 0 ] )
## }

## possible_preferences <- pwrset(possible_obstarget)
## names(possible_preferences) <- paste0("pref", 1:length(possible_preferences))

## count_conditions <- list()
## for (acount in 0:3){
##     for (afeature in c(prob_levels, payoff_levels, "_NA", "NA_")){
##         count_conditions[[length(count_conditions) + 1]] <-
##             function(infostring){
##                 return(str_count(infostring, afeature) == acount)
##             }
##         names(count_conditions)[length(count_conditions)] <-
##             paste(acount, afeature, sep = "of")
##     }
## }
## ## custom_actionrule <- function(myinfostring, n_obs){
## ##     available_obs <-
## ##         unique(
## ##             unlist(
## ##                 map(seen_states[[myinfostring]]$children, function(x){
## ##                     x$obs_description
## ##                 })))
## counting_strategies <- list()
## for (aconditionindex in 1:length(count_conditions)){
##     for (apreferenceindex in 1:length(possible_preferences)){
##         acondition <- count_conditions[[aconditionindex]]
##         conditionname <- names(count_conditions)[aconditionindex]
##         apreference <- possible_preferences[[apreferenceindex]]
##         preferencename <- names(possible_preferences)[apreferenceindex]
        
##         ##This stratmaker thing is to create an environment where acondition and apreference have the bindings you want when the strategy function is called.
##         stratmaker <- function(acondition,
##                                conditionname,
##                                stratpreference,
##                                preferencename){
##             ##If you don't touch stratpreference, you get a bad binding in ret function. (always length 8, fill pref set). But if you print() or assign this length, suddenly bindings are ok again. What the actual f*** is going on here? [PW: probably something to do with the way this fn is defined inside a loop]
##             why_is_this_consequential <- length(stratpreference)
##             ret <-
##                 function (myinfostring, n_obs){
##                     available_obs <-
##                         unique(
##                             unlist(
##                                 map(seen_states[[myinfostring]]$children, function(x){
##                                     x$obs_description
##                                 })))
                    
##                     first_pref <- c()
##                     second_pref <- c()

##                     ##choice actions don't show up in obs_description, so check here
##                     check_for_choice <- best_action(myinfostring, n_obs)
##                     for (anaction in check_for_choice){
##                         if (substr(anaction, 1, 1) == "c"){
##                             first_pref <- c(first_pref, anaction)
##                         }
##                     }
##                     ##This bit does the counting strategy: if the counting condition is met, do one of the preferred actions if possible.

##                     for (anaction in available_obs){
##                         if (acondition(myinfostring) && anaction %in% stratpreference){
##                             first_pref <- c(first_pref, anaction)
##                         }else{
##                             second_pref <- c(second_pref, anaction)
##                         }
##                     }
##                     if (length(first_pref) > 0) return(first_pref)
##                     if (length(second_pref) > 0) return(second_pref)
##                     warning(paste("counting strategy washout", myinfostring, n_obs))#ever happens?
##                     return(available_obs)
##                 }#end strategy function on infostring, n_obs
            
##             return(ret)
##         }#end stratmaker
        
##         ##if(length(apreference)!=8)browser()#ok
        
##         counting_strategies[[length(counting_strategies) + 1]] <-
##             stratmaker(acondition,conditionname,apreference,preferencename)
        
##         names(counting_strategies)[length(counting_strategies)] <- paste0(
##             conditionname,
##             preferencename
##         )
##     }#for each possible preference set
## }#for each condtion in count_conditions

## counting_value.df <- data.frame()

## strat_limit <- 250
## strat_sampler <- base::sample(1:length(counting_strategies), strat_limit)
## for (astrategy in strat_sampler){
##     print(signif(which(strat_sampler == astrategy) / strat_limit, 3))
    
##     for (mynobs in 0:6){
##         counting_value.df <-
##             rbind(counting_value.df,
##                   data.frame(
##                       n_obs = mynobs,
##                       strategy = names(counting_strategies)[astrategy],
##                       value = value_strategy(mynobs,
##                                              counting_strategies[[astrategy]]
##                                              )
##                   ))#end rbind to counting_value
##     }#end for each strategy
## }#end for each n_obs

## ggplot(counting_value.df, aes(x = n_obs, y = value, color = strategy)) +
##     geom_line() +
##     guides(color = FALSE) +
##     ggtitle("counting strategies")


##MAIN
value.df <- data.frame()
for (i in 0:6){
    print(i)
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

##basic strategy plot:
## ggsave(
    ggplot(value.df, aes(x = n_obs)) +
    geom_line(aes(y = value_opt, color = "opt")) +
    geom_line(aes(y = value_rnd, color = "rnd")) +
    geom_line(aes(y = value_probfirst, color = "probfirst")) +
    geom_line(aes(y = value_payfirst, color = "payfirst"))
                                        #+
#    geom_line(aes(y = value_payfirst, color = "optionfirst"))
  ##  ,
  ##   file = "eco_simple_strategies.png")

#ggsave(
## ggplot(value.df, aes(x = n_obs)) +
##     geom_line(data = counting_value.df, aes(x = n_obs,
##                                             y = value,
##                                             group = strategy,
##                                             color = "counting"),  alpha = .1
##               ) +
##     geom_line(aes(y = value_opt, color = "opt")) +
##     geom_line(aes(y = value_rnd, color = "rnd"))
#,
#file = "prefrank_strategies.png")
