library(tidyverse)
library(hash)
rm(list = ls())
theme_set(theme_light())
set.seed(4)

##tuneable settings:
coarsegrain_prob <- function(p){
    if (!is.numeric(p))return("NA")
    if (p < .3) return("low")
    if (p > .7) return("high")
    return ("mid")
}

coarsegrain_payoff <- function(p){
    if (!is.numeric(p))return("NA")
    if (p < 13) return("low")
    if (p > 27) return("high")
    return ("mid")
}

##assumes prob/payoff independence.
##Consider replacing this with get_option or get_trial?
get_prob <- function(n_obs){
    runif(n_obs, 0, 1)
}
get_payoff <- function(n_obs){
    rnorm(n_obs, 20, 7)
}

##Optional: get the implications of these settings. Could set by fiat!
##Set-by-compu-approx makes more sense for complex stim, these are simple.
n_test <- 5000000
test_probs <- get_prob(n_test)
test_payoffs <- get_payoff(n_test)

probtable <- table(sapply(test_probs, coarsegrain_prob))
probtable <- probtable / sum(probtable)
paytable <-  table(sapply(test_payoffs, coarsegrain_payoff))
paytable <- paytable / sum(paytable)

problevels <- c(names(probtable), "NA")
payofflevels <- c(names(paytable), "NA")

prob_navalue <- signif(mean(test_probs), 3)
payoff_navalue <- signif(mean(test_payoffs), 3)
option_navalue <- signif(mean(test_probs * test_payoffs), 3)


##End tuneable settings
allnames <- c()
seen_states <- hash()

##if you just want to deal with info states as strings
##ie the ones returned by 'coarsegrain_x'
infostate_sort <- function(p1_string, p2_string, p3_string,
                         v1_string, v2_string, v3_string){
    paste0(sort(c(paste(p1_string, v1_string, sep = ":"),
                  paste(p2_string, v2_string, sep = ":"),
                  paste(p3_string, v3_string, sep = ":")
                  )
                ),
           collapse = " "
           )
}

##if you want to carry around actual stim values
##making obs, taking value, converting to coarsegrain as necessary...
trial_constructor <- function(p1, p2, p3, v1, v2, v3){
    ret <- list(option1 = list(prob = p1, payoff = v1),
                option2 = list(prob = p2, payoff = v2),
                option3 = list(prob = p3, payoff = v3))
    class(ret) <- "trial"
    return(ret)
}

"toString.trial" <- function(atrial){
    ##sort into "canonical order"
    paste0(sort(
        c(paste(coarsegrain_prob(atrial$option1$prob),
                coarsegrain_payoff(atrial$option1$payoff), sep = ":"),
          paste(coarsegrain_prob(atrial$option2$prob),
                coarsegrain_payoff(atrial$option2$payoff), sep = ":"),
          paste(coarsegrain_prob(atrial$option3$prob),
                coarsegrain_payoff(atrial$option3$payoff), sep = ":")
          )
    ), collapse = "_")
}
##use alphanumeric ordering on canonical order string representation to sort etc
"==.trial" <- function(a, b){
    toString(a) == toString(b)
}
"<.trial" <- function(a, b){
    toString(a) < toString(b)
}
">.trial" <- function(a, b){
    toString(a) > toString(b)
}

get_choice <- function(atrial){
    if (!class(atrial) == "trial"){
       stop(paste("get_choice: ", atrial, "is not class(trial)"))
    }
    mybest <- -Inf
    myindex <- 0
    rndorder <- base::sample(1:3, 3) #take-first in rnd order = rnd tiebreaks
    for (i in rndorder){
        if (is.na(atrial[[i]]$prob) &&
           is.na(atrial[[i]]$payoff)){
            myval <- option_navalue
            if (myval > mybest){
                mybest <- myval
                myindex <- i
            }
            next
        }#end both na
        if (is.na(atrial[[i]]$prob)){
            myval <- prob_navalue * atrial[[i]]$payoff
            if (myval > mybest){
                mybest <- myval
                myindex <- i
            }
            next
        }#end prob na
        if (is.na(atrial[[i]]$payoff)){
            myval <- payoff_navalue * atrial[[i]]$prob
            if (myval > mybest){
                mybest <- myval
                myindex <- i
            }
            next
        }#end payoff na
        myval <- atrial[[i]]$prob * atrial[[i]]$payoff
        if (myval > mybest){
            mybest <- myval
            myindex <- i
        }
    }#end for each option
    return(list(value = mybest, whichoption = myindex))
}#end choose a trial

possible_observations <- function(infostring){
    infostring <- row.names(bob)[100]
    strsplit(infostring, " ")[[1]] %>%
        map(function(x){
            strsplit(x, ":")[[1]]
        }
        )
    candidates <- list()
    
}

get_transition_matrix <- function(anaction){
for (prob1 in problevels){
    for (prob2 in problevels){
        for (prob3 in problevels){
            for (pay1 in payofflevels){
                for (pay2 in payofflevels){
                    for (pay3 in payofflevels){
                        mystate <- infostate_sort(prob1,
                                                prob2,
                                                prob3,
                                                pay1,
                                                pay2,
                                                pay3)
                        if (is.null(seen_states[[mystate]])){
                            seen_states[[mystate]] <- TRUE
                            allnames <- c(allnames, mystate)
                        }
                    }
                }
            }
        }
    }
}

hm_states <- length(allnames)

return(matrix(rep(NA, hm_states ^ 2),
              nrow = hm_states,
              ncol = hm_states,
              byrow = TRUE,
              dimnames = list(allnames, allnames)
              )
       )
}
