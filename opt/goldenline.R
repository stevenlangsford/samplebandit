library(tidyverse)
library(patchwork)
library(hash)

library(basetheme) ##trying it out.
basetheme("clean")
##theme_set(theme_light())
rm(list = ls())

##Order is important (low->high)
##Keep the 'prob' and 'pay' flagsm they're used later in 'get_strategy'.

##"Ecological" environment following Chater 2006 (sort of)
source("ecoworld.R")

##Original three-level split:
## prob_levels <- c("lowprob", "midprob", "highprob")
## problevel_prob <- c(.4, .2, .4)
## problevel_value <- c(.25, .5, .75)

## payoff_levels <- c("lowpay", "midpay", "highpay")
## paylevel_prob <- c(.1, .8, .1)
## paylevel_value <- c(13, 20, 27)
## prob_na_expected <- .5
## pay_na_expected <- 20
## value_na_na <- 10


##n-level split (best if howfine>=5)
## howfine <- 5
## ##uniform(0,1) prob
## prob_levels <- paste0("prob", 1:howfine)
## problevel_prob <- rep(1 / howfine, howfine)
## problevel_value <- seq(from = .01, to = .99, length = howfine)

## ##Normal payoff: N(mupay, sigmapay)
## ##Note even spacing gives bad breakpoints if howfine < 5
## mupay <- 20
## sigmapay <- 7 #wedell-like

## payoff_levels <- paste0("pay", 1:howfine)
## paylevel_pnorm <- sapply(
##     seq(from = mupay - (2 * sigmapay),
##         to = mupay + (2 * sigmapay),
##         length = howfine + 1
##         ),
##     pnorm, mupay, sigmapay)
## paylevel_prob <- c()
## for (i in 2:length(paylevel_pnorm)){
##     paylevel_prob <- c(paylevel_prob,
##                        paylevel_pnorm[i] - paylevel_pnorm[i - 1])
## }
## paylevel_prob <- paylevel_prob / sum(paylevel_prob)

## paylevel_value <- seq(from = mupay - (2 * sigmapay),
##         to = mupay + (2 * sigmapay),
##         length = howfine + 1
##         )[-i]

## prob_na_expected <- .5
## pay_na_expected <- mupay
## value_na_na <- mupay * 0.5


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

option_expectedvalue <- function(myproblevel, mypayofflevel){
    ifelse(is.na(myproblevel),
           prob_na_expected,
           problevel_value[which(prob_levels == myproblevel)]
           ) *
        ifelse(is.na(mypayofflevel),
               pay_na_expected,
               paylevel_value[which(payoff_levels == mypayofflevel)]
               )
}

##erase position info by id-ing everything under a canonical sorted order.
infostring <- function(p1, v1, p2, v2, p3, v3){
    paste0(sort(c(paste(p1, v1, sep = "_"),
                  paste(p2, v2, sep = "_"),
                  paste(p3, v3, sep = "_"))),
           collapse = ":"
           )
}

seen_vwobs <- hash()
value_withobs <- function(myinfostring, n_obs){
    if (!is.null(seen_vwobs[[paste0(myinfostring, n_obs)]])){
        return(seen_vwobs[[paste0(myinfostring, n_obs)]])
    }
    if (n_obs > str_count(myinfostring, "NA")){
        stop(paste(n_obs, "obs on ", myinfostring, "is impossible"))
    }

    if (n_obs == 0) return(seen_states[[myinfostring]]$value_now)

    actionvalues <- map(seen_states[[myinfostring]]$children, function(achild){
        if (length(achild) == 0) return(-Inf)
        ev <- 0
        for (i in 1:length(achild$state)){
            ev <- ev +
                value_withobs(seen_states[[achild$state[i]]]$mystringid,
                              n_obs - 1) *
                as.numeric(achild$prob[i])
        }
        return(ev)
    })

    seen_vwobs[[paste0(myinfostring, n_obs)]] <- max(unlist(actionvalues))
    return(max(unlist(actionvalues)))
}

best_action <- function(myinfostring, n_obs){

    mystate <- seen_states[[myinfostring]]

    choicevalues <- c(
        option_expectedvalue(mystate$myfeatures$p1, mystate$myfeatures$v1),
        option_expectedvalue(mystate$myfeatures$p2, mystate$myfeatures$v2),
        option_expectedvalue(mystate$myfeatures$p3, mystate$myfeatures$v3)
    )
    choicedescription <- c(
        paste(mystate$myfeatures$p1, mystate$myfeatures$v1),
        paste(mystate$myfeatures$p2, mystate$myfeatures$v2),
        paste(mystate$myfeatures$p3, mystate$myfeatures$v3)
    )

    goodchoices <- choicedescription[
        which(choicevalues == max(choicevalues))
    ]

    if (n_obs == 0){
        return(paste("choose:", paste(unique(goodchoices), collapse = " OR ")))
    }#if no more obs left

    obsvalues <- map(mystate$children, function(achild){
        if (length(achild) == 0) return(-Inf)
        ev <- 0
        for (i in 1:length(achild$state)){
            ev <- ev +
                value_withobs(seen_states[[achild$state[i]]]$mystringid,
                              n_obs - 1) *
                as.numeric(achild$prob[i])
            ##without signif pruning you get some bad "equal not ==" failures
            ##from e-15 rounding diffs
        }
        return(signif(ev,6))
    }
    ) %>% unlist

    if (max(obsvalues) == -Inf){
        warning(paste("best_action(",
                      myinfostring, n_obs,
                      ") hit nothing-observable before n_obs ran out."))
        return(paste("choose:", paste(unique(goodchoices), collapse = " OR ")))
    }

    if (max(obsvalues) <= mystate$value_now){
        return(paste("choose:", paste(unique(goodchoices), collapse = " OR ")))
    }

    good_obs <- which(unlist(obsvalues) == max(unlist(obsvalues)))

    goodobs_description <- sapply(names(good_obs), function(targ){
        afeature <- substr(targ, 1, 1)
        anopt <- substr(targ, 2, 2)
        obs_description <- paste(afeature,
                                 paste(mystate$myfeatures[paste0("p", anopt)],
                                       mystate$myfeatures[paste0("v", anopt)])
                                 )
        return(obs_description)
    }
    ) %>% unique
    return(paste(goodobs_description))
}

infostate_constructor <- function(p1, p2, p3, v1, v2, v3){
    mystringid <- infostring(p1, v1, p2, v2, p3, v3)
    if (!is.null(seen_states[[mystringid]]))return(seen_states[[mystringid]])

    children <- list(
        p1 = list(),
        p2 = list(),
        p3 = list(),
        v1 = list(),
        v2 = list(),
        v3 = list()
    )

    for (aproblevel in prob_levels){
        if (is.na(p1)){
            children$p1[["state"]] <-
                c(children$p1[["state"]],
                  infostate_constructor(aproblevel, p2, p3,
                                        v1, v2, v3)$mystringid
                  )
            children$p1[["prob"]] <-
                c(children$p1[["prob"]],
                  problevel_prob[which(prob_levels == aproblevel)]
                  )
            children$p1[["obs_description"]] <-
                paste("p", p1, v1)
        }
        if (is.na(p2)){
            children$p2[["state"]] <-
                c(children$p2[["state"]],
                  infostate_constructor(p1, aproblevel, p3,
                                        v1, v2, v3)$mystringid
                  )
            children$p2[["prob"]] <-
                c(children$p2[["prob"]],
                  problevel_prob[which(prob_levels == aproblevel)]
                  )
            children$p2[["obs_description"]] <-
                paste("p", p2, v2)
        }
        if (is.na(p3)){
            children$p3[["state"]] <-
                c(children$p3[["state"]],
                  infostate_constructor(p1, p2, aproblevel,
                                        v1, v2, v3)$mystringid
                  )
            children$p3[["prob"]] <-
                c(children$p3[["prob"]],
                  problevel_prob[which(prob_levels == aproblevel)]
                  )
            children$p3[["obs_description"]] <-
                paste("p", p3, v3)
        }
    }#end for each problevel

    for (apaylevel in payoff_levels){
        if (is.na(v1)){
            children$v1[["state"]] <-
                c(children$v1[["state"]],
                  infostate_constructor(p1, p2, p3,
                                        apaylevel, v2, v3)$mystringid
                  )
            children$v1[["prob"]] <-
                c(children$v1[["prob"]],
                  paylevel_prob[which(payoff_levels == apaylevel)]
                  )
            children$v1[["obs_description"]] <-
                paste("v", p1, v1)
        }
        if (is.na(v2)){
            children$v2[["state"]] <-
                c(children$v2[["state"]],
                  infostate_constructor(p1, p2, p3,
                                        v1, apaylevel, v3)$mystringid
                  )
            children$v2[["prob"]] <-
                c(children$v2[["prob"]],
                  paylevel_prob[which(payoff_levels == apaylevel)]
                  )
            children$v2[["obs_description"]] <-
                paste("v", p2, v2)
        }
        if (is.na(v3)){
            children$v3[["state"]] <-
                c(children$v3[["state"]],
                  infostate_constructor(p1, p2, p3,
                                        v1, v2, apaylevel)$mystringid
                  )
            children$v3[["prob"]] <-
                c(children$v3[["prob"]],
                  paylevel_prob[which(payoff_levels == apaylevel)]
                  )
            children$v3[["obs_description"]] <-
                paste("v", p3, v3)
        }
    }

    value_now <- max(c(option_expectedvalue(p1, v1), #choose1
                       option_expectedvalue(p2, v2), #choose2,
                       option_expectedvalue(p3, v3)  #choose3
                       ))

    ret <- list(
        myfeatures = list(p1 = p1,
                          p2 = p2,
                          p3 = p3,
                          v1 = v1,
                          v2 = v2,
                          v3 = v3),
        mystringid = mystringid,
        children = children,
        value_now = value_now
    )
    seen_states[[mystringid]] <- ret
    return(ret)
}

init <- infostate_constructor(NA, NA, NA, NA, NA, NA)

opt_only <- hash()
populate_optonly <- function(infostate, n_obs){
    mybest <- best_action(infostate, n_obs)[1]#new: TIES BROKEN. Having one branch makes it easier to process the get-strategy-descriptions later.
    if (!is.null(opt_only[[infostate]]))return()

    for (x in seen_states[[infostate]]$children){
        if (length(x) == 0) next #observed features have children 'list()'
        if (x$obs_description %in% mybest){#mybest might have length > 1
            for (astate in x$state){
                populate_optonly(astate, n_obs - 1)
            }
        }
    }
    opt_only[[infostate]] <- mybest
}

##populate_optonly(init$mystringid, 1)

##for n_obs = 6, all actions have equal value
##But opt_only still has fewer states than seen_states
##only because opt stops exploring when no improvement is possible
##ie anything that includes [high_prob high_payoff] halts and chooses that.
##so the lines with more than one high-high don't get fully explored.
