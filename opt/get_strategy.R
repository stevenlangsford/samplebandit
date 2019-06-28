library(tidyverse)
source("goldenline.R")#contains rm(list=ls())
##goldenline.R puts seen_states and opt_only in the environment.
##Both hash tables with infostate strings as keys
##seen states values are lists, info about current features and children.
##opt_only values are action strings.
##action strings start with choose, p[rob], or v[alue], followed by features.
##There might be more than one action, separated by OR.


##setup
obs_budget <- 4
populate_optonly(init$mystringid, obs_budget)#2nd arg is obseration budget

instruction_list <- list()


##end setup. populate action-group
for (whichstep in 1:(obs_budget + 1)){
##steps go to obs + 1 because last step is choose.

    thisstep <- opt_only[names(opt_only)[sapply(names(opt_only),
                                                function(x){
                                                    str_count(x, "NA") == 7 - whichstep #ugh magic number (features+1)
                                                })]
                         ]

    actiongroup <- list()
    
    for (astate in names(thisstep)){
        anaction <- thisstep[[astate]][1]#tiebreaker on equally good actions.
        if (is.null(actiongroup[[anaction]])){
            actiongroup[[anaction]] <- astate
        }else{
            actiongroup[[anaction]] <- c(actiongroup[[anaction]], astate)
        }
    }

    ##fun facts about this group of states.
    
    get_funfacts <- function(agroup, matchfn = c(all, any)[[1]] ){
        
        ##agroup is a vector of infostate strings

        fun_facts <- c()
        ##Order by simplicity, so you can just use the first unique id-ing fact.
        ##helper functions add directly to fun_facts vector.
        ##This is so you can pull out code for id'ing the properties.
        ##Then just have less-messy sequence of calls to them in 'main sequence'
        ##'main sequence' should then be much more readable?
        ##probably a bad pattern, but this is how this works for now.

        ##Helper fns
        ##all ff_ functions add to fun_facts
        ff_infostate_morethan_matches <- function(target, targcount){
            if (matchfn(sapply(agroup, function(x){
                str_count(x, target) > targcount
            }
            ))){
                fun_facts <<- c(fun_facts, paste("more than",
                                                 targcount,
                                                 "of",
                                                 target))
            }
        }
        
        ff_infostate_exactly_matches <- function(target, targcount){
            if (matchfn(sapply(agroup, function(x){
                str_count(x, target) == targcount
            }
            ))){
                fun_facts <<- c(fun_facts, paste("exactly",
                                                 targcount,
                                                 "of",
                                                 target))
            }
        }

        ff_highest_prob <- function(){
            probrank <- function(aprob){
                if (is.na(aprob)) return(-Inf)
                return(which(prob_levels == aprob))
            }
            mymax <- -Inf
            for (astate in agroup){
                mymax <- max(mymax, sapply(seen_states[[astate]]$myfeatures[c("p1", "p2", "p3")], probrank))
            }

            if (mymax == -Inf){
                fun_facts <<- c(fun_facts, "all probs are NA")
            }else{
                fun_facts <<- c(fun_facts,
                                paste("max prob is", prob_levels[mymax]))
            }
        }

        ff_highest_payoff <- function(){
            payrank <- function(apay){
                if (is.na(apay)) return(-Inf)
                return(which(payoff_levels == apay))
            }
            mymax <- -Inf
            for (astate in agroup){
                mymax <- max(mymax, sapply(seen_states[[astate]]$myfeatures[c("v1", "v2", "v3")], payrank))
            }
            if (mymax == -Inf){
                fun_facts <<- c(fun_facts, "all payoffs are NA")
            }else{
                fun_facts <<- c(fun_facts,
                                paste("max payoff is", payoff_levels[mymax]))
            }
        }




        ff_lowest_prob <- function(){
            probrank <- function(aprob){
                if (is.na(aprob)) return(Inf)
                return(which(prob_levels == aprob))
            }
            mymin <- Inf
            for (astate in agroup){
                mymin <- min(mymin, sapply(seen_states[[astate]]$myfeatures[c("p1", "p2", "p3")], probrank))
            }

            if (mymin == Inf){
                ##fun_facts <<- c(fun_facts, "all probs are NA")
                ##if you're checking both max and min, only one of them needs to notice all NA.
            }else{
                fun_facts <<- c(fun_facts,
                                paste("min prob is", prob_levels[mymin]))
            }
        }

        ff_lowest_payoff <- function(){
            payrank <- function(apay){
                if (is.na(apay)) return(Inf)
                return(which(payoff_levels == apay))
            }
            mymin <- Inf
            for (astate in agroup){
                mymin <- min(mymin, sapply(seen_states[[astate]]$myfeatures[c("v1", "v2", "v3")], payrank))
            }
            if (mymin == Inf){
                ##fun_facts <<- c(fun_facts, "all payoffs are NA")
                ##max has you covered.
            }else{
                fun_facts <<- c(fun_facts,
                                paste("min payoff is", payoff_levels[mymin]))
            }
        }

        ##End helper fns: main-sequence fact-getter follows.

        ##Important to order these 'fun facts' by simplicity, simplest first.

        ##A single feature, appears more than X times
        for (acount in 0:3){
            for (alevel in c(prob_levels, "NA_")){
                ff_infostate_morethan_matches(alevel, acount)
            }
            for (alevel in c(payoff_levels, "_NA")){
                ff_infostate_morethan_matches(alevel, acount)
            }
        }

        ##A single feature, appears exactly X times
        for (acount in 0:3){
            for (alevel in c(prob_levels, "NA_")){
                ff_infostate_exactly_matches(alevel, acount)
            }
            for (alevel in c(payoff_levels, "_NA")){
                ff_infostate_exactly_matches(alevel, acount)
            }
        }

        ## A pair of features, appears more than X times
        for (acount in 0:3){
            for (plevel in c(prob_levels, "NA")){
                for (vlevel in c(payoff_levels, "NA")){
                    ff_infostate_morethan_matches(paste0(plevel, "_", vlevel),
                                                  acount)
                }
            }
        }

        ## A pair of features, appears exactly X times
        for (acount in 0:3){
            for (plevel in c(prob_levels, "NA")){
                for (vlevel in c(payoff_levels, "NA")){
                    ff_infostate_exactly_matches(paste0(plevel, "_", vlevel),
                                                 acount)
                }
            }
        }

        ##max/min prob/payoff
        ff_highest_prob()
        ff_highest_payoff()
        ff_lowest_prob()
        ff_lowest_payoff()
        

        return(fun_facts)
    }

    if (length(names(actiongroup)) == 1){
        instruction_list[[length(instruction_list) + 1]] <-
            paste("take action |", names(actiongroup))
        next
    }


    all_ff <- lapply(actiongroup, get_funfacts, all)
    any_ff <- lapply(actiongroup, get_funfacts, any)



    instruction_set <- c()
    covered_scenarios <- c()

    ##First pass:
    for (i in 1:length(all_ff)){
        myfacts <- all_ff[[i]]
        altfacts <- unique(as.character(unlist(any_ff[-i])))
        idfacts <- setdiff(myfacts, altfacts)[1]##assume in simplicity order.
        if (!is.na(idfacts)){

            instruction_set <- c(instruction_set, paste("if",
                                                        idfacts,
                                                        "then take action |",
                                                        names(actiongroup)[i]
                                                        )
                                 )
            covered_scenarios <- c(covered_scenarios, -i)
        }
    }
    ##second pass:
    instruction_set <- c(instruction_set, "THEN")
    
    for (i in 1:length(names(actiongroup))){
        if (-i %in% covered_scenarios) next
        myfacts <- all_ff[[i]]
        altfacts <- any_ff[c(-i, covered_scenarios)] %>%
            unlist %>%
            as.character %>%
            unique
        idfacts <- setdiff(myfacts, altfacts)[1]##1st assumes simplicity order
        if (!is.na(idfacts)){
            instruction_set <- c(instruction_set, paste("if",
                                                        idfacts,
                                                        "then take action |",
                                                        names(actiongroup)[i]
                                                        )
                                 )
            covered_scenarios <- c(covered_scenarios, -i)
        }
    }

    ##remove the "THEN" instruction-tag if the second pass was not used
    if (instruction_set[length(instruction_set)] == "THEN"){
        instruction_set <- instruction_set[-length(instruction_set)]
    }
    
    ##Leftovers:
    if (length(covered_scenarios) != length(names(actiongroup))){
        instruction_set <- c(instruction_set, "ALSO")
        for (i in 1:length(names(actiongroup))){
            if (-i %in% covered_scenarios) next
            for (astate in actiongroup[[i]]){
                instruction_set <- c(instruction_set,
                                     paste("if",
                                           astate,
                                           "take action",
                                           names(actiongroup)[i]
                                           )
                                     )
            }
        }
    }

    instruction_list[[length(instruction_list) + 1]] <- instruction_set
}#end for each step

instruction_list
## [4] "if min prob is lowprob then take action | v midprob NA" ##NOPE
