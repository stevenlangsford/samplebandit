##assuming instruction from best_action()
opttable.df <- data.frame(state = c(), instruction = c())

build_opttable <- function(infostate, n_obs){
    if (infostate %in% opttable.df$state){
        stop(paste(infostate, n_obs, "double dipping error?"))
    }
    
    instruction <-  best_action(infostate, n_obs)
    
    opttable.df <<- rbind(opttable.df, data.frame(
                                           state = infostate,
                                           action = instruction
                                       )
                          )
    if (str_count(instruction, "choose") > 0){
        print(paste("out: ", infostate))
        return() #No children if your action is choice not obs.
    }
    for (candidate_action in
         strsplit(instruction, "observe: ")[[1]] %>% .[sapply(., function(x){
             nchar(x) > 0
         }
         )]){
        ##Software gods hate this kind of thing. Do penance.

        tomatch <- strsplit(candidate_action, " : ")[[1]]
        mystate <- seen_states[[infostate]]

        ##TRUE if you want to observe here:
        can_observe <- paste0(tomatch[1], (1:3)[c(
                                              paste(mystate$myfeatures["p1"], mystate$myfeatures["v1"]) == tomatch[2],
                                              paste(mystate$myfeatures["p2"], mystate$myfeatures["v2"]) == tomatch[2],
                                              paste(mystate$myfeatures["p3"], mystate$myfeatures["v3"]) == tomatch[2]
                                          )])

        children <- c()
        ##the feature you want to observe is in tomatch[1]
        if (tomatch[1] == "p"){
            for (alevel in prob_levels){
                for (targfeature in can_observe){
                    children <- c(children,
                                  switch(targfeature,
                                         "p1" = infostate_constructor(alevel,
                                                                      mystate$myfeatures["p2"],
                                                                      mystate$myfeatures["p3"],
                                                                      mystate$myfeatures["v1"],
                                                                      mystate$myfeatures["v2"],
                                                                      mystate$myfeatures["v3"])$mystringid,
                                         "p2" = infostate_constructor(mystate$myfeatures["p1"],
                                                                      alevel,
                                                                      mystate$myfeatures["p3"],
                                                                      mystate$myfeatures["v1"],
                                                                      mystate$myfeatures["v2"],
                                                                      mystate$myfeatures["v3"])$mystringid,
                                         "p3" = infostate_constructor(mystate$myfeatures["p1"],
                                                                      mystate$myfeatures["p2"],
                                                                      alevel,
                                                                      mystate$myfeatures["v1"],
                                                                      mystate$myfeatures["v2"],
                                                                      mystate$myfeatures["v3"])$mystringid
                                         )#end switch. Might be the ugliest code you've ever.
                                  )
                }#targfeature
            }#problevels
        }else{ #ie. tomatch = 'v'
            for (alevel in payoff_levels){
                for (targfeature in can_observe){
                    children <- c(children,
                                  switch(targfeature,
                                         "v1" = infostate_constructor(mystate$myfeatures["p1"],
                                                                      mystate$myfeatures["p2"],
                                                                      mystate$myfeatures["p3"],
                                                                      alevel,
                                                                      mystate$myfeatures["v2"],
                                                                      mystate$myfeatures["v3"])$mystringid,
                                         "v2" = infostate_constructor(mystate$myfeatures["p1"],
                                                                      mystate$myfeatures["p2"],
                                                                      mystate$myfeatures["p3"],
                                                                      mystate$myfeatures["v1"],
                                                                      alevel,
                                                                      mystate$myfeatures["v3"])$mystringid,
                                         "v3" = infostate_constructor(mystate$myfeatures["p1"],
                                                                      mystate$myfeatures["p2"],
                                                                      mystate$myfeatures["p3"],
                                                                      mystate$myfeatures["v1"],
                                                                      mystate$myfeatures["v2"],
                                                                      alevel)$mystringid
                                         )#end switch. Might be the ugliest code you've ever.
                                  )
                }#targfeature
            }#paylevels
        }#tomatch is v

        for (achild in unique(children)){
            if (n_obs > 0){
                if (!(achild %in% opttable.df$state)){
                    build_opttable(achild, n_obs - 1)
                }
            }
        }#end for each unique child
        browser()
        return()
    }
}

opttable.df <- data.frame(state = c(), action = c())
build_opttable("NA_NA:NA_NA:NA_NA", 5)
View(opttable.df)

####################################################################################################
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


##So the actual optimal sequence of observations is a tree, right?
##There are often ties, options that are equally good given the info available.
##Best way would be to take the optimal tree with the simplest description.
##But I don't know how to do that.
##So here's a hack: select an arbitrary one of the optimal options,
##but do it using this preference ranking order
##choose the actions that are most often good,
##if you're lucky that might mean you give the same suggestion consistently
##Thereby simplifying the instruction set.
tiebreaker_list <- table(
    sapply(names(seen_states), function(astate){
        best_action(astate,
                    ifelse(
                        str_count(seen_states[[astate]]$mystringid, "NA") == 0,
                        0, 1)
                    )
    })
) %>% sort %>% rev %>% names
