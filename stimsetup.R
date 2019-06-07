##helper fns: not actually for setup, but belong here

##Check the mean and sd of features from some trial-generator
##Handy to set appropriate ordobs params.
featurediststats <- function(ntrials = 10000, trialfn = rnd_wedell){
pile_o_trials <- t(replicate(ntrials, trialfn()))#usual trial gen process
dim(pile_o_trials) <- ntrials * 3 #flatten matrix to list
myprobs <- sapply(pile_o_trials, function(trial){
    trial[1]
})
mypayouts <- sapply(pile_o_trials, function(trial){
    trial[2]
})

myvalue <- sapply(pile_o_trials, function(trial){
    trial[1] * trial[2]
})

ans <- list(prob = c(mean = mean(myprobs), sd = sd(myprobs)),
            payout = c(mean = mean(mypayouts), sd = sd(mypayouts)),
            value = c(mean = mean(myvalue), sd = sd(myvalue))
            )
return(ans)
}


##world setup fns
rnd_option <- function(scale="howes16"){
    if (scale == "howes16"){
    return(c(rbeta(1, 1, 1), #these match the priors used in howes16
      rnorm(1, 100, 5))
      )
    }
    if (scale == "wedell"){
        return(
            c(rbeta(1, 1, 1),
              rnorm(1, 20, 7) #these match the 'typical range' of wedell stim.
            )
        )
    }
}

rnd_trial <- function(){
    list(rnd_option(),
         rnd_option(),
         rnd_option())
}

rnd_wedell <- function(){
    ##targetA
targ_a <- list(
        a1 = c(.83, 12),
        a2 = c(.67, 15),
        a3 = c(.5, 20),
        a4 = c(.4, 25)
    )
    ##targetB
targ_b <- list(
        b1 = c(.3, 33),
        b2 = c(.4, 25),
        b3 = c(.5, 20),
        b4 = c(.67, 15)
)
    ##range,  frequency,  rangefrequency.
    decoy <- list(
        Ra1 = c(.4, 20),
        Ra2 = c(.5, 18),
        Ra3 = c(.67, 13),
        Ra4 = c(.83, 10),
        Fa1 = c(.35, 25),
        Fa2 = c(.45, 20),
        Fa3 = c(.62, 15),
        Fa4 = c(.78, 12),
        RFa1 = c(.35, 20),
        RFa2 = c(.45, 18),
        RFa3 = c(.62, 13),
        RFa4 = c(.78, 10),
        ##range,  frequency,  rangefrequency.
        Rb1 = c(.25, 33),
        Rb2 = c(.35, 25),
        Rb3 = c(.45, 20),
        Rb4 = c(.62, 15),
        Fb1 = c(.3, 30),
        Fb2 = c(.4, 20),
        Fb3 = c(.5, 18),
        Fb4 = c(.67, 13),
        RFb1 = c(.25, 30),
        RFb2 = c(.35, 20),
        RFb3 = c(.45, 18),
        RFb4 = c(.62, 13)
    )

            candidate <- list(
    unlist(base::sample(targ_a, 1)),
    unlist(base::sample(targ_b, 1)),
    unlist(base::sample(decoy, 1))
    )
    while (isTRUE(all.equal(as.numeric(candidate[[1]]), #avoid a==b
                            as.numeric(candidate[[2]])
                            ))){
        candidate <- list(
            unlist(base::sample(targ_a, 1)),
            unlist(base::sample(targ_b, 1)),
            unlist(base::sample(decoy, 1))
        )
    }
    return(candidate)
}

systematic_stimset <- function(hm_diststeps = 5,
                               hm_deltasteps = 5,
                               max_dist = 5,
                               max_delta = 5){
    ##note 'delta' is in payout, prob adjusts to hit target value.
    targ <- c(.4, 25) #one arbitrary pair from wedell stim:
    alt <- c(10 / 15, 15)
    
    diststeps <- seq(from = 0, to = max_dist, length = hm_diststeps)
    deltasteps <- seq(from = 0, to = max_delta, length = hm_deltasteps)

    ##Init, to overwrite
    nulltrial <- function(){
        return(list(c(NA, NA), c(NA, NA), c(NA, NA)))
    }

    stimset <- t(replicate(hm_diststeps * hm_deltasteps, nulltrial()))

    rowcounter <- 1 #lazy: tracks row over dist/delta combos.
    for (dist in diststeps){
        for (delta in deltasteps){
            stimset[[rowcounter, 1]] <- targ
            stimset[[rowcounter, 2]] <- alt
        ##decoy is worth 'dist' less than targ: but orientation random.
            mypayout <- targ[2] - delta
            myprob <- (targ[1] * targ[2] - dist) / mypayout
            stimset[[rowcounter, 3]] <- c(myprob, mypayout)

            rowcounter <- rowcounter + 1
        }
    }
    return(stimset)
}

standardize_stimset <- function(astimset){
    #only really works if you have 'natural' stim ranges.
    allprobs <- sapply(astimset, function(x){
        x[1]
    })
    allpayoffs <- sapply(astimset, function(x){
        x[2]
    })

    probs_mean <- mean(allprobs)
    probs_sd <- sd(allprobs)
    pay_mean <- mean(allpayoffs)
    pay_sd <- sd(allpayoffs)

    for (i in 1:nrow(astimset)){
        for (j in 1:ncol(astimset)){
            astimset[[i, j]][1] <- (astimset[[i, j]][1] - probs_mean) / probs_sd
            astimset[[i, j]][2] <- (astimset[[i, j]][2] - pay_mean) / pay_sd
        }
    }
    return(astimset)
}
