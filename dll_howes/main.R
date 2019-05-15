library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

theme_set(theme_light())
rm(list = ls())
set.seed(1)

##world setup
rnd_option <- function(){
#prob-payoff trials that match the howes16 priors.
    c(rbeta(1, 1, 1),
      rnorm(1, 100, 5)
      )
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

ordobs <- function(f1, f2, coeff, theta){
        p_lessthan <- function(mydiff){
            1 / (1 + exp(coeff * mydiff + theta))
        }
        p_greaterthan <- function(mydiff){
            1 / (1 + exp(-coeff * mydiff + theta))
        }
        p10 <- function(mydiff){
            p_lessthan(mydiff) * (1 - p_greaterthan(mydiff))
        }
        p00 <- function(mydiff){
            (1 - p_lessthan(mydiff)) * (1 - p_greaterthan(mydiff))
        }
        p01 <- function(mydiff){
            (1 - p_lessthan(mydiff)) * (p_greaterthan(mydiff))
        }
    #p11 doesn't make much sense... ok to just ignore&renormalize? Odd?
    ans <- c(p10(f1 - f2), p00(f1 - f2), p01(f1 - f2))
    return(base::sample(1:3, size = 1, prob = ans / sum(ans)))
}
get_ordtruth <- function(f1, f2, coeff, theta){
    repruns <- table(replicate(1000, ordobs(f1, f2, coeff, theta)))
    ans <- which(repruns == max(repruns))[1]
    return(names(ans))
}

calcobs <- function(anoption, ppntnoise){
    rnorm(1, anoption[1] * anoption[2], ppntnoise)
}

##sim setup
n_stim <- 10
n_ppnts <- 50

stim <- t(replicate(n_stim, rnd_wedell())) #n*3 matrix of rnd_options

##ppnt params

ppnt_coeff_prob <- rep(100, n_ppnts)
ppnt_theta_prob <- rep(1, n_ppnts)
ppnt_coeff_payout <- rep(2, n_ppnts)
ppnt_theta_payout <- rep(1, n_ppnts)

ppnt_calc_noise <- rep(10, n_ppnts)

##end setup.
##wrangle setup into convenient format:
ppnt_coeff <- matrix(c(ppnt_coeff_prob, ppnt_coeff_payout),
                     ncol = 2,
                     nrow = n_ppnts)
ppnt_theta <- matrix(c(ppnt_theta_prob, ppnt_theta_payout),
                     ncol = 2,
                     nrow = n_ppnts)


trials.df <- data.frame()
for (astim in 1:n_stim){
    for (appnt in 1:n_ppnts){
        featureframe <- t(unlist(stim[astim, ])) %>% data.frame
        names(featureframe) <- c("prob_A", "pay_A",
                                 "prob_B", "pay_B",
                                 "prob_C", "pay_C")
        trials.df <- rbind(trials.df, data.frame(
                                          ppnt = appnt,
                                          stim = astim
                                     ) %>% cbind(featureframe))
    }
}
trials.df$trial <- 1:nrow(trials.df)



###################                                                                                            
##get sim exp observations: for now, just one of everything for each trial.
ordobs.df <- data.frame()
calcobs.df <- data.frame()

for (atrial in 1:nrow(trials.df)){
    ##one of each calcobs:
    for (anoption in 1:3){
        calcobs.df <- rbind(calcobs.df, data.frame(
                                            ppntid = trials.df[atrial, "ppnt"],
                                            trial = atrial,
                                            stim = trials.df[atrial, "stim"],
                                            option = anoption,
                                            obs = calcobs(stim[[trials.df[atrial, "stim"], anoption]], ppnt_calc_noise[trials.df[atrial, "ppnt"]]),
                                            simtruth = calcobs(stim[[trials.df[atrial, "stim"], anoption]], 0.00000001)#truth is just an obs with chuck norris precision.
                                            ))
    }
    ##one of each ordobs:
    for (opt1 in 2:3){
        for (opt2 in 1:(opt1 - 1)){
            for (afeature in 1:2){
                ordobs.df <- rbind(ordobs.df, data.frame(
                                                  ppntid = trials.df[atrial, "ppnt"],
                                                  trial = atrial,
                                                  stim = trials.df[atrial, "stim"],
                                                  opt1 = opt1,
                                                  opt2 = opt2,
                                                  feature = afeature,
                                                  obs = ordobs(stim[[trials.df[atrial, "stim"], opt1]][afeature],
                                                               stim[[trials.df[atrial, "stim"], opt2]][afeature],
                                                               ppnt_coeff[trials.df[atrial, "ppnt"], afeature],
                                                               ppnt_theta[trials.df[atrial, "ppnt"], afeature]),
                                                  simtruth = get_ordtruth(stim[[trials.df[atrial, "stim"], opt1]][afeature],
                                                               stim[[trials.df[atrial, "stim"], opt2]][afeature],
                                                               ppnt_coeff[trials.df[atrial, "ppnt"], afeature],
                                                               ppnt_theta[trials.df[atrial, "ppnt"], afeature])#truth repeats ordobs but with chuck norris precision.
                ))
            }
        }
    }
}

datalist <- list(n_trials = nrow(trials.df),
                 n_options = 3,
                 n_features = 2,
                 n_ppnts = n_ppnts,

                 n_ordobs = nrow(ordobs.df),
                 ord_ppntid = ordobs.df$ppntid,
                 ordtrial = ordobs.df$trial,
                 ordopt1 = ordobs.df$opt1,
                 ordopt2 = ordobs.df$opt2,
                 ordfeature = ordobs.df$feature,
                 ordobs = ordobs.df$obs,
                 ordcoeff = ppnt_coeff,
                 ordtheta = ppnt_theta,

                 n_calcobs = nrow(calcobs.df),
                 calcppntid = calcobs.df$ppntid,
                 calctrial = calcobs.df$trial,
                 calcoption = calcobs.df$option,
                 calcobs = calcobs.df$obs,
                 calcnoise = ppnt_calc_noise)

fit <- stan(file = "howes2016.stan",
            data = datalist,
            iter = 1500,
            chains = 4,
            control = list(max_treedepth = 15,
                           adapt_delta = .99))

samples <- data.frame(extract(fit, permuted = TRUE))

save.image("testRun.RData")

source("vis.R")
