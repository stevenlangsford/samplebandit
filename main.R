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

ordobs <- function(a, b, tolerance, noise){
    diff <- rnorm(1, a - b, noise)
    if (diff < (-tolerance)) return(1);
    if (abs(diff) < tolerance) return(2);
    if (diff > tolerance) return(3);
}

calcobs <- function(anoption, ppntnoise){
    rnorm(1, anoption[1] * anoption[2], ppntnoise)
}

##sim setup
n_stim <- 5
n_ppnts <- 3

stim <- t(replicate(n_stim, rnd_wedell())) #n*3 matrix of rnd_options

##ppnt params
ppnt_tolerance_prob <- rep(.1, n_ppnts)
ppnt_ordnoise_prob <- rep(.15, n_ppnts)
ppnt_tolerance_payout <- rep(1.5, n_ppnts) #check these param vals?
ppnt_ordnoise_payout <- rep(2.5, n_ppnts)
ppnt_calc_noise <- rep(3, n_ppnts)

##end setup.
##wrangle setup into convenient format:
ppnt_tolerance <- matrix(c(ppnt_tolerance_prob, ppnt_tolerance_payout),
                     ncol = 2,
                     nrow = n_ppnts)
ppnt_ordnoise <- matrix(c(ppnt_ordnoise_prob, ppnt_ordnoise_payout),
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
                                                               ppnt_tolerance[trials.df[atrial, "ppnt"], afeature],
                                                               ppnt_ordnoise[trials.df[atrial, "ppnt"], afeature]),
                                                  simtruth = ordobs(stim[[trials.df[atrial, "stim"], opt1]][afeature],
                                                               stim[[trials.df[atrial, "stim"], opt2]][afeature],
                                                               ppnt_tolerance[trials.df[atrial, "ppnt"], afeature],
                                                               .000000001)#truth repeats ordobs but with chuck norris precision.
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
                 ordtolerance = ppnt_tolerance,
                 ordnoise = ppnt_ordnoise,

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

source("vis.R")

## print(bystim_beliefcloud(1));
## x11();
## print(ordobs_calibrationcheck(1))
     
## for (i in 1:5){
##     ggsave(bystim_beliefcloud(i),
##            file = paste0("plots/beliefs", i, ".png"))
##     ggsave(stim_choicepattern(i),
##            file = paste0("plots/choicepattern", i, ".png"))
##     ggsave(ordobs_calibrationcheck(i) - bystim_beliefcloud(i),
##            file = paste0("plots/combocheck", astim, ".png"),
##            width = 10, height = 10)
##     ggsave(ordobs_calibrationcheck(i),
##            file = paste0("plots/ordcalibration", i, ".png"))
##     }
## for (i in 1:5){
##     ggsave(bystim_beliefcloud(i),
##            file = paste0("plots/beliefs", i, ".png"))
##     ggsave(stim_choicepattern(i),
##            file = paste0("plots/choicepattern", i, ".png"))
##     ggsave(ordobs_calibrationcheck(i) - bystim_beliefcloud(i),
##            file = paste0("plots/combocheck", astim, ".png"),
##            width = 10, height = 10)
     print(ordobs_calibrationcheck(1)); x11(); print(bystim_beliefcloud(1))
##            file = paste0("plots/ordcalibration", i, ".png"))
##     }


##standardize features: tune once
##systematic decoy variation, not rnd stim
##noiseless ordobs?
