library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

theme_set(theme_light())
rm(list = ls())

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
    repruns <- replicate(1000, ordobs(f1, f2, coeff, theta))
    return(which(repruns == max(repruns))[1])
}

calcobs <- function(anoption, ppntnoise){
    rnorm(1, anoption[1] * anoption[2], ppntnoise)
}

##sim setup
n_trials <- 3
n_ppnts <- 5

trials <- t(replicate(n_trials, rnd_trial())) #n*3 matrix of rnd_options

##ppnt params
ppnt_coeff_prob <- rep(100, n_ppnts)
ppnt_theta_prob <- rep(1, n_ppnts)
ppnt_coeff_payout <- rep(2, n_ppnts)
ppnt_theta_payout <- rep(1, n_ppnts)

##store together because it's nice to index into these rather than use if
ppnt_coeff <- matrix(c(ppnt_coeff_prob, ppnt_coeff_payout),
                     ncol = 2,
                     nrow = n_ppnts)
ppnt_theta <- matrix(c(ppnt_theta_prob, ppnt_theta_payout),
                     ncol = 2,
                     nrow = n_ppnts)

calc_noise <- rep(1, n_ppnts)

##get sim exp observations
ordobs.df <- data.frame()
calcobs.df <- data.frame();
sweep_ordobs <- function(){
    for (atrial in 1:n_trials){
        print(paste("Ordsweep trial", atrial));
        for (appnt in 1:n_ppnts){

            for (afeature in 1:2){
                for (opt1 in 2:3){
                    for (opt2 in 1:opt1){
                        
                        
                        ordobs.df <<- rbind(ordobs.df,
                                            data.frame(
                                                ppntid = appnt,
                                                trial = atrial,
                                                opt1 = opt1,
                                                opt2 = opt2,
                                                feature = afeature,
                                                obs = ordobs(trials[[atrial, opt1]][afeature],
                                                             trials[[atrial, opt2]][afeature],
                                                             ppnt_coeff[appnt, afeature],
                                                             ppnt_theta[appnt, afeature]))
                                        # f2, tolerance, p_err
                                            )
                    }
                }
            }
        }
    }
}

sweep_calcobs <- function(){
    for (atrial in 1:n_trials){
        for (appnt in 1:n_ppnts){
            for (anoption in 1:3){
                calcobs.df <<- rbind(calcobs.df,
                                    data.frame(
                                        ppntid = appnt,
                                        trial = atrial,
                                        option = anoption,
                                        obs = calcobs(trials[[atrial, anoption]],
                                                      calc_noise[appnt])
                                    )
                                    )
            }
        }
    }
}
##get some obs
sweep_ordobs()
sweep_calcobs()

datalist <- list(n_trials = n_trials,
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
                 calcnoise = calc_noise)

fit <- stan(file = "howes2016.stan",
            data = datalist,
            iter = 1500,
            chains = 4,
            control = list(max_treedepth = 10))

samples <- data.frame(extract(fit, permuted = TRUE))

source("vis.R")
