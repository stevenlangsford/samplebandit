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
    ##prob-payoff trials that match the howes16 priors.
    c(rbeta(1, 1, 1),
      rnorm(1, 100, 5)
      )
}

rnd_trial <- function(){
    list(rnd_option(),
         rnd_option(),
         rnd_option())
}

ordobs <- function(f1, f2, tolerance, p_err){
    if (runif(1, 0, 1) < p_err){
        return(base::sample(1:3, 1))
    }
    ##Ok, error didn't fire, return a good obs:
    if (abs(f1 - f2) < tolerance){
        return(2)
    }
    if (f1 < f2){
        return(1)
    }
    if (f1 > f2){
        return(3)
    }
    stop(paste("ordobs washout", f1, f2, tolerance, p_err))
}

calcobs <- function(anoption, ppntnoise){
    rnorm(1, anoption[1] * anoption[2], ppntnoise)
}

##sim setup
n_trials <- 3
n_ppnts <- 5

trials <- replicate(n_trials, rnd_trial()) #n*3 matrix of rnd_options

##ppnt params
tolerance <- matrix(c(rep(.1, n_ppnts), #prob tolerance
                      rep(10, n_ppnts)), #payoff tolerance
                    nrow = n_ppnts,
                    ncol = 2)
ord_error <- rep(.01, n_ppnts)
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
                                                             tolerance[appnt, afeature],
                                                             ord_error[appnt]))
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
                 ordtolerance = tolerance,
                 orderror = ord_error,

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
