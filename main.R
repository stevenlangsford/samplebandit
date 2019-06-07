library(tidyverse)
library(rstan)
library(shinystan)
library(patchwork)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

theme_set(theme_light())
rm(list = ls())
set.seed(1)

source("stimsetup.R") #a bunch of functions for generating trials: doesn't actualy create 'stim', that's done here in main, but using the functions from this.
#trial format is c(prob, payout)

starttime <- Sys.time()
#This is the core theoretical claim: people get information about the options presented to them via these two kinds of observation, then take the Bayes-normative option that maximizes expected return.
ordobs <- function(a, b, tolerance, noise){
    diff <- rnorm(1, a - b, noise)
    if (diff < (-tolerance)) return(1);
    if (abs(diff) < tolerance) return(2);
    if (diff > tolerance) return(3);
}

calcobs <- function(anoption, noise){
    rnorm(1, anoption[1] * anoption[2], noise)
}

##sim setup
##Rnd stim setup:
##n_stim <- 5
##n_ppnts <- 3
##stim <- t(replicate(n_stim, rnd_wedell())) #n*3 matrix of rnd_options

##Systematic stim setup:
##stim <- systematic_stimset(hm_diststeps = 6, hm_deltasteps = 2, max_delta = 3)

##cpp comparison stim:
load("cpp_stim.RData")
stim <- stim[1:5, ]

n_stim <- nrow(stim)
n_ppnts <- 20 #too low?
##time guide for my laptop:
##9 stim 10 ppnts ~17 seconds per k-transitions 5.5 min
##5 stim 20 ppnts ~25 seconds per k-transitions 
##9 stim 20 ppnts ~50 seconds per k-transitions 36 min
##12 stim 20 ppnts ~53 seconds per k-transitions 2.3 hrs
##15 stim 20 ppnts ~65 seconds per k-transitions 
##ppnt params
##Not at all clear what these params should be.
##Hopefully there's a range where they reproduce the empirical context effects (attraction, similarity, compromise, some other things)
##Even more hopefully, they should be allowed to vary a bit across ppnts and reproduce in aggregate the attested relationship between the different effects
##Attraction is correlated with compromise, similarity is negatively correlated with both of them.

##guess/explore on std normal scale, then invert standardization to put the guess in the scale of your trial features?
std_guesstol <- .3#Tolerance in feature-sd units.
std_guessnoise <- .3#Total guess: x1 tolerance width?
std_guesscalcnoise <- .5#For consistency, calcnoise also in sd(trialvalue) units

myfeaturestats <- featurediststats(trialfn = rnd_wedell) #note trialfn should match actual stim defined above.

ppnt_tolerance_prob <- rep(myfeaturestats$prob["sd"] *
                           std_guesstol,
                           n_ppnts)
ppnt_ordnoise_prob <- rep(myfeaturestats$prob["sd"] *
                          std_guessnoise,
                          n_ppnts)
ppnt_tolerance_payout <- rep(myfeaturestats$payout["sd"] *
                             std_guesstol,
                             n_ppnts)
ppnt_ordnoise_payout <- rep(myfeaturestats$payout["sd"] *
                            std_guessnoise,
                            n_ppnts)

ppnt_calc_noise <- rep(std_guesscalcnoise *
                       myfeaturestats$value["sd"],
                       n_ppnts)

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
##The goal is to turn this into a sequential sampling story where people are allowed to select which observation they want next and when they have observed enough.
##Attested empirical effect is that context effects are reduced under time pressure. Also all the cool kids in this space care about response times, the original howes16 account is static, so this is a desirable extension to stay in the game.
##Anyway anticipating this sequential-sampling extension is why the data are formatted the way they are, with each observation surrounded by a gigantic cloud of index variables indicating where it came from. It's so you can mess around with the ordobs.df and calcobs.df input at will to flexibly include or drop observations for any particular feature/trial/participant.

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

endtime <- Sys.time()
bettertimer <- proc.time()#better than start vs end?


source("vis.R") #not just a clean collection of plotting functions as you might expect: does some format wrangling putting stim into stim.df and adding info to trials.df, so can't run without following main.R.
saveplots <- TRUE
showplots <- FALSE
save.image("cpp_comparison.RData")#OPTIONAL

for (i in 1:n_stim){
    if (saveplots){
        ggsave(bystim_beliefcloud(i),
               file = paste0("plots/beliefs", i, ".png"))
        ggsave(stim_choicepattern(i),
               file = paste0("plots/choicepattern", i, ".png"))
        ggsave(ordobs_calibrationcheck(i) - bystim_beliefcloud(i),
               file = paste0("plots/combocheck", i, ".png"),
               width = 10, height = 10)
        ggsave(ordobs_calibrationcheck(i),
               file = paste0("plots/ordcalibration", i, ".png"))
    }
}
