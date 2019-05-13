library(tidyverse)
theme_set(theme_light())
rm(list = ls())
set.seed(4)

coeff <- 2 #first try for payoff: crossovers at +/- .5, but 'eq' quite likely out to 1ish?
theta <- 1

#coeff <- 100 #first try for prob: crossovers at +/- .01, eq out to +/- .05
#theta <- 1
which_targdiff <- c("probs", "payoffs")[2]#choose one
tolerance <- 2 #sets limits of xcomb, ie desired vis perspective.

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

hm_trials <- 1000
probdiffs <- rbeta(hm_trials, 1, 1) - rbeta(hm_trials, 1, 1)
payoffdiffs <- rnorm(hm_trials, 100, 5) - rnorm(hm_trials, 100, 5)

targdiff <- if (which_targdiff == "probs"){
                probdiffs
            }else{
                payoffdiffs
            }

xcomb <- seq(from = -tolerance,
             to = tolerance,
             length = length(targdiff)
             )
lt <- sapply(xcomb, p10)
gt <- sapply(xcomb, p01)
eq <- sapply(xcomb, p00)

for (i in 1:hm_trials){
    z <- sum(lt[i], gt[i], eq[i])
    lt[i] <- lt[i] / z
    gt[i] <- gt[i] / z
    eq[i] <- eq[i] / z
}

check.df <- data.frame(x = xcomb,
                       lt = lt,
                       gt = gt,
                       eq = eq)
ggplot(check.df, aes(x = x)) +
    geom_point(aes(y = lt, color = "lt")) +
    geom_point(aes(y = gt, color = "gt")) +
    geom_point(aes(y = eq, color = "eq")) +
    ggtitle(paste(which_targdiff, "coeff", coeff, "theta", theta))
