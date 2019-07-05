library(tidyverse)
library(patchwork)
##paper link: https://www.sciencedirect.com/science/article/pii/S0010028505001015

## "Brown and Qian (2004) asked participants to estimate the relative frequencies with which different probabilities occur in the environment, and found that (a) low and high probabilities are rated as occurring most frequently, and (b) high probabilities are rated as occurring more often than low probabilities."

##Eco probability distribution estimated from prob-related phrases in corpus:
b <- .59 # best, 90% var captured in range 0.46 to 0.67
subj_p <- function(p){
    (p ^ b) /
        ( ( (p ^ b) + (1 - p) ^ b) ^ (1 / b))
}

x <- seq(from = 0, to = 1, length = 1000)
prob.df <- data.frame(x = x, p = sapply(x, subj_p))

howfine <- 5
if (nrow(prob.df) %% howfine != 0) stop("nrow(prob.df) %% howfine != 0")
prob.df$bracket <- rep(1:howfine, each = nrow(prob.df) / howfine)


##payoffs. For gains, just a power law: constant*(gainamount^y)
##best y for gains is  -.93, for losses -.96
##values up to 10000, rank of 500 should be about .8 (from Fig. 2)
maxpay <- 1500
minpay <- 1

#this is only vaguely related to Chater 2006 and is probably dumb.
payraw.df <- data.frame( value = minpay:maxpay,
                     freq = sapply(minpay:maxpay, function(x){x ^ -.5}),
                     bracket = rep(1:howfine,
                                   each = length(minpay:maxpay) /
                                       howfine)) %>%
    mutate(freq = freq / sum(freq))

pay.df <- payraw.df %>%
    group_by(bracket) %>%
    summarize(prob = sum(freq), ev = mean(value)) %>%
    ungroup

payoff_levels <- paste0("pay", 1:howfine)
paylevel_prob <- pay.df$prob
paylevel_value <- pay.df$ev

prob_levels <- paste0("prob", 1:howfine)
problevel_prob <- rep(1 / howfine, howfine)
problevel_value <- prob.df %>%
    group_by(bracket) %>%
    summarize(value = mean(p)) %>%
    ungroup %>%
    select(value) %>%
    unlist %>% as.numeric

prob_na_expected <- with(prob.df, sum(x * p) / nrow(prob.df))
pay_na_expected <- with(pay.df, sum(prob * ev) / nrow(pay.df))

value_na_na <- prob_na_expected * pay_na_expected

##ggsave(
ggplot(payraw.df, aes(x = value, y = freq, color = as.character(bracket))) +
    geom_point() +
    xlab("payoff feature") +
    ylab("frequency") +
    guides(color = FALSE) +
    ggtitle("Payoffs (bins by color)") +
ggplot(prob.df, aes(x = x, y = p / nrow(prob.df))) +
    geom_point(aes(color = as.character(bracket)), alpha = .5) +
    ggtitle(paste("Probabilities (bins by color)")) +
    xlab("probability feature") +
    ylab("frequency") +
    guides(color = FALSE)
##,
##file = "eco_world.png")

##This is the setup you have to mimic to run goldenline.R:
## prob_levels <- c("lowprob", "midprob", "highprob")
## problevel_prob <- c(.4, .2, .4)
## problevel_value <- c(.25, .5, .75)

## payoff_levels <- c("lowpay", "midpay", "highpay")
## paylevel_prob <- c(.1, .8, .1)
## paylevel_value <- c(13, 20, 27)
## prob_na_expected <- .5
## pay_na_expected <- 20
## value_na_na <- 10
