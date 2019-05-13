library(tidyverse)
library(rstan)
library(shinystan)
theme_set(theme_light())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


rm(list = ls())
set.seed(1)

##ppnt setup
coeff <- 5
theta <- 1

n_options <- 5
features <- rnorm(n_options, 0, 1)
n_obsweeps <- 4

ordobs.df <- data.frame()

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


for (i in 1:n_obsweeps){
    for (opt1 in 2:n_options){
        for (opt2 in 1:opt1){
            ordobs.df <- rbind(ordobs.df,
                               data.frame(
                                   opt1 = opt1,
                                   opt2 = opt2,
                                   obs = ordobs(features[opt1],
                                                features[opt2],
                                                coeff,
                                                theta)
                                   ))
        }
    }
}

datalist <- list(n_obs = nrow(ordobs.df),
                 n_options = n_options,
                 obs = ordobs.df$obs,
                 opt1 = ordobs.df$opt1,
                 opt2 = ordobs.df$opt2,
                 coeff = coeff,
                 theta = theta)

fit <- stan(file = "min.stan",
            data = datalist,
            iter = 2000,
            chains = 4)

samples <- as.data.frame(extract(fit, permuted = TRUE))

recovered <- samples %>% select(starts_with("features")) %>%
    gather(id, value) %>%
    mutate(feature = sapply(id, function(x){
        strsplit(x, "\\.")[[1]][2]
    }
    ))

ggplot(recovered, aes(x = value, color = id)) +
    geom_density() +
    geom_vline(data = data.frame(x = features,
                                 id = paste0("features.", 1:length(features))),
               aes(xintercept = x, color = id))
