library(tidyverse)
library(rstan)
library(shinystan)
theme_set(theme_light())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


rm(list = ls())
set.seed(1)

##ppnt setup
tolerance <- .1
noise <- .3

n_options <- 5
features <- rnorm(n_options, 0, 1)
n_obsweeps <- 4

ordobs.df <- data.frame()

ordobs <- function(a, b, tolerance, noise){
    diff <- rnorm(1, a - b, noise)
    if (diff < (-tolerance)) return(1);
    if (abs(diff) < tolerance) return(2);
    if (diff > tolerance) return(3);
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
                                                tolerance,
                                                noise)
                                   ))
        }
    }
}

datalist <- list(n_obs = nrow(ordobs.df),
                 n_options = n_options,
                 obs = ordobs.df$obs,
                 opt1 = ordobs.df$opt1,
                 opt2 = ordobs.df$opt2,
                 tolerance = tolerance,
                 noise = noise)

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
