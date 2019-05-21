library(tidyverse)
library(patchwork)
theme_set(theme_light())


ordobs <- function(a, b, tolerance, noise){
    diff <- rnorm(1, a - b, noise)
    if (diff < (-tolerance)) return(1);
    if (abs(diff) < tolerance) return(2);
    if (diff > tolerance) return(3);
}

ordobs_overview <- function(tolerance,
                            noise,
                            comb_length=20,
                            range_of_interest = c(-2, 2)
                            ){

    ordobs_ltprob <- function(diff, tolerance, noise){
        pnorm(-tolerance, diff, noise)
    }
    ordobs_eqprob <- function(diff, tolerance, noise){
        pnorm(tolerance, diff, noise) - pnorm(-tolerance, diff, noise)
    }
    ordobs_gtprob <- function(diff, tolerance, noise){
        1 - pnorm(tolerance, diff, noise)
    }

    data.frame(comb = seq(from = range_of_interest[1],
                          to = range_of_interest[2],
                          length = comb_length)) %>%
        mutate(lt = sapply(comb, function(x){
            ordobs_ltprob(x, tolerance, noise)
        }),
        eq = sapply(comb, function(x){
            ordobs_eqprob(x, tolerance, noise)
        }),
        gt = sapply(comb, function(x){
            ordobs_gtprob(x, tolerance, noise)
        })) %>%
        gather(key, value, lt:gt) %>%
        mutate(key = ordered(key, levels = c("lt", "eq", "gt"))) %>%
        ggplot(aes(x = key, y = value, fill = key)) +
        geom_bar(stat = "identity") + facet_grid(.~comb) +
        guides(fill = FALSE)
}

ordobs_overview(tolerance = .1, noise = .1, range_of_interest = c(-.5, .5))
