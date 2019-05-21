library(tidyverse)
library(patchwork)
theme_set(theme_light())

rm(list = ls())

ordobs <- function(a, b, tolerance, noise){
    diff <- rnorm(1, a - b, noise)
    if (diff < (-tolerance)) return(1);
    if (abs(diff) < tolerance) return(2);
    if (diff > tolerance) return(3);
}

##local R-sim generated ordobs:
ordobs.df <- data.frame()
a <- -2 #ugly setup + while loop mimics cpp for loop literally.

while (a < 2){
    b <- -2
    while (b < 2){
        for (rep in 1:50){
        ordobs.df <- rbind(ordobs.df, data.frame(
                                          ordobs = ordobs(a, b, .3, .3),
                                          a = a,
                                          b = b,
                                          tolerance = 0.1,
                                          noise = 0.1,
                                          simsource = "R")
                           )
        }
        b <- b + 0.25
    }
    a <- a + 0.25
}
## c generated ordobs from ord_phi.cpp
cpp_ordobs.df <- read.csv("ordphi_output.csv") %>%
    mutate(simsource = "cpp")

allsimobs.df <- rbind(cpp_ordobs.df, ordobs.df)

facet_plot <- ggplot(allsimobs.df,
                  aes(x = ordobs, fill = ordered(ordobs, levels = 1:3))) +
    geom_bar() +
    facet_grid(simsource~as.factor(a - b)) +
    guides(fill = FALSE)

dodge_plot <- ggplot(allsimobs.df,
                     aes(x = ordobs, fill = simsource)) +
    geom_bar(position = "dodge") +
    facet_grid(.~as.factor(a - b)) +
    guides(fill = FALSE)

print(facet_plot / dodge_plot)
