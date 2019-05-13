library(tidyverse)
library(patchwork)
theme_set(theme_light())

##get df of stim for plotting:
stim.df <- data.frame()
for (atrial in 1:nrow(trials)){
stim.df <- stim.df %>% rbind(data.frame(option = as.factor(1:3),
                      prob = sapply(trials[atrial,], function(x){
                          x[1]
                      }),
                      payoff = sapply(trials[atrial,], function(x){
                          x[2]
                      }),
                      trial = atrial
                      ))
}
stim.df$value <- with(stim.df, prob * payoff)

#plots!

vis_stim <- function(trialids){
    ggplot(stim.df %>% filter(trial %in% trialids),
           aes(x = prob,
               y = payoff,
               color = option)) +
        geom_point(size = 5) +
        facet_wrap(~trial)
}

#vis_stim(1:3) #ok.

estvals <- samples %>%
    select(starts_with("estval")) %>%
    gather(id, value) %>%
    mutate(
        trial = sapply(id, function(x){
        as.numeric(strsplit(x, "\\.")[[1]][2])
        }),
        option = sapply(id, function(x){
        as.numeric(strsplit(x, "\\.")[[1]][3])
        })
    )


ggplot(estvals, aes(x = value, color =  as.factor(option))) +
    geom_density() +
    geom_vline(data = stim.df,
               aes(xintercept = value, color = as.factor(option))) +
    facet_wrap(.~trial)
