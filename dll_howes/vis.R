library(tidyverse)
library(patchwork)
theme_set(theme_light())
##Assumes main.R ran.
##Does some data manipulation for plot prep: stim to stim.df,
##adds sim choice info to trials.df.

##stim to stim.df:
stim.df <- data.frame()
for (astim in 1:nrow(stim)){
stim.df <- stim.df %>% rbind(data.frame(option = as.factor(1:3),
                      prob = sapply(stim[astim, ], function(x){
                          x[1]
                      }),
                      payoff = sapply(stim[astim, ], function(x){
                          x[2]
                      }),
                      stimid = astim
                      ))
}
stim.df$value <- with(stim.df, prob * payoff)

##Add sim choice obs to trials.df:
if (exists("samples")){
    choice.df <- samples %>% select(starts_with("choice")) %>%
    gather(id, choice) %>%
    mutate( trial = sapply(id, function(x){
        as.numeric(strsplit(x, "\\.")[[1]][2])
    }
    )) %>%
    group_by(trial) %>%
    summarize(choose_A = sum(choice == 1) / n(),
              choose_B = sum(choice == 2) / n(),
              choose_C = sum(choice == 3) / n(),
              bestchoice = which(c(choose_A, choose_B, choose_C) ==
                                 pmax(choose_A, choose_B, choose_C))
              ) %>%
    ungroup()
trials.df <- inner_join(trials.df, choice.df)#implicitly on common col 'trial'
#plots!
}

vis_stim <- function(stimids = 1:max(stim.df$stimid), showlegend=FALSE){
aplot <- ggplot(stim.df %>% filter(stimid %in% stimids),
           aes(x = prob,
               y = payoff,
               color = option)) +
    geom_point(size = 5) +
    facet_wrap(~stimid)
if (!showlegend) aplot <- aplot + guides(color = FALSE)
return(aplot)
}

vis_trialbars <- function(){
ggplot(trials.df) +
    geom_bar(aes(x = 1, y = choose_A, fill = "A"), stat = "identity") +
    geom_bar(aes(x = 2, y = choose_B, fill = "B"), stat = "identity") +
    geom_bar(aes(x = 3, y = choose_C, fill = "C"), stat = "identity") +
    facet_grid(paste0("ppnt", ppnt)~paste0("stim", stim)) +
    guides(fill = FALSE)
}

calcobs_calibrationcheck <- function(){
    ggplot(calcobs.df,
           aes(x = simtruth,
               y = obs,
               color = paste(stim, option))) +
        geom_point() + geom_line(aes(x = simtruth, y = simtruth),
                                 color = "black")
}

ordobs_calibrationcheck <- function(astim){
myobs.df <- ordobs.df %>% filter(
                              stim == astim
                              ) %>%
    mutate(mycontrast = paste0(opt1, opt2))

(ggplot(myobs.df, aes(x = obs, fill = obs == simtruth)) +
    geom_bar(position = "dodge") + guides(fill = FALSE) +
 facet_grid(feature~mycontrast)) / (vis_stim(astim, showlegend = TRUE))
}


##     print(
## vis_stim() /
##     (
##         ggplot(trials.df, aes(x = bestchoice, fill = as.factor(bestchoice))) +
##         geom_bar() + facet_grid(.~stim) +
##         guides(fill = FALSE)
##     )
## )
