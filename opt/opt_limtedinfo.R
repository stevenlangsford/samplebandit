library(tidyverse)
library(patchwork)
theme_set(theme_light())

set.seed(4)
rm(list = ls())

##settable: world params
##feature distributions:
for (mypaysd in seq(from = 1, to = 50, length = 50)){
    print(paste("progress:", mypaysd, "50"))
rnd_prob <- function(){
    runif(1, 0, 1);
}
rnd_payoff <- function(){
    rnorm(1, 100, mypaysd);
}
##Expectation if feature unknown:
blind_prob <- .5 #E(rnd_prob)
blind_payoff <- 20 #E(rnd_payoff)

##How many sim trials to run
n_stim <- 200

##End settables: MAIN

rnd_trial <- function(){
    list(c(rnd_prob(), rnd_payoff()),
         c(rnd_prob(), rnd_payoff()),
         c(rnd_prob(), rnd_payoff())
         )
}

get_stim <- function(n){
    ##Format convention:
    ##stim is a matrix,
    ##rows are trials, cols are options,
    ##an option is c(prob, payoff)
    t(replicate(n, rnd_trial()))
}

stim <- get_stim(n_stim);
if (length(unlist(stim[1, ])) != 6){
    stop("3 options of 2 features assumption broken")
}

get_choice <- function(trial, infostate){
    if (!infostate[1])trial[[1]][1] <- blind_prob
    if (!infostate[2])trial[[1]][2] <- blind_payoff
    if (!infostate[3])trial[[2]][1] <- blind_prob
    if (!infostate[4])trial[[2]][2] <- blind_payoff
    if (!infostate[5])trial[[3]][1] <- blind_prob
    if (!infostate[6])trial[[3]][2] <- blind_payoff

    values <- unlist(map(trial, prod))
    max_at <- which(values == max(values))
    if (length(max_at) == 1){
        ##gotta check length to avoid base::sample weirdness. :-(
        return(max_at)
    }else{
        ##break ties at random
        return(base::sample(max_at, 1))
    }
}

sim.df <- data.frame()
binary <- function(x){
    if (all(x < 2)) x else paste(binary(x %/% 2), x %% 2, sep = "")
}
lpad <- function(x, n){
    x <- as.character(x)
    while (nchar(x) < n){
        x <- paste0("0", x)
    }
    return(x)
}
get_infostate <- function(id){
    ##magic number 6 is number of features in a trial.
    ##lpad(binary)->as.numeric->as.logical avoids writing out 64 infomasks :-P
    sapply(unlist(strsplit(lpad(binary(id), 6), split = NULL)),
           function(x){
               as.logical(as.numeric(x))
           })
}
infostate_group <- function(id){
    mymask <- get_infostate(id)
    totalinfo <- sum(mymask)
    prob_obs <- sum(mymask[c(1,3,5)])
    pay_obs <-  sum(mymask[c(2,4,6)])
    return(paste0(totalinfo, "(", prob_obs, ":", pay_obs, ")"))
}

for (i in 1:nrow(stim)){
#    print(paste("progress:", i, "of", nrow(stim)))
    for (amask in 0:(2 ^ 6 - 1)){
        sim.df <- rbind(sim.df,
                        data.frame(stim = i,
                                   info = amask,
                                   choice = get_choice(stim[i, ],
                                                       get_infostate(amask)
                                                       )
                                   )
                        )
    }
}
sim.df$gain <- unlist(map2(sim.df$stim,
                           sim.df$choice,
                           function(mystim, mychoice){
                               return(prod(stim[[mystim, mychoice]]))
                           }
                           )
                      )
sim.df$info_group <- sapply(sim.df$info, infostate_group)

by_info_mask.df <- sim.df %>%
    group_by(info) %>%
    summarize(mean_gain = mean(gain)) %>%
    ungroup() %>%
    mutate(info_group = sapply(info, infostate_group),
           total_obs = substr(info_group, 1, 1),
           prob_obs =  substr(info_group, 3, 3)
           )

by_info_group.df <- sim.df %>%
    group_by(info_group) %>%
    summarize(mean_gain = mean(gain))
perfectscore <- max(by_info_group.df$mean_gain)

ggsave(ggplot(by_info_group.df, aes(x = info_group, y = mean_gain)) +
       geom_point(size = 5) +
       ylim(c(0, perfectscore)),
       file = paste0("plots/infogroup/", lpad(mypaysd, 3), "byinfogroup.png")
       )

ggsave(ggplot(by_info_mask.df, aes(x = info, y = mean_gain)) +
    geom_point(size = 5) +
    facet_grid(prob_obs~total_obs) +
    guides(color = FALSE) +
    ylim(c(0, perfectscore)),
    file = paste0("plots/infomask/", lpad(mypaysd, 3), "byinfomask.png")
    )
}#for each paysd
