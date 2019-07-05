
## ##params
## p_instructions_done <- .3
## p_condition_done <- .8


## value_of_opt <- function(infostate, n_obs){
##     if (n_obs == 0) return(seen_states[[infostate]]$value_now)
##     myaction <- best_action(infostate, n_obs)[1] #tiebreakers are free here.

##     mystate <- seen_states[[infostate]]
##     myvalue <- 0 #stores prob weighted sum of values reachable by opt actions.

##     for (achild in 1:length(mystate$children)){
##         if (length(mystate$children[[achild]]) == 0) next #skip known features
##         if (mystate$children[[achild]]$obs_description == myaction){
##             for (reachable in 1:length(mystate$children[[achild]]$state)){
##                 myvalue <- myvalue +
##                     (value_of_opt(mystate$children[[achild]]$state[reachable],
##                                  n_obs - 1) *
##                     mystate$children[[achild]]$prob[reachable])
##             }
##             ## print(myvalue)
##             ## print(infostate)
##             ## print("***")
##             break #identical actions likely to exist: can only take one.
##         }#if observing achild here matches best action
##     }#for each child
##     return(myvalue)
## }#value of opt




## ##setup fns
## possible_actions <- c()
## for (anaction in action_target){
##     if (str_count(anaction, "_NA") > 0) {
##         possible_actions <- c(possible_actions,
##                               paste(anaction, "observe-payoff")
##                               )
##     }
##         if (str_count(anaction, "NA_") > 0) {
##         possible_actions <- c(possible_actions,
##                               paste(anaction, "observe-prob")
##                               )
##         }
##     possible_actions <- c(possible_actions,
##                           paste(anaction, "choose")
##                           )
## }
## action <- function(){
##     base::sample(possible_actions,1)
## }
## conditions <- c()
## for (acount in 0:3){
##     for (acomparison in c("<", "==", ">")){
##         for (atarget in action_target){
##             conditions <- c(conditions, paste0("str_count(infostate,'",
##                                                atarget, "')",
##                                                acomparison,
##                                                acount))
##         }
##     }
## }

## condition <- function(){
##     if (runif(1, 0, 1) < p_instructions_done){
##         return(base::sample(conditions, 1))
##     }

##     return(paste0(base::sample(conditions, 1),
##                   base::sample(c("&&","||"), 1),
##                   condition()
##                   )
##            )
## }

## instructionset <- function(){
##     if (runif(1, 0, 1) < p_instructions_done){
##         return("done")
##     }

##     return(list(c(condition(), action()), instructionset()))
## }

## #apply_action <- function(infostate,anaction){

## mystate <- seen_states[[names(seen_states)[505]]]
## anaction <- strsplit(anaction, " ")[[1]]

## opt1 <- paste(mystate$myfeatures$p1, mystate$myfeatures$v1, sep = "_")
## opt2 <- paste(mystate$myfeatures$p2, mystate$myfeatures$v2, sep = "_")
## opt3 <- paste(mystate$myfeatures$p3, mystate$myfeatures$v3, sep = "_")

## opt_matches <- sapply(c(opt1, opt2, opt3), function(opt){
##     str_count(opt, anaction) > 0
##     })


## rnd_order <- base::sample(1:3, 3)
## for (i in rnd_order){
##     if (opt_matches[i]){
##         #DO THE ACTION
##     }
## }
## #}


## bob <- instructionset()
