rm(list=ls())
path <- "greedy_flipbook/obsdump/"
as.numeric(strsplit(tail(list.files(path,pattern="ord"),1)," ")[[1]][[2]])
       
ordobs.df <- data.frame()
try({ordobs.df<<-read.csv(paste0(path,tail(list.files(path,pattern="ord"),1)))})

calcobs.df <- data.frame()
try({calcobs.df <<- read.csv(paste0(path,tail(list.files(path,pattern="calc"),1)))})

print("kept goin")
