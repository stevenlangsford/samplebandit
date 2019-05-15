library(tidyverse)
library(rstan)
library(patchwork)
theme_set(theme_light())

rm(list = ls())

wpts <- c(
 12,
 15,
 20,
 25,
 33,
 25,
 20,
 15,
 20,
 18,
 13,
 10,
 25,
 20,
 15,
 12,
 20,
 18,
 13,
 10,
 33,
 25,
 20,
 15,
 30,
 20,
 18,
 13,
 30,
 20,
 18,
 13
)    

#load("testRun.RData")

