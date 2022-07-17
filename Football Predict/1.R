library(rstan)
library(rstanarm)
library(dplyr)
library(parallel)
library(iterators)
library(foreach)
library(doParallel)
source("rolling-window.R")
source("simpleStan.R")
library(posterior)

library(readxl)
data_all <- read_excel("data.xlsx")
options(mc.cores = parallel::detectCores()) 
rstan_options(auto_write = TRUE) 

df_ob<-r_window(data_all,window=3,pred_week = 1 ,start_week=2,start_year = 8)
fin<-handicap_rdata(df_ob,1)

save(fin,file="1.RData")

table<-odd_match(fin,data_all,window=3,pred_week = 1,start_week=2,start_year = 8)
write.csv(table,"1.csv")

