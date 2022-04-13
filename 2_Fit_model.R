
rm(list=ls())

# Load library
library(tidyverse)
library(rstan)
library(stringr)

set.seed(123)

# Set work dir and data dir
GITDIR = '/home/kimhj9502/Project/project_tDCS_public'
# PATH/TO/YOUR/GIT/project_tDCS_public
setwd(GITDIR)

# Load data
data = read.table(str_glue('{GITDIR}/Data.txt') , header=T, sep = '\t')

# Rename columns
colnames(data)
names(data)[names(data) == "subject"] <- "subjID"
names(data)[names(data) == "key_pressed"] <- "keyPressed"

## Sham
data_sham <- data %>% filter(tdcs == 1)

## subject list
sham <- unique(data_sham$subjID)
n_sham <- length(sham)

# Make datalist
Tsubj <- array(180, n_sham) # number of trials for each subject
maxTrials <- max(Tsubj)
outcome <- array(0, c(n_sham, maxTrials) )
pressed <- array(-1, c(n_sham, maxTrials) )
cue     <- array(1, c(n_sham, maxTrials))

for (i in 1:n_sham) {
  curSubj <- sham[i]
  useTrials    <- Tsubj[i]
  tmp <- subset(data_sham, subjID == curSubj)
  cues <- str_replace_all(tmp$cond, c("nogo_reward" = "2", "go_reward" = "1", "nogo_punish" = "3", "go_punish" = "4"))
  if (dim(tmp)[1] > 0){
    outcome[i, 1:useTrials] <- tmp$outcome
    pressed[i, 1:useTrials] <- tmp$keyPressed
    cue[i, 1:useTrials]     <- as.numeric(as.factor(as.numeric(cues)))
  }
}

dataList_sham <- list(
  N       = n_sham,
  T       = maxTrials,
  Tsubj   = Tsubj,
  outcome = outcome,
  pressed = pressed,
  cue     = cue
)

## Anode
data_anode <- data %>% filter(tdcs == 2)

## subject list
anode <- unique(data_anode$subjID)
n_anode <- length(anode)

# Make datalist
Tsubj <- array(180, n_anode) # number of trials for each subject
maxTrials <- max(Tsubj)
outcome <- array(0, c(n_anode, maxTrials) )
pressed <- array(-1, c(n_anode, maxTrials) )
cue     <- array(1, c(n_anode, maxTrials))

#data <- data_anode

for (i in 1:n_anode) {
  curSubj <- anode[i]
  useTrials    <- Tsubj[i]
  tmp <- subset(data_anode, subjID == curSubj)
  cues <- str_replace_all(tmp$cond, c("nogo_reward" = "2", "go_reward" = "1", "nogo_punish" = "3", "go_punish" = "4"))
  if (dim(tmp)[1] > 0){
    outcome[i, 1:useTrials] <- tmp$outcome
    pressed[i, 1:useTrials] <- tmp$keyPressed
    cue[i, 1:useTrials]     <- as.numeric(as.factor(as.numeric(cues)))
  }
}

dataList_anode <- list(
  N       = n_anode,
  T       = maxTrials,
  Tsubj   = Tsubj,
  outcome = outcome,
  pressed = pressed,
  cue     = cue
)

# Model fitting
## param init
params_m1 <- c('xi', 'ep','b','pi','rho')
params_m2 <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun')
params_m3 <- c('xi', 'ep','b','piRew', 'piPun', 'rhoRew', 'rhoPun')

pars_init <- function(params, n, chain_id){
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0,n)
  }
  alpha <- chain_id
  return(ret)
}

n_chains <- 4
init_ll_m1_sham <- lapply(1:n_chains, function(id) pars_init(params_m1, n_sham, chain_id = id))
init_ll_m2_sham <- lapply(1:n_chains, function(id) pars_init(params_m2, n_sham, chain_id = id))
init_ll_m3_sham <- lapply(1:n_chains, function(id) pars_init(params_m3, n_sham, chain_id = id))

init_ll_m1_anode <- lapply(1:n_chains, function(id) pars_init(params_m1, n_anode, chain_id = id))
init_ll_m2_anode <- lapply(1:n_chains, function(id) pars_init(params_m2, n_anode, chain_id = id))
init_ll_m3_anode <- lapply(1:n_chains, function(id) pars_init(params_m3, n_anode, chain_id = id))

## Load model
m1 <- stan_model("./Stan/m1.stan")
m2 <- stan_model("./Stan/m2.stan")
m3 <- stan_model("./Stan/m3.stan")

## Sampling
fit_m1_sham <- sampling(m1, data = dataList_sham, iter = 4000, warmup = 2000, 
                        chains = 4, cores = 4, init = init_ll_m1_sham, seed = '1612654857')
fit_m2_sham <- sampling(m2, data = dataList_sham, iter = 4000, warmup = 2000, 
                        chains = 4, cores = 4, init = init_ll_m2_sham, seed = '1612654857')
fit_m3_sham <- sampling(m3, data = dataList_sham, iter = 4000, warmup = 2000, 
                        chains = 4, cores = 4, init = init_ll_m3_sham, seed = '1612654857')

fit_m1_anode <- sampling(m1, data = dataList_anode, iter = 4000, warmup = 2000, 
                         chains = 4, cores = 4, init = init_ll_m1_anode, seed = '1518386713')
fit_m2_anode <- sampling(m2, data = dataList_anode, iter = 4000, warmup = 2000, 
                         chains = 4, cores = 4, init = init_ll_m2_anode, seed = '1518386713')
fit_m3_anode <- sampling(m3, data = dataList_anode, iter = 4000, warmup = 2000, 
                         chains = 4, cores = 4, init = init_ll_m3_anode, seed = '1518386713')

# Save output
save(fit_m1_sham, fit_m2_sham, fit_m3_sham,
     fit_m1_anode, fit_m2_anode, fit_m3_anode, 
     file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_public.RData')
     # PATH/TO/YOUR/SAVE/project_tDCS_public


