
rm(list=ls())

# Load library
library(tidyverse)
library(rstan)
library(hBayesDM)
library(loo)
library(stringr)
library(reshape2)

# Set work env
setwd('/home/kimhj9502/Project/project_tDCS/analysis/modeling')
DATA_DIR = '/data2/project_BRL/project_tDCS/behav/data'

# Load data
rawdata <- read.table(str_glue('{DATA_DIR}/all_2102.txt') , header=T, sep = '\t')

# Exclude
data = rawdata %>% filter(subject != 4) %>% # IRB issue
  filter(subject != 14) %>% # IRB issue
  filter(subject != 15) %>% # sleep in ses-2
  filter(subject != 27) %>% # sleep in ses-2 # GoWin accuracy 0.06
  filter(subject != 113) %>% # sleep
  filter(subject != 61) %>% # GoWin accuracy 0.04
  filter(subject != 94) %>% # GoWin accuracy 0
  filter(subject != 102) %>% # GoWin accuracy 0.02
  filter(subject != 126) # GoWin accuracy 0
  #filter(subject != 18) %>% # model fitting issue
  #filter(subject != 125) # model fitting issue

colnames(data)
names(data)[names(data) == "subject"] <- "subjID"
names(data)[names(data) == "key_pressed"] <- "keyPressed"

subj_list <- unique(data$subjID)
num_subj <- length(subj_list)
print(subj_list); print(num_subj)

# Make datalist
Tsubj <- array(180, c(num_subj, 2)) # number of trials for each subject

maxTrials <- max(Tsubj)

outcome <- array(0, c(num_subj, maxTrials, 2) )
pressed <- array(-1, c(num_subj, maxTrials, 2) )
cue     <- array(1, c(num_subj, maxTrials, 2))

for (i in 1:num_subj) {
  curSubj <- subj_list[i]
  for (b in 1:2) {
    useTrials    <- Tsubj[i,b]
    tmp <- subset(data, subjID == curSubj & db == b)
    cues <- str_replace_all(tmp$cond, c("nogo_reward" = "2", "go_reward" = "1", "nogo_punish" = "3", "go_punish" = "4"))
    if (dim(tmp)[1] > 0){
      outcome[i, 1:useTrials, b] <- tmp$outcome
      pressed[i, 1:useTrials, b] <- tmp$keyPressed
      cue[i, 1:useTrials, b]     <- as.numeric(as.factor(as.numeric(cues)))
    }
  }
}

dataList <- list(
  N       = num_subj,
  T       = maxTrials,
  B       = 2,
  Tsubj   = Tsubj,
  outcome = outcome,
  pressed = pressed,
  cue     = cue
)

# Load stan model
m5 <- stan_model("./stan_code/m5.stan")
m6 <- stan_model("./stan_code/m6.stan")
m7 <- stan_model("./stan_code/m7.stan")
m8 <- stan_model("./stan_code/m8.stan")

# Set parameter initial value
params_m5 <- c('xi', 'ep','b', 'pi_0_rew','pi_delta_rew', 'rhoRew', 'rhoPun')
params_m6 <- c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 'rhoRew', 'rhoPun')
params_m7 <- c('xi', 'ep','b', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')
params_m8 <- c('xi', 'ep','b_0', 'b_delta','pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')

pars_init_m5 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m5))
  ret[['sigma']] <- rep(0.5, length(params_m5))
  for (param in params_m5) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_m6 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m6))
  ret[['sigma']] <- rep(0.5, length(params_m6))
  for (param in params_m6) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_m7 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m7))
  ret[['sigma']] <- rep(0.5, length(params_m7))
  for (param in params_m7) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_m8 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m8))
  ret[['sigma']] <- rep(0.5, length(params_m8))
  for (param in params_m8) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

# Sampling
print('start running models')
fit_m5 <- sampling(m5, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m5)
fit_m6 <- sampling(m6, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m6)
fit_m7 <- sampling(m7, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m7)
fit_m8 <- sampling(m8, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m8)

# Save output
save(fit_m5, fit_m6, fit_m7, fit_m8, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_c1.RData')

