
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
  #filter(subject != 18) %>% # m10 model fitting issue
  #filter(subject != 125) # m10 model fitting issue

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
m14 <- stan_model("./stan_code/m14.stan")
m16 <- stan_model("./stan_code/m16.stan")
m20 <- stan_model("./stan_code/m20.stan")
m8 <- stan_model("./stan_code/m8.stan")

# Set parameter initial value
params_m14 <- c('xi', 'ep','b_0', 'b_delta','pi_0','pi_delta', 'rhoRew', 'rhoPun', 'k_0', 'k_delta')
params_m16 <- c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 'rhoRew', 'rhoPun', 'k_0', 'k_delta')
params_m20 <- c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun', 'k_0', 'k_delta')

pars_init_m14 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m14))
  ret[['sigma']] <- rep(0.5, length(params_m14))
  for (param in params_m14) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_m16 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m16))
  ret[['sigma']] <- rep(0.5, length(params_m16))
  for (param in params_m16) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_m20 = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_m20))
  ret[['sigma']] <- rep(0.5, length(params_m20))
  for (param in params_m20) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

# Sampling
print('start running models')
fit_m14 <- sampling(m14, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m14)
fit_m16 <- sampling(m16, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m16)
fit_m20 <- sampling(m20, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_m20)

# Save output
save(fit_m14, fit_m16, fit_m20, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_c2.RData')
save(fit_m14, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m14_prior-fixed.RData')

