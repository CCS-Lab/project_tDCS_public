
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
    if (dim(tmp)[1] > 0){
      outcome[i, 1:useTrials, b] <- tmp$outcome
      pressed[i, 1:useTrials, b] <- tmp$keyPressed
      cue[i, 1:useTrials, b]     <- as.numeric(as.factor(tmp$cue))
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
m4_nod <- stan_model("./stan_code/gng_m4_2cond_NoDiff.stan")

m4_Go <- stan_model("./stan_code/gng_m4_2cond_goBias.stan")
m4_Pav <- stan_model("./stan_code/gng_m4_2cond_pavBias.stan")
m4_GoPav <- stan_model("./stan_code/gng_m4_2cond_go_pav_bias.stan")

m4_Go_sep <- stan_model("./stan_code/gng_m4_2cond_goBias_sep.stan")
m4_Pav_sep <- stan_model("./stan_code/gng_m4_2cond_pavBias_sep.stan")
m4_GoPav_sep <- stan_model("./stan_code/gng_m4_2cond_go_pav_bias_sep.stan")

# Set parameter initial value
params_nod <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun')

params_Go <- c('xi', 'ep','b_0','b_delta','pi','rhoRew', 'rhoPun')
params_Pav <- c('xi', 'ep','b','pi_0','pi_delta','rhoRew', 'rhoPun')
params_GoPav <- c('xi', 'ep','b_0', 'b_delta','pi_0','pi_delta','rhoRew', 'rhoPun')

params_Go_sep <- c('xi', 'ep','b_1','b_2','pi','rhoRew', 'rhoPun')
params_Pav_sep <- c('xi', 'ep','b','pi_1','pi_2','rhoRew', 'rhoPun')
params_GoPav_sep <- c('xi', 'ep','b_1', 'b_2','pi_1','pi_2','rhoRew', 'rhoPun')

pars_init_nod = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_nod))
  ret[['sigma']] <- rep(0.5, length(params_nod))
  for (param in params_nod) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_Go = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_Go))
  ret[['sigma']] <- rep(0.5, length(params_Go))
  for (param in params_Go) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_Pav = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_Pav))
  ret[['sigma']] <- rep(0.5, length(params_Pav))
  for (param in params_Pav) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_GoPav = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_GoPav))
  ret[['sigma']] <- rep(0.5, length(params_GoPav))
  for (param in params_GoPav) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_Go_sep = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_Go_sep))
  ret[['sigma']] <- rep(0.5, length(params_Go_sep))
  for (param in params_Go_sep) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_Pav_sep = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_Pav_sep))
  ret[['sigma']] <- rep(0.5, length(params_Pav_sep))
  for (param in params_Pav_sep) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

pars_init_GoPav_sep = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params_GoPav_sep))
  ret[['sigma']] <- rep(0.5, length(params_GoPav_sep))
  for (param in params_GoPav_sep) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

# Sampling
print('start running models')
print('fit_m4_nod')
fit_m4_nod_qc <- sampling(m4_nod, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_nod)

print('fit_m4')
fit_m4_Go_qc <- sampling(m4_Go, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_Go)
fit_m4_Pav_qc <- sampling(m4_Pav, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_Pav)
fit_m4_GoPav_qc <- sampling(m4_GoPav, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_GoPav)

# Save output
save(fit_m4_nod_qc, fit_m4_Go_qc, fit_m4_Pav_qc, fit_m4_GoPav_qc, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_m4_diff_exp_2102_qc.RData')

# Sampling
print('fit_m4_sep')
fit_m4_Go_sep_qc <- sampling(m4_Go_sep, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_Go_sep)
fit_m4_Pav_sep_qc <- sampling(m4_Pav_sep, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_Pav_sep)
fit_m4_GoPav_sep_qc <- sampling(m4_GoPav_sep, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init_GoPav_sep)

# Save output
save(fit_m4_Go_sep_qc, fit_m4_Pav_sep_qc, fit_m4_GoPav_sep_qc, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_m4_sep_exp_2102_qc.RData')

save(fit_m4_GoPav_sep_qc, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_sep_prior-fixed.RData')
save(fit_m4_GoPav_qc, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_prior-fixed.RData')

