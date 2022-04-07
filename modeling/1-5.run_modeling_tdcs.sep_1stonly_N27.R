
rm(list=ls())

# Load library
library(tidyverse)
library(rstan)
library(hBayesDM)
library(loo)
library(stringr)
library(reshape2)

# https://docs.google.com/spreadsheets/d/1zBnkRBbBJrOVvdCj651VyQ0Lz5CHHQGLCAlXUt_OPog/edit#gid=0

# Set work env
setwd('/home/kimhj9502/Project/project_tDCS/analysis/modeling')
DATA_DIR = '/data2/project_BRL/project_tDCS/behav/data'
SAVE_DIR = '/data2/project_BRL/project_tDCS/behav/modeling'

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

data = data %>% filter(ses==1)

colnames(data)
names(data)[names(data) == "subject"] <- "subjID"
names(data)[names(data) == "key_pressed"] <- "keyPressed"


# Sham
data_sham = data %>% filter(db == 1)

## subject list
subj_list <- unique(data_sham$subjID)
num_subj <- length(subj_list)
print(subj_list); print(num_subj)

# Make datalist
Tsubj <- array(180, num_subj) # number of trials for each subject

maxTrials <- max(Tsubj)

outcome <- array(0, c(num_subj, maxTrials) )
pressed <- array(-1, c(num_subj, maxTrials) )
cue     <- array(1, c(num_subj, maxTrials))

for (i in 1:num_subj) {
  curSubj <- subj_list[i]
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
  N       = num_subj,
  T       = maxTrials,
  Tsubj   = Tsubj,
  outcome = outcome,
  pressed = pressed,
  cue     = cue
)

## param init
params_m3 <- c('xi', 'ep','b','pi','rho')
params_m4 <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun')
params_pi_rp <- c('xi', 'ep','b','piRew', 'piPun', 'rhoRew', 'rhoPun')
params_pi_rp_rho <- c('xi', 'ep','b','piRew', 'piPun', 'rho')

params <- params_pi_rp
pars_init = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

## Load models
gng_m4_pi_rp <- stan_model("./stan_code/gng_m4_pi-rp.stan")
gng_m4_pi_rp_rho <- stan_model("./stan_code/gng_m4_pi-rp_rho.stan")

## Sampling
fit_m4_sham = gng_m4(data=data_sham, niter=4000, nwarmup=2000, nchain=4, ncore=4) #, init = pars_init)
fit_m3_anode = gng_m3(data=data_anode, niter=4000, nwarmup=2000, nchain=4, ncore=4) #, init = pars_init)

fit_m4_pi_rp_anode <- sampling(gng_m4_pi_rp, data = dataList,
                              pars = c('xi', 'ep','b','piRew', 'piPun', 'rhoRew', 'rhoPun',
                                       'mu_xi', 'mu_ep', 'mu_b', 'mu_piRew', 'mu_piPun','mu_rhoRew','mu_rhoPun',
                                       'log_lik'),
                              iter = 4000, warmup = 2000, chains = 4, cores = 4, init = pars_init)
fit_m4_pi_rp_rho_sham <- sampling(gng_m4_pi_rp_rho, data = dataList,
                                    pars = c('xi', 'ep','b','piRew', 'piPun', 'rho',
                                             'mu_xi', 'mu_ep', 'mu_b', 'mu_piRew', 'mu_piPun','mu_rho',
                                             'log_lik'),
                                    init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)

# LOOIC
loo(extract_log_lik(fit_m4_pi_rp_sham))
loo(extract_log_lik(fit_m4_pi_rp_anode))
loo(extract_log_lik(fit_m4_sham$fit))
loo(extract_log_lik(fit_m3_sham$fit))

# Plot (sanity check)
params <- params_m3
trace_list = list()
for (n in 1:length(params)){
  p = traceplot(fit_m3_anode$fit, pars=c(params[n]))
  #p = traceplot(fit_m4_pi_rp_anode, pars=c(params[n]))
  trace_list[[n]] = p
}

pdf(str_glue("{SAVE_DIR}/estimate_plots/sep/traceplots_m3_1stonly_anode_N16.pdf"))
for (j in 1:length(trace_list)) {
  print(trace_list[[j]])
}
dev.off()


# Anode
data_anode = data %>% filter(db == 2)

# To test idv model
#data_anode = data_anode %>% filter(subjID == 28 | subjID == 18)

## subject list
subj_list <- unique(data_anode$subjID)
num_subj <- length(subj_list)
print(subj_list); print(num_subj)

# Make datalist
Tsubj <- array(180, num_subj) # number of trials for each subject

maxTrials <- max(Tsubj)

outcome <- array(0, c(num_subj, maxTrials) )
pressed <- array(-1, c(num_subj, maxTrials) )
cue     <- array(1, c(num_subj, maxTrials))

for (i in 1:num_subj) {
  curSubj <- subj_list[i]
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
  N       = num_subj,
  T       = maxTrials,
  Tsubj   = Tsubj,
  outcome = outcome,
  pressed = pressed,
  cue     = cue
)

# Save output
save(fit_m3_sham, fit_m3_anode, fit_m4_sham, fit_m4_anode,
     fit_m4_pi_rp_sham, fit_m4_pi_rp_anode,
     file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_tdcs_1stonly_N27.RData')
