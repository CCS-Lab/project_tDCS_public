
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

# Load data
rawdata = read.table(str_glue('{DATA_DIR}/all_2106.txt') , header=T, sep = '\t')

# Make accuracy data
rawdata_subj <- rawdata %>% group_by(subject, cond, db) %>% summarise(accuracy = mean(is_correct))

# Check go_reward = 0 subjects
go_reward <- rawdata_subj %>% filter(cond == 'go_reward') %>% filter(accuracy < 0.1)

# First session & 2nd session only
## IRB
data = rawdata %>% filter(subject != 4) %>% # IRB issue
  filter(subject != 14)

## N = 31
anode_first = c(28, 18, 33, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127, 126) # 116
# go_reward < 0.1 : 61
# data qual: 103
sham_first = c(15, 30, 54, 43, 11, 27, 73, 79, 101, 108, 110, 104, 125, 69)
# go_reward < 0.1 : 94, 102
# data qual : 113

anode_second = c(30, 54, 43, 11, 94, 73, 79, 101, 102, 108, 110, 104, 125) 
# go_reward < 0.1 : 27
# data qual: 15, 113
sham_second = c(28, 18, 33, 61, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127) # 126
# go_reward < 0.1 : 126
# data qual : x

## Sham
data_sham <- data %>% filter(db == 1) %>%
  filter(subject %in% c(sham_first, sham_second))

## Anode
data_anode <- data %>% filter(db == 2) %>%
  filter(subject %in% c(anode_first, anode_second))

data <- rbind(data_sham, data_anode)

# 
colnames(data)
names(data)[names(data) == "subject"] <- "subjID"
names(data)[names(data) == "key_pressed"] <- "keyPressed"

## N = 27
anode_first = c(28, 18, 33, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127, 126, 103) # 116
# go_reward < 0.1 : 61
sham_first = c(15, 30, 54, 43, 11, 27, 73, 79, 101, 108, 110, 104, 125, 69)
# go_reward < 0.1 : 94, 102, 126
# data qual : 113

## Sham
data_sham <- data %>% filter(db == 1)
## Anode
data_anode <- data %>% filter(db == 2)

# Sham

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
params_nod <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun')
params_pi_rp <- c('xi', 'ep','b','piRew', 'piPun', 'rhoRew', 'rhoPun')


params_k_sep <- c('xi', 'ep','epRew', 'epPun','b', 'pi','rhoRew', 'rhoPun')
params_k_sep_r <- c('xi', 'ep','epRew','b', 'pi','rhoRew', 'rhoPun')
params_k_sep_rho <- c('xi', 'ep','epRew', 'epPun','b', 'pi','rho')
params_k_sep_r_rho <- c('xi', 'ep','epRew','b', 'pi','rho')
params_k_boer <- c('xi', 'ep','epRew', 'b', 'rhoRew', 'rhoPun')
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
gng_m4_k_nopi <- stan_model("./stan_code/gng_m4_k_nopi.stan")
gng_m4_k_rp_nopi <- stan_model("./stan_code/gng_m4_k-rp_nopi.stan")
gng_m4_k_sep <- stan_model("./stan_code/gng_m4_k_sep.stan")
gng_m4_k_sep_r <- stan_model("./stan_code/gng_m4_k_sep-r.stan")
gng_m4_boer <- stan_model("./stan_code/gng_m4_k_boer.stan")
gng_m4_k_sep_rho <- stan_model("./stan_code/gng_m4_k_sep_rho.stan")
gng_m4_k_sep_r_rho <- stan_model("./stan_code/gng_m4_k_sep-r_rho.stan")
gng_m4_pi_rp_rho <- stan_model("./stan_code/gng_m4_pi-rp_rho.stan")


gng_m4_k_rp <- stan_model("./stan_code/gng_m4_k-rp.stan") # 44
gng_m4_pi_rp_k_rp <- stan_model("./stan_code/gng_m4_pi-rp_k-rp.stan") # 50

save(fit_m4_pi_rp_sham,fit_m4_k_nopi_sham, fit_m4_k_rp_nopi_sham, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tmp.RData')
save(fit_m4_k_rp_sham, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tmp_k_rp.RData')
save(fit_m4_pi_rp_k_rp_sham, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tmp_pi_rp_k_rp.RData')
save(fit_m4_pi_rp_k_sham, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tmp_pi_rp_k.RData')
save(fit_m4_pi_rp_sham, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tmp_pi_rp-fixed.RData')
save(fit_m4_pi_rp_sham, fit_m4_pi_rp_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_pi_rp_iter4000.RData')
save(fit_m4_k_sep_sham, fit_m4_k_sep_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_k_sep.RData')


save(c05_sham, c06_sham,c07_sham,c08_sham,c09_sham,c010_sham,
     file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tmp2.RData')



## Sampling
fit_m4_sham = gng_m4(data=data_sham, niter=4000, nwarmup=2000, nchain=4, ncore=4) #, init = pars_init)
fit_m3_sham = gng_m3(data=data_sham, niter=4000, nwarmup=2000, nchain=4, ncore=4) #, init = pars_init)
fit_m3_anode = gng_m3(data=data_anode, niter=4000, nwarmup=2000, nchain=4, ncore=4) #, init = pars_init)

fit_m4_k_sham <- sampling(gng_m4_k, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
fit_m3_k_sham <- sampling(gng_m3_k, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)

fit_m4_pi_rp_sham <- sampling(gng_m4_pi_rp, data = dataList_sham, iter = 4000, warmup = 2000, chains = 4, cores = 4, init = pars_init)
fit_m4_k_nopi_sham <- sampling(gng_m4_k_nopi, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
fit_m4_k_rp_nopi_sham <- sampling(gng_m4_k_rp_nopi, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)

fit_m4_k_rp_sham <- sampling(gng_m4_k_rp, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
fit_m4_pi_rp_k_rp_sham <- sampling(gng_m4_pi_rp_k_rp, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
fit_m4_pi_rp_k_sham <- sampling(gng_m4_pi_rp_k, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)

fit_m4_k_sep_sham <- sampling(gng_m4_k_sep,
                              data = dataList,
                              pars = c('xi', 'ep','epRew', 'epPun',
                                       'b', 'pi','rhoRew', 'rhoPun',
                                       'mu_xi', 'mu_ep','mu_epRew', 'mu_epPun',
                                       'mu_b', 'mu_pi','mu_rhoRew', 'mu_rhoPun',
                                       'log_lik'),
                              init = pars_init,
                              iter = 4000, warmup = 2000, chains = 4, cores = 4)

fit_m4_boer_sham <- sampling(gng_m4_boer,
                             data = dataList,
                             pars = c('xi', 'ep','epRew',
                                      'b', 'rhoRew', 'rhoPun',
                                      'mu_xi', 'mu_ep','mu_epRew',
                                      'mu_b', 'mu_rhoRew', 'mu_rhoPun',
                                      'log_lik'),
                             init = pars_init,
                             iter = 4000, warmup = 2000, chains = 4, cores = 4)


fit_m4_k_sep_sham <- sampling(gng_m4_k_sep, data = dataList, iter = 4000, warmup = 2000, chains = 4, cores = 4)
fit_m4_k_sep_rho_sham <- sampling(gng_m4_k_sep_rho, data = dataList,
                              pars = c('xi', 'ep','epRew','epPun', 'b','pi','rho',
                                       'mu_xi', 'mu_ep','mu_epRew', 'mu_epPun','mu_b', 'mu_rho',
                                       'log_lik'),
                              init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)

fit_m4_k_sep_r_rho_sham <- sampling(gng_m4_k_sep_r_rho, data = dataList,
                                  pars = c('xi', 'ep','epRew','b','pi','rho',
                                           'mu_xi', 'mu_ep','mu_epRew', 'mu_b', 'mu_rho',
                                           'log_lik'),
                                  init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)

fit_m4_pi_rp_rho_sham <- sampling(gng_m4_pi_rp_rho, data = dataList,
                                    pars = c('xi', 'ep','b','piRew', 'piPun', 'rho',
                                             'mu_xi', 'mu_ep', 'mu_b', 'mu_piRew', 'mu_piPun','mu_rho',
                                             'log_lik'),
                                    init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)

ext_m4_k_sham <- extract(fit_m4_k_sham)
mean(ext_m4_k_sham$mu_k)

# Anode
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

#data <- data_anode

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

## param init
params_m4_k <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun', 'k')
params_m3_k <- c('xi', 'ep','b','pi','rho', 'k')
params_rp_rho <- c('xi', 'epRew','epPun','b','pi','rho')
params <- params_rp_rho
pars_init = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

fit_m4_anode = gng_m4(data=data, niter=4000, nwarmup=2000, nchain=4, ncore=4)#, init = pars_init)
fit_m4_k_anode <- sampling(gng_m4_k, data = dataList, iter = 4000, warmup = 2000, chains = 4, cores = 4, init = pars_init)
fit_m3_k_anode <- sampling(gng_m3_k, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
c07_anode <- sampling(gng_m4_pi_rp_k, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
c08_anode <- sampling(gng_m4_k_rp, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
fit_m4_pi_rp_anode <- sampling(gng_m4_pi_rp, data = dataList, iter = 4000, warmup = 2000, chains = 4, cores = 4, init = pars_init)
fit_m4_k_idv_anode <- sampling(gng_m4_k_idv, data = dataList, iter = 4000, warmup = 2000, chains = 4, cores = 4, init = pars_init)
fit_m4_k_pm1_anode <- sampling(gng_m4_k_pm1, data = dataList, iter = 2000, warmup = 1000, chains = 4, cores = 4, init = pars_init)
fit_m4_k_swart_anode <- sampling(gng_m4_k_swart, data = dataList, iter = 4000, warmup = 2000, chains = 4, cores = 4) #, init = pars_init)
fit_m4_k_comb_anode <- sampling(gng_m4_k_comb, data = dataList, iter = 4000, warmup = 2000, chains = 4, cores = 4)
fit_m4_k_sep_anode<- sampling(gng_m4_k_sep,
                              data = dataList,
                              pars = c('xi', 'ep','epRew', 'epPun',
                                       'b', 'pi','rhoRew', 'rhoPun',
                                       'mu_xi', 'mu_ep','mu_epRew', 'mu_epPun',
                                       'mu_b', 'mu_pi','mu_rhoRew', 'mu_rhoPun', 'log_lik'),
                              iter = 4000, warmup = 2000, chains = 4, cores = 4)
fit_m4_boer_anode <- sampling(gng_m4_boer,
                             data = dataList,
                             pars = c('xi', 'ep','epRew',
                                      'b', 'rhoRew', 'rhoPun',
                                      'mu_xi', 'mu_ep','mu_epRew',
                                      'mu_b', 'mu_rhoRew', 'mu_rhoPun',
                                      'log_lik'),
                             iter = 4000, warmup = 2000, chains = 4, cores = 4)
fit_m4_k_sep_r_anode <- sampling(gng_m4_k_sep_r, data = dataList,
                                pars = c('xi', 'ep','epRew','b','pi','rhoRew', 'rhoPun',
                                         'mu_xi', 'mu_ep','mu_epRew', 'mu_b', 'mu_rhoRew', 'mu_rhoPun',
                                         'log_lik'),
                                init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)
fit_m4_k_sep_rho_anode <- sampling(gng_m4_k_sep_rho, data = dataList,
                                  pars = c('xi', 'ep','epRew','epPun', 'b','pi','rho',
                                           'mu_xi', 'mu_ep','mu_epRew', 'mu_epPun','mu_b', 'mu_rho',
                                           'log_lik'),
                                  init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)

fit_m4_k_sep_rho_r_anode <- sampling(gng_m4_k_sep_r_rho, data = dataList,
                                   pars = c('xi', 'ep','epRew','b','pi','rho',
                                            'mu_xi', 'mu_ep','mu_epRew', 'mu_b', 'mu_rho',
                                            'log_lik'),
                                   init = pars_init, iter = 4000, warmup = 2000, chains = 4, cores = 4)

fit_m4_pi_rp_rho_anode <- sampling(gng_m4_pi_rp_rho, data = dataList_anode,
                                  pars = c('xi', 'ep','b','piRew', 'piPun', 'rho',
                                           'mu_xi', 'mu_ep', 'mu_b', 'mu_piRew', 'mu_piPun','mu_rho',
                                           'log_lik'),
                                  iter = 4000, warmup = 2000, chains = 4, cores = 4)


# Save output
save(fit_m4_sham, fit_m4_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_tdcs_1stonly.RData')
save(fit_m3_sham, fit_m3_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m3_tdcs_1stonly.RData')
save(fit_m4_sham, fit_m4_anode, fit_m3_k_sham, fit_m3_sham, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_c0_1.RData')
save(fit_m4_k_sham,fit_m4_k_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_m4_k.RData')
save(c07_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_c07_anode.RData')
save(fit_m4_k_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_k_anode_sig1all_iter4000.RData')
save(fit_m4_k_idv_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_k_idv_anode_sig1all.RData')
save(fit_m4_k_sep_r_sham, fit_m4_k_sep_r_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_k_sep_r_1stonly.RData')
save(fit_m4_boer_sham, fit_m4_boer_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_boer.RData')
save(fit_m4_k_sep_rho_sham, fit_m4_k_sep_rho_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_sep_rho.RData')
save(fit_m4_k_sep_r_rho_sham, fit_m4_k_sep_r_rho_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_sep_r_rho.RData')
save(fit_m4_pi_rp_rho_sham, fit_m4_pi_rp_rho_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_pi_rp_rho_1stonly.RData')
save(fit_m4_pi_rp_sham, fit_m4_pi_rp_anode, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_tdcs_pi_rp_1stonly.RData')
