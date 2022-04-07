
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
  filter(subject != 126) #%>% # GoWin accuracy 0
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

# clean the space
rm(tmp); rm(Tsubj); rm(b); rm(cue); rm(cues); rm(curSubj);rm(i); rm(maxTrials);rm(outcome); rm(pressed); rm(useTrials)

# Load stan model
m4_GoPav_rew.pun <- stan_model("./stan_code/gng_m4_2cond_go_pav_bias_rew.pun.stan")
m4_Pav_pun <- stan_model("./stan_code/gng_m4_2cond_pav_pun.stan")
m4_GoPav_pun <- stan_model("./stan_code/gng_m4_2cond_go_pav_pun.stan")
m4_GoPav_punOnly <- stan_model("./stan_code/gng_m4_2cond_go_pav_pun_only.stan")
m4_GoPav_rewpun_sep <-stan_model("./stan_code/gng_m4_2cond_GoPav_rewpun_sep.stan")

# Set parameter initial value
params_GoPav_rew.pun <- c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')
params_Pav_pun <- c('xi', 'ep','b', 'pi_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')
params_GoPav_pun <- c('xi', 'ep','b_0', 'b_delta','pi_rew','pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')
params_GoPav_pun_only <- c('xi', 'ep','b_0', 'b_delta','pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')
params_GoPav_rewpun_sep <- c('xi', 'ep','b_1', 'b_2','piRew_1','piRew_2', 'piPun_1','piPun_2','rhoRew', 'rhoPun')


params <- params_GoPav_rewpun_sep
pars_init = function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0,num_subj)
  }
  return(ret)
}

# Sampling
print('start running models')
fit_m4_GoPav_rew.pun <- sampling(m4_GoPav_rew.pun, data = dataList,
                                 iter = 4000, warmup = 2000, chains = 4, cores = 4,
                                 pars = c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 
                                          'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun',
                                          'mu_xi', 'mu_ep','mu_b_0', 'mu_b_delta','mu_pi_0_rew','mu_pi_delta_rew', 
                                          'mu_pi_0_pun','mu_pi_delta_pun','mu_rhoRew', 'mu_rhoPun',
                                          'log_lik'),
                                 init = pars_init_GoPav_rew.pun)

fit_m4_Pav_pun <- sampling(m4_Pav_pun, data = dataList,iter = 4000, warmup = 2000, chains = 4, cores = 4,
                           pars = c('xi', 'ep','b', 'pi_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun',
                                    'mu_xi', 'mu_ep','mu_b', 'mu_pi_rew', 'mu_pi_0_pun','mu_pi_delta_pun','mu_rhoRew', 'mu_rhoPun',
                                    'log_lik'),init = pars_init)

fit_m4_GoPav_pun <- sampling(m4_GoPav_pun, data = dataList,iter = 4000, warmup = 2000, chains = 4, cores = 4,
                           pars = c('xi', 'ep','b_0', 'b_delta', 'pi_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun',
                                    'mu_xi', 'mu_ep','mu_b_0','mu_b_delta', 'mu_pi_rew', 'mu_pi_0_pun','mu_pi_delta_pun','mu_rhoRew', 'mu_rhoPun',
                                    'log_lik'), init = pars_init)

fit_m4_GoPav_pun_only <- sampling(m4_GoPav_punOnly, data = dataList,iter = 4000, warmup = 2000, chains = 4, cores = 4,
                             pars = c('xi', 'ep','b_0', 'b_delta', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun',
                                      'mu_xi', 'mu_ep','mu_b_0','mu_b_delta', 'mu_pi_0_pun','mu_pi_delta_pun','mu_rhoRew', 'mu_rhoPun',
                                      'log_lik'), init = pars_init)

fit_m4_GoPav_rewpun_sep <- sampling(m4_GoPav_rewpun_sep, data = dataList,
                                 iter = 4000, warmup = 2000, chains = 4, cores = 4,
                                 pars = c('xi', 'ep','b_1', 'b_2','piRew_1','piRew_2', 
                                          'piPun_1','piPun_2','rhoRew', 'rhoPun',
                                          'mu_xi', 'mu_ep','mu_b_1', 'mu_b_2','mu_piRew_1','mu_piRew_2', 
                                          'mu_piPun_1','mu_piPun_2','mu_rhoRew', 'mu_rhoPun',
                                          'log_lik'),
                                 init = pars_init)



# Save output
save(fit_m4_GoPav_rew.pun, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_m4_rew.pun_iter4000_prior-fixed.RData')
save(fit_m4_Pav_pun, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_Pav_pun_iter4000_prior-fixed.RData')
save(fit_m4_GoPav_pun, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_GoPav_pun.RData')
save(fit_m4_GoPav_pun_only, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_GoPav_pun_only.RData')
save(fit_m4_GoPav_rewpun_sep, file ='/data2/project_BRL/project_tDCS/behav/modeling/fit_RData/fit_m4_GoPav_rewpun_sep.RData')

