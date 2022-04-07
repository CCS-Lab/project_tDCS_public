
rm(list=ls())

# Load library
library(tidyverse)
library(rstan)
library(hBayesDM)
library(loo)
library(stringr)
library(bayesplot)
library(ggthemes)
library(reshape2)

# Set work env
setwd('/home/kimhj9502/Project/project_tDCS/analysis/modeling')
SAVE_DIR = '/data2/project_BRL/project_tDCS/behav/modeling'
N = '18'

# Load data (N = 27)
load(str_glue('{SAVE_DIR}/fit_RData/fit_m4_tdcs.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_tdcs_c0_1.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_tdcs_m4_k.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_tmp2.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_tdcs_k_sep_r.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_tdcs_boer.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_m4_k_sep_sham.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_m4_pi_rp_iter4000.RData')) # SfN N=27 including 2 sesssions
load(str_glue('{SAVE_DIR}/fit_RData/fit_tdcs_sep_rho.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_m3_tdcs_1stonly.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_tdcs_pi_rp_1stonly.RData'))

# Check parameter estimation
## Parameter lists
params_m3 <- c('xi', 'ep','b','pi','rho')
params_nod <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun')
params_k <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun','k')
params_m3 <- c('xi', 'ep','b','pi','rho')

params_c05 <- c('xi', 'ep','b','rhoRew', 'rhoPun', 'k')
params_c06 <- c('xi', 'ep','b','piRew', 'piPun', 'rhoRew', 'rhoPun')
params_c07 <- c('xi', 'ep','b','piRew', 'piPun','rhoRew', 'rhoPun','k')
params_c08 <- c('xi', 'ep','b', 'pi','rhoRew', 'rhoPun','kRew', 'kPun')
params_c09 <- c('xi', 'ep','b','rhoRew', 'rhoPun','kRew', 'kPun')
params_c010 <- c('xi', 'ep','b','piRew', 'piPun','rhoRew', 'rhoPun','kRew', 'kPun')

params_k_sep_r <- c('xi', 'ep','epRew','b', 'pi','rhoRew', 'rhoPun')
params_k_sep <- c('xi', 'ep','epRew','epPun','b', 'pi','rhoRew', 'rhoPun')
params_k_sep_rho <- c('xi', 'ep','epRew','epPun','b', 'pi','rho')
params_boer <- c('xi', 'ep','epRew','b', 'rhoRew', 'rhoPun')
params_GoPav_rew.pun <- c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')
params_GoPav_pun <- c('xi', 'ep','b_0', 'b_delta','pi_rew','pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')


## Save images
fits = c(c05_sham, c06_sham,c07_sham,c08_sham,c09_sham,c010_sham)
fits_n = c('c05_sham', 'c06_sham','c07_sham','c08_sham','c09_sham','c010_sham')
params = list(params_c05,params_c06,params_c07,params_c08,params_c09,params_c010)

# Make plots.
traceplot(m4_noses$fit, pars=c('rhoPun'))

# Sham
for (i in 1:length(fits)){
  print(fits_n[i])
  trace_list = list()
  #for (n in 1:length(params)){
  #  #p = traceplot(fit_m3_sham$fit, pars=c(params[n]))
  #  p = traceplot(fits[n], pars=c(params[n]))
  #  trace_list[[n]] = p
  #}
  for (n in 1:length(params[[i]])){
    p = traceplot(fits[[i]], pars=c(params[[i]][n]))
    trace_list[[n]] = p
  }
  
  pdf(str_glue("{SAVE_DIR}/traceplots_{fits_n[i]}_N{N}_c0_sham.pdf"))
  for (j in 1:length(trace_list)) {
    print(trace_list[[j]])
  }
  dev.off()
}
  
#pdf(str_glue("{SAVE_DIR}/estimate_plots/sep/traceplots_sham_N{N}_prior-fixed.pdf"))
pdf(str_glue("{SAVE_DIR}/estimate_plots/sep/traceplots_m4_rp_1stonly_sham_N{N}.pdf"))
for (j in 1:length(trace_list)) {
  print(trace_list[[j]])
}
dev.off()

# Anode
params <- c('xi', 'ep', 'b','piRew', 'piPun','rho')
params <- params_pi_rp_rho
trace_list = list()
for (n in 1:length(params)){
  #p = traceplot(fit_m4_anode$fit, pars=c(params[n]))
  p = traceplot(fit_m4_pi_rp_rho_anode, pars=c(params[n]))
  trace_list[[n]] = p
}

pdf(str_glue("{SAVE_DIR}/estimate_plots/sep/traceplots_m4_pi_rp_1stonly_anode_N18.pdf"))
for (j in 1:length(trace_list)) {
  print(trace_list[[j]])
}
dev.off()
  
#}
rhat_list = list()
for (n in 1:length(params)){
  q = stan_rhat(fit_m4_sham$fit, params[n])
  rhat_list[[n]] = q
}

pdf(str_glue("{SAVE_DIR}/estimate_plots/sep/rhatplots_sham_N{N}_prior-fixed.pdf"))
for (j in 1:length(rhat_list)) {
  print(rhat_list[[j]])
}
dev.off()

# Anode
rhat_list = list()
for (n in 1:length(params)){
  q = stan_rhat(fit_m4_anode$fit, params[n])
  rhat_list[[n]] = q
}

pdf(str_glue("{SAVE_DIR}/estimate_plots/sep/rhatplots_anode_N{N}_prior-fixed.pdf"))
for (j in 1:length(rhat_list)) {
  print(rhat_list[[j]])
}
dev.off()

# Compare models
#loo(extract_log_lik(fit_m4_nod_qc))
#loo(extract_log_lik(fit_m4_Go_qc))

loo_compare(loo(extract_log_lik(fit_m3_sham$fit)),
            loo(extract_log_lik(fit_m3_k_sham)),
            loo(extract_log_lik(fit_m4_sham$fit)),
            loo(extract_log_lik(fit_m4_k_sham)),
            loo(extract_log_lik(c07_sham)),
            loo(extract_log_lik(c08_sham)))

loo_compare(loo(extract_log_lik(fit_m4_k_anode)),
            loo(extract_log_lik(fit_m4_anode$fit)) )

loo_compare(loo(extract_log_lik(fit_m4_k_sep_r_anode)),
            loo(extract_log_lik(fit_m4_anode$fit)) )

loo_compare(loo(extract_log_lik(fit_m4_sham$fit)),
            loo(extract_log_lik(fit_m4_pi_rp_sham)),
            #loo(extract_log_lik(fit_m4_k_sep_sham)),
            loo(extract_log_lik(fit_m4_k_sep_r_sham))) #,
            #loo(extract_log_lik(fit_m4_boer_sham)))

# fit_m4_GoPav_sep_qc is the lowest but the estimation is bad (trace, r_hat).
# So, fit_m4_GoPav_qc is the best model

# SfN 2021
###theme#####
color_scheme_set("teal")
bayesplot_theme_update(text = element_text(size = 25, family = "sans"))
#############
mcmc_intervals(fit_m4_pi_rp_sham, pars = c('mu_xi','mu_ep') , prob = 0.95, prob_outer = 1) + xlim(0,0.65)
mcmc_intervals(fit_m4_pi_rp_anode, pars = c('mu_xi','mu_ep') , prob = 0.95, prob_outer = 1) + xlim(0,0.65)

mcmc_intervals(fit_m4_pi_rp_sham, pars = 'mu_ep' , prob = 0.95, prob_outer = 1) + xlim(0,0.71)
mcmc_intervals(fit_m4_pi_rp_anode, pars = 'mu_ep' , prob = 0.95, prob_outer = 1) + xlim(0,0.71)

mcmc_intervals(fit_m4_pi_rp_sham, pars = 'mu_b' , prob = 0.95, prob_outer = 1) + xlim(-1,1.5)
mcmc_intervals(fit_m4_pi_rp_anode, pars = 'mu_b' , prob = 0.95, prob_outer = 1) + xlim(-1,1.5)

mcmc_intervals(fit_m4_pi_rp_sham, pars = c('mu_piRew', 'mu_piPun') , prob = 0.95, prob_outer = 1) + xlim(-1,1)
mcmc_intervals(fit_m4_pi_rp_anode, pars = c('mu_piRew', 'mu_piPun') , prob = 0.95, prob_outer = 1) + xlim(-1,1)

mcmc_intervals(fit_m4_pi_rp_sham, pars = c('mu_rhoRew', 'mu_rhoPun') , prob = 0.95, prob_outer = 1) + xlim(0,31)
mcmc_intervals(fit_m4_pi_rp_anode, pars = c('mu_rhoRew', 'mu_rhoPun') , prob = 0.95, prob_outer = 1) + xlim(0,31)

# individual parameter plot
## parameters related to GoBias
pdf(str_glue("{SAVE_DIR}/result_plots/sep/b_plots_tdcs_N{N}.pdf"))
mcmc_areas(fit_m4_k_sep_r_sham,
           regex_pars = "rhoPun\\[.*",
           prob = 0.95, prob_outer = 1) +
  xlim(0,1)

mcmc_areas(fit_m4_anode$fit,
           regex_pars = "b\\[.*",
           prob = 0.8) +
  xlim(-4,8)

mcmc_areas(fit_m4_k_sep_r_anode,
           regex_pars = "rhoPun$",
           prob = 0.95, prob_outer = 1) #+
  xlim(0,1)

mcmc_areas(fit_m4_anode$fit,
           regex_pars = "b$",
           prob = 0.8) +
  xlim(-4,8)
dev.off()

## parameters related to pavBias
pdf(str_glue("{SAVE_DIR}/result_plots/sep/pi_plots_tdcs_N{N}.pdf"))
mcmc_areas(fit_m4_sham$fit,
           regex_pars = "pi\\[.*",
           prob = 0.8) +
  xlim(-3,3)

mcmc_areas(fit_m4_anode$fit,
           regex_pars = "pi\\[.*",
           prob = 0.8) +
  xlim(-3,3)

mcmc_areas(fit_m4_sham$fit,
           regex_pars = "pi$",
           prob = 0.8) +
  xlim(-3,3)

mcmc_areas(fit_m4_anode$fit,
           regex_pars = "pi$",
           prob = 0.8) +
  xlim(-3,3)

dev.off()

## other parameters
pdf(str_glue("{SAVE_DIR}/result_plots/sep/otherparams_plots_tdcs_N{N}.pdf"))
mcmc_areas(fit_m4_sham$fit, regex_pars = "xi$", prob = 0.8) + xlim(0,0.5)
mcmc_areas(fit_m4_sham$fit, regex_pars = "xi\\[.*", prob = 0.8) + xlim(0,0.5)
mcmc_areas(fit_m4_sham$fit, regex_pars = "ep\\[.*", prob = 0.8) + xlim(0,1)
mcmc_areas(fit_m4_sham$fit, regex_pars = "ep$", prob = 0.8) + xlim(0,1)
mcmc_areas(fit_m4_sham$fit, regex_pars = "rhoRew\\[.*", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_sham$fit, regex_pars = "rhoRew$", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_sham$fit, regex_pars = "rhoPun\\[.*", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_sham$fit, regex_pars = "rhoPun$", prob = 0.8) + xlim(0,50)

mcmc_areas(fit_m4_pi_rp_anode, regex_pars = "xi\\[.*", prob = 0.95, prob_outer = 1) #+ xlim(0,0.5)
mcmc_areas(fit_m4_anode$fit, regex_pars = "xi$", prob = 0.8) + xlim(0,0.5)
mcmc_areas(fit_m4_pi_rp_anode, regex_pars = "ep\\[.*", prob = 0.95, prob_outer = 1) + xlim(0,1)
mcmc_areas(fit_m4_anode$fit, regex_pars = "ep$", prob = 0.8) + xlim(0,1)
mcmc_areas(fit_m4_pi_rp_anode, regex_pars = "rhoRew\\[.*", prob = 0.95, prob_outer = 1) #+ xlim(0,50)
mcmc_areas(fit_m4_anode$fit, regex_pars = "rhoRew$", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_pi_rp_sham, regex_pars = "rhoPun\\[.*", prob = 0.95, prob_outer = 1) #+ xlim(0,50)
mcmc_areas(fit_m4_pi_rp_anode, regex_pars = "rhoPun$", prob = 0.95, prob_outer = 1) #+ xlim(0,1)

dev.off()

# Paired t-test
t.test(fit_m4_sham$allIndPars$pi, fit_m4_anode$allIndPars$pi, paired = TRUE, alternative = "two.sided")
t.test(fit_m4_sham$allIndPars$b, fit_m4_anode$allIndPars$b, paired = TRUE, alternative = "two.sided")
t.test(fit_m4_sham$allIndPars$xi, fit_m4_anode$allIndPars$xi, paired = TRUE, alternative = "two.sided")
t.test(fit_m4_sham$allIndPars$ep, fit_m4_anode$allIndPars$ep, paired = TRUE, alternative = "two.sided")
t.test(fit_m4_sham$allIndPars$rhoRew, fit_m4_anode$allIndPars$rhoRew, paired = TRUE, alternative = "two.sided")
t.test(fit_m4_sham$allIndPars$rhoPun, fit_m4_anode$allIndPars$rhoPun, paired = TRUE, alternative = "two.sided")

# Check correlation
## between b_0 and b_delta
df_corr <- left_join(fit_m4_sham$allIndPars, fit_m4_anode$allIndPars, by = "subjID")

ggplot(df_corr, aes(x=b.x, y=b.y)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation b_sham & b_anode") + 
  theme_bw() +
  annotate("text", x = 1, y = 1, label = cor(df_corr$b.x, df_corr$b.y)) +
  xlim(-3.5, 3.5) + ylim(-3.5, 3.5)
  #geom_text(label=rownames(df_corr))

## between pi_0 and pi_delta
ggplot(df_corr, aes(x=pi.x, y=pi.y)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation pi_sham & pi_anode") + 
  theme_bw() +
  annotate("text", x = 1, y = 1, label = cor(df_corr$pi.x, df_corr$pi.y)) +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5)

## between xi
ggplot(df_corr, aes(x=xi.x, y=xi.y)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation xi_sham & xi_anode") + 
  theme_bw() +
  annotate("text", x = 0.13, y = 0.13, label = cor(df_corr$xi.x, df_corr$xi.y)) +
  xlim(0, 0.15) + ylim(0, 0.15)

## between ep
ggplot(df_corr, aes(x=ep.x, y=ep.y)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation ep_sham & ep_anode") + 
  theme_bw() +
  annotate("text", x = 0.9, y = 0.9, label = cor(df_corr$ep.x, df_corr$ep.y)) +
  xlim(0, 1) + ylim(0, 1)

## between rhoRew
ggplot(df_corr, aes(x=rhoRew.x, y=rhoRew.y)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation rhoRew_sham & rhoRew_anode") + 
  theme_bw()+
  annotate("text", x = 28, y = 28, label = cor(df_corr$rhoRew.x, df_corr$rhoRew.y)) +
  xlim(0, 30) + ylim(0, 30)

## between rhoPun
ggplot(df_corr, aes(x=rhoPun.x, y=rhoPun.y)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation rhoPun_sham & rhoPun_anode") + 
  theme_bw()+
  annotate("text", x = 28, y = 28, label = cor(df_corr$rhoPun.x, df_corr$rhoPun.y)) +
  xlim(0, 30) + ylim(0, 30)


# Plot difference between Go bias nad Pav bias
ext_sham  <- extract(fit_m4_pi_rp_sham)
ext_anode <- extract(fit_m4_pi_rp_anode)

params <- c('xi','ep','b','piRew','piPun','rhoRew','rhoPun')

### All IndPars
allIndPars <- array(NA, c(17, length(params)))
allIndPars <- as.data.frame(allIndPars)
names(allIndPars) <- params

for (i in 1:17) {
  allIndPars[i,] <- c(mean(ext_anode$xi[, i]),
                      mean(ext_anode$ep[, i]),
                      mean(ext_anode$b[, i]),
                      mean(ext_anode$piRew[, i]),
                      mean(ext_anode$piPun[, i]),
                      mean(ext_anode$rhoRew[, i]),
                      mean(ext_anode$rhoPun[, i]))
}

allIndParsSham <- allIndPars
allIndParsAnode <- allIndPars

res <- cor(allIndPars)
round(res, 2)

allIndParsSham_csv <- cbind(subj_list_sham, allIndParsSham)
allIndParsAnode_csv <- cbind(subj_list_anode, allIndParsAnode)

write.csv(allIndParsSham_csv, file = str_glue('modeling_sham_tmp.csv', row.names=TRUE))
write.csv(allIndParsAnode_csv, file = str_glue('modeling_anode_tmp.csv', row.names=TRUE))

#########
tmp_sham_epRew_ep <- allIndParsSham$epRew - allIndParsSham$ep
tmp_anode_epRew_ep <- allIndParsAnode$epRew - allIndParsAnode$ep
tmp <- data.frame(subject=subj_list,
           sham_epRew_ep = tmp_sham_epRew_ep,
           anode_epRew_ep = tmp_anode_epRew_ep)

tmp %>% filter((sham_epRew_ep > 0) | (anode_epRew_ep > 0) )
#########

ind_xi_del <- as.data.frame(ext_anode$xi - ext_sham$xi)
names(ind_xi_del) <- c('xi_1','xi_2','xi_3','xi_4','xi_5','xi_6','xi_7','xi_8','xi_9','xi_10',
                       'xi_11','xi_12','xi_13','xi_14','xi_15','xi_16','xi_17','xi_18','xi_19','xi_20',
                       'xi_21','xi_22','xi_23','xi_24','xi_25','xi_26','xi_27')
mu_xi_del <- as.data.frame(ext_anode$mu_xi - ext_sham$mu_xi); names(mu_xi_del) <- 'mu_xi'
mcmc_areas(mu_xi_del, prob = 0.95, prob_outer = 1)
xi_del <- cbind(mu_xi_del, ind_xi_del)
dim(ind_xi_del)
xi_del <- colMeans(ind_xi_del)
###########
k_anode <- as.data.frame(ext_anode$epRew - ext_anode$ep)
names(k_anode) <- c('k_1','k_2','k_3','k_4','k_5','k_6','k_7','k_8','k_9','k_10',
                    'k_11','k_12','k_13','k_14','k_15','k_16','k_17','k_18','k_19','k_20',
                    'k_21','k_22','k_23','k_24','k_25','k_26','k_27')
mu_ep_del <- as.data.frame(ext_anode$mu_ep - ext_sham$mu_ep); names(mu_ep_del) <- 'mu_ep'
mu_k_anode <- as.data.frame(ext_anode$mu_epRew - ext_anode$mu_ep); names(mu_k_anode) <- 'mu_k'

mcmc_areas(k_anode, prob = 0.95, prob_outer = 1)
mcmc_areas(mu_k_anode, prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(k_anode$k_5), prob = 0.95, prob_outer = 1)

k_sham <- as.data.frame(ext_sham$epRew - ext_sham$ep)
names(k_sham) <- c('k_1','k_2','k_3','k_4','k_5','k_6','k_7','k_8','k_9','k_10',
                    'k_11','k_12','k_13','k_14','k_15','k_16','k_17','k_18','k_19','k_20',
                    'k_21','k_22','k_23','k_24','k_25','k_26','k_27')
mu_k_sham <- as.data.frame(ext_sham$mu_epRew - ext_sham$mu_ep); names(mu_k_sham) <- 'mu_k'

mcmc_areas(k_sham, prob = 0.95, prob_outer = 1)
mcmc_areas(mu_k_sham, prob = 0.95, prob_outer = 1)
mcmc_areas(k_sham %>% select(c('k_1','k_2','k_3','k_4')), prob = 0.95, prob_outer = 1)
###########
ep_anode <- as.data.frame(ext_anode$ep)
names(ep_anode) <- c('ep_1','ep_2','ep_3','ep_4','ep_5','ep_6','ep_7','ep_8','ep_9','ep_10',
                     'ep_11','ep_12','ep_13','ep_14','ep_15','ep_16','ep_17','ep_18','ep_19','ep_20',
                     'ep_21','ep_22','ep_23','ep_24','ep_25','ep_26','ep_27')
mu_ep_anode <- as.data.frame(ext_anode$mu_ep); names(mu_ep_anode) <- 'mu_ep'

mcmc_areas(ep_anode, prob = 0.95, prob_outer = 1)
mcmc_areas(mu_ep_anode, prob = 0.95, prob_outer = 1)

ep_sham <- as.data.frame(ext_sham$ep)
names(ep_sham) <- c('ep_1','ep_2','ep_3','ep_4','ep_5','ep_6','ep_7','ep_8','ep_9','ep_10',
                    'ep_11','ep_12','ep_13','ep_14','ep_15','ep_16','ep_17','ep_18','ep_19','ep_20',
                    'ep_21','ep_22','ep_23','ep_24','ep_25','ep_26','ep_27')
mu_ep_sham <- as.data.frame(ext_sham$mu_ep); names(mu_ep_sham) <- 'mu_ep'

mcmc_areas(ep_sham, prob = 0.95, prob_outer = 1)
mcmc_areas(mu_ep_sham, prob = 0.95, prob_outer = 1)

ind_epRew_del <- as.data.frame(ext_anode$epRew - ext_sham$epRew)
names(ind_epRew_del) <- c('epRew_1','epRew_2','epRew_3','epRew_4','epRew_5','epRew_6','epRew_7','epRew_8','epRew_9','epRew_10',
                          'epRew_11','epRew_12','epRew_13','epRew_14','epRew_15','epRew_16','epRew_17','epRew_18','epRew_19','epRew_20',
                          'epRew_21','epRew_22','epRew_23','epRew_24','epRew_25','epRew_26','epRew_27')
mu_epRew_del <- as.data.frame(ext_anode$mu_epRew - ext_sham$mu_epRew); names(mu_epRew_del) <- 'mu_epRew'
epRew_del <- cbind(mu_epRew_del, ind_epRew_del)
mcmc_areas(mu_epRew_del, prob = 0.95, prob_outer = 1)
epRew_del <- colMeans(ind_epRew_del)
###########

ind_ep_del <- as.data.frame(ext_anode$ep - ext_sham$ep)
names(ind_ep_del) <- c('ep_1','ep_2','ep_3','ep_4','ep_5','ep_6','ep_7','ep_8','ep_9','ep_10',
                       'ep_11','ep_12','ep_13','ep_14','ep_15','ep_16','ep_17','ep_18','ep_19','ep_20',
                       'ep_21','ep_22','ep_23','ep_24','ep_25','ep_26','ep_27')
mu_ep_del <- as.data.frame(ext_anode$mu_ep - ext_sham$mu_ep); names(mu_ep_del) <- 'mu_ep'
ep_del <- cbind(mu_ep_del, ind_ep_del)
mcmc_areas(mu_ep_del, prob = 0.95, prob_outer = 1)
ep_del <- colMeans(ind_ep_del)

ind_b_del <- as.data.frame(ext_anode$b - ext_sham$b)
names(ind_b_del) <- c('b_1','b_2','b_3','b_4','b_5','b_6','b_7','b_8','b_9','b_10',
                      'b_11','b_12','b_13','b_14','b_15','b_16','b_17','b_18','b_19','b_20',
                      'b_21','b_22','b_23','b_24','b_25','b_26','b_27')
mu_b_del <- as.data.frame(ext_anode$mu_b - ext_sham$mu_b); names(mu_b_del) <- 'mu_b'
mcmc_areas(mu_b_del, prob = 0.95, prob_outer = 1)
b_del <- cbind(mu_b_del, ind_b_del)
b_del <- colMeans(ind_b_del)

ind_pi_del <- as.data.frame(ext_anode$pi - ext_sham$pi)
names(ind_pi_del) <- c('pi_1','pi_2','pi_3','pi_4','pi_5','pi_6','pi_7','pi_8','pi_9','pi_10',
                       'pi_11','pi_12','pi_13','pi_14','pi_15','pi_16','pi_17','pi_18','pi_19','pi_20',
                       'pi_21','pi_22','pi_23','pi_24','pi_25','pi_26','pi_27')
mu_pi_del <- as.data.frame(ext_anode$mu_pi - ext_sham$mu_pi); names(mu_pi_del) <- 'mu_pi'
pi_del <- cbind(mu_pi_del, ind_pi_del)
pi_del <- colMeans(ind_pi_del)

############################
ind_piRew_del <- as.data.frame(ext_anode$piRew - ext_sham$piRew)
names(ind_piRew_del) <- c('piRew_1','piRew_2','piRew_3','piRew_4','piRew_5','piRew_6','piRew_7','piRew_8','piRew_9','piRew_10',
                          'piRew_11','piRew_12','piRew_13','piRew_14','piRew_15','piRew_16','piRew_17','piRew_18','piRew_19','piRew_20',
                          'piRew_21','piRew_22','piRew_23','piRew_24','piRew_25','piRew_26','piRew_27')
mu_piRew_del <- as.data.frame(ext_anode$mu_piRew - ext_sham$mu_piRew); names(mu_piRew_del) <- 'mu_piRew'
mcmc_areas(mu_piRew_del, prob = 0.95, prob_outer = 1)
piRew_del <- cbind(mu_piRew_del, ind_piRew_del)
#piRew_del <- colMeans(ind_piRew_del)

ind_piPun_del <- as.data.frame(ext_anode$piPun - ext_sham$piPun)
names(ind_piPun_del) <- c('piPun_1','piPun_2','piPun_3','piPun_4','piPun_5','piPun_6','piPun_7','piPun_8','piPun_9','piPun_10',
                          'piPun_11','piPun_12','piPun_13','piPun_14','piPun_15','piPun_16','piPun_17','piPun_18','piPun_19','piPun_20',
                          'piPun_21','piPun_22','piPun_23','piPun_24','piPun_25','piPun_26','piPun_27')
mu_piPun_del <- as.data.frame(ext_anode$mu_piPun - ext_sham$mu_piPun); names(mu_piPun_del) <- 'mu_piPun'
piPun_del <- cbind(mu_piPun_del, ind_piPun_del)
mcmc_areas(mu_piPun_del, prob = 0.95, prob_outer = 1)
#piPun_del <- colMeans(ind_piPun_del)
###############################
ind_rhoRew_del <- as.data.frame(ext_anode$rhoRew - ext_sham$rhoRew)
names(ind_rhoRew_del) <- c('rhoRew_1','rhoRew_2','rhoRew_3','rhoRew_4','rhoRew_5','rhoRew_6','rhoRew_7','rhoRew_8','rhoRew_9','rhoRew_10',
                           'rhoRew_11','rhoRew_12','rhoRew_13','rhoRew_14','rhoRew_15','rhoRew_16','rhoRew_17','rhoRew_18','rhoRew_19','rhoRew_20',
                           'rhoRew_21','rhoRew_22','rhoRew_23','rhoRew_24','rhoRew_25','rhoRew_26','rhoRew_27')
mu_rhoRew_del <- as.data.frame(ext_anode$mu_rhoRew - ext_sham$mu_rhoRew); names(mu_rhoRew_del) <- 'mu_rhoRew'
mcmc_areas(mu_rhoRew_del, prob = 0.95, prob_outer = 1)
rhoRew_del <- cbind(mu_rhoRew_del, ind_rhoRew_del)
#rhoRew_del <- colMeans(ind_rhoRew_del)

ind_rhoPun_del <- as.data.frame(ext_anode$rhoPun - ext_sham$rhoPun)
names(ind_rhoPun_del) <- c('rhoPun_1','rhoPun_2','rhoPun_3','rhoPun_4','rhoPun_5','rhoPun_6','rhoPun_7','rhoPun_8','rhoPun_9','rhoPun_10',
                           'rhoPun_11','rhoPun_12','rhoPun_13','rhoPun_14','rhoPun_15','rhoPun_16','rhoPun_17','rhoPun_18','rhoPun_19','rhoPun_20',
                           'rhoPun_21','rhoPun_22','rhoPun_23','rhoPun_24','rhoPun_25','rhoPun_26','rhoPun_27')
mu_rhoPun_del <- as.data.frame(ext_anode$mu_rhoPun - ext_sham$mu_rhoPun); names(mu_rhoPun_del) <- 'mu_rhoPun'
mcmc_areas(mu_rhoPun_del, prob = 0.95, prob_outer = 1)
rhoPun_del <- cbind(mu_rhoPun_del, ind_rhoPun_del)
#rhoPun_del <- colMeans(ind_rhoPun_del)

ind_k_del <- as.data.frame(ext_anode$k - ext_sham$k)
names(ind_k_del) <- c('k_1','k_2','k_3','k_4','k_5','k_6','k_7','k_8','k_9','k_10',
                      'k_11','k_12','k_13','k_14','k_15','k_16','k_17','k_18','k_19','k_20',
                      'k_21','k_22','k_23','k_24','k_25','k_26','k_27')
mu_k_del <- as.data.frame(ext_anode$mu_k - ext_sham$mu_k); names(mu_k_del) <- 'mu_k'
k_del <- cbind(mu_k_del, ind_k_del)

mcmc_areas(xi_del, prob = 0.95, prob_outer = 1)
mcmc_areas(ep_del, prob = 0.95, prob_outer = 1)
mcmc_areas(b_del, prob = 0.95, prob_outer = 1)
mcmc_areas(pi_del, prob = 0.95, prob_outer = 1)
mcmc_areas(epRew_del, prob = 0.95, prob_outer = 1)
mcmc_areas(rhoRew_del, prob = 0.95, prob_outer = 1)
mcmc_areas(rhoPun_del, prob = 0.95, prob_outer = 1)
mcmc_areas(k_del, prob = 0.95, prob_outer = 1)

mcmc_areas(as.data.frame(xi_del$mu_xi), prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(ep_del$mu_ep), prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(b_del$mu_b), prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(epRew_del$mu_epRew), prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(pi_del$mu_pi), prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(rhoRew_del$mu_rhoRew), prob = 0.95, prob_outer = 1)
mcmc_areas(as.data.frame(rhoPun_del$mu_rhoPun), prob = 0.95, prob_outer = 1)

# epRew model
df <- data.frame(xi_del,ep_del,epRew_del,b_del, pi_del,rhoRew_del, rhoPun_del)

# sep model
df2 <- data.frame(xi_del,ep_del,b_del, pi_del,rhoRew_del, rhoPun_del)




mcmc_areas(xi_del , regex_pars = "xi\\_.*", prob = 0.95) + xlim(0,0.5)
+ xlim(-1,1)

plotHDI(rowMeans(ext_m4_GoPav_sep_qc$b_2) - rowMeans(ext_m4_GoPav_sep_qc$b_1)) + xlim(-1,1)
plotHDI(rowMeans(ext_m4_GoPav_sep_qc$pi_2) - rowMeans(ext_m4_GoPav_sep_qc$pi_1)) + xlim(-1,1)

# Extract model regressors
for (reg in c('Qgo', 'Qnogo', 'Wgo', 'Wnogo', 'SV')){
  for (i in 1:2){
    if (i == 1) stim = 'sham' else stim = 'anode'
    tmp <- colMeans(ext_m4_GoPav_qc[[reg]])[,,i]
    write.csv(tmp, file = str_glue('{SAVE_DIR}/regs/{reg}_{stim}.csv', row.names=TRUE))
  }
}

# Add results in BRL-indices-all.csv
ext_m4_GoPav_sep_qc <- extract(fit_m4_GoPav_sep_qc)

IDX_DIR = '/home/kimhj9502/Project/project_WMDM/behavior_analysis/data-organized'
idxAll = read_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))

# drop columns
drops <- c('xi_tdcs', 'ep_tdcs','b_tdcs', 'pi_0_tdcs','pi_delta_tdcs','rhoRew_tdcs', 'rhoPun_tdcs')
idxAll <- idxAll[ , !(names(idxAll) %in% drops)]

df_est <- sapply(c('xi', 'ep','b_1', 'b_2', 'pi_1','pi_2','rhoRew', 'rhoPun'), function (par) {
  colMeans(ext_m4_GoPav_sep_qc[[par]])
}) %>%
  as_tibble() %>%
  mutate(subject = subj_list) %>%
  gather(param, value, xi:rhoPun) %>%
  mutate(param = str_c(param, "_tdcs")) %>%
  spread(param, value)

left_join(idxAll, df_est, by = "subject") %>%
  write_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))
