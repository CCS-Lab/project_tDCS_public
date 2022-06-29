
rm(list=ls())

# Load library
library(tidyverse)
library(rstan)
library(loo)
library(stringr)
library(bayesplot)
library(ggthemes)

set.seed(123)

# Set work dir and data dir
GITDIR = '/home/kimhj9502/Project/project_tDCS_public' # PATH/TO/YOUR/GIT/project_tDCS_public
SAVEDIR = '/data2/project_BRL/project_tDCS/behav/modeling/fit_RData' # PATH/TO/YOUR/SAVE/project_tDCS_public
setwd(GITDIR)

# Set theme
color_scheme_set("teal")
bayesplot_theme_update(text = element_text(size = 25, family = "sans"))

# Load data (N = 31)
load(str_glue('{SAVEDIR}/fit_tdcs_public.RData'))

# Compare models
loo(extract_log_lik(fit_m1_sham))
loo(extract_log_lik(fit_m2_sham))
loo(extract_log_lik(fit_m3_sham))

loo(extract_log_lik(fit_m1_anode))
loo(extract_log_lik(fit_m2_anode))
loo(extract_log_lik(fit_m3_anode))

# Plots for the paper
ext_anode  <- extract(fit_m3_anode)
ext_sham  <- extract(fit_m3_sham)

# mu_xi
Sham <- ext_sham$mu_xi; Anode <- ext_anode$mu_xi; mu_xi <- data.frame(Sham,Anode)
#mcmc_intervals(mu_xi , prob = 0.95, prob_outer = 1) + xlim(0,0.25)
mcmc_intervals_data(mu_xi , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_xi , prob = 0.95, prob_outer = 1) + xlim(0,0.25)

# mu_ep
Sham <- ext_sham$mu_ep; Anode <- ext_anode$mu_ep; mu_ep <- data.frame(Sham,Anode)
#mcmc_intervals(mu_ep , prob = 0.95, prob_outer = 1)  + xlim(0,0.71)
mcmc_intervals_data(mu_ep , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_ep , prob = 0.95, prob_outer = 1)  + xlim(0,0.71)

# mu_b
Sham <- ext_sham$mu_b; Anode <- ext_anode$mu_b; mu_b <- data.frame(Sham,Anode)
#mcmc_intervals(mu_b , prob = 0.95, prob_outer = 1) + xlim(-2,5)
mcmc_intervals_data(mu_b , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_b , prob = 0.95, prob_outer = 1) + xlim(-2,5)

# mu_piRew
Sham <- ext_sham$mu_piRew; Anode <- ext_anode$mu_piRew; mu_piRew <- data.frame(Sham,Anode)
#mcmc_intervals(mu_piRew , prob = 0.95, prob_outer = 1) + xlim(-1,1)
mcmc_intervals_data(mu_piRew , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_piRew , prob = 0.95, prob_outer = 1) + xlim(-1,1)

# mu_piPun
Sham <- ext_sham$mu_piPun; Anode <- ext_anode$mu_piPun; mu_piPun <- data.frame(Sham,Anode)
#mcmc_intervals(mu_piPun , prob = 0.95, prob_outer = 1) + xlim(-1,1)
mcmc_intervals_data(mu_piPun , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_piPun , prob = 0.95, prob_outer = 1) + xlim(-1,1)

# mu_rhoRew
Sham <- ext_sham$mu_rhoRew; Anode <- ext_anode$mu_rhoRew; mu_rhoRew <- data.frame(Sham,Anode)
#mcmc_intervals(mu_rhoRew , prob = 0.95, prob_outer = 1) + xlim(0,41)
mcmc_intervals_data(mu_rhoRew , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_rhoRew , prob = 0.95, prob_outer = 1) + xlim(0,41)

# mu_rhoPun
Sham <- ext_sham$mu_rhoPun; Anode <- ext_anode$mu_rhoPun; mu_rhoPun <- data.frame(Sham,Anode)
#mcmc_intervals(mu_rhoPun , prob = 0.95, prob_outer = 1) + xlim(0,41)
mcmc_intervals_data(mu_rhoPun , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_rhoPun , prob = 0.95, prob_outer = 1) + xlim(0,41)

# Supple?
mu_xi_diff <- as.data.frame(ext_anode$mu_xi - ext_sham$mu_xi); names(mu_xi_diff) <- 'mu_xi'
mcmc_areas(mu_xi_diff, prob = 0.95, prob_outer = 1) + vline_0(linetype="dotted")
mcmc_intervals_data(mu_xi_diff, prob = 0.95, prob_outer = 1)

mu_ep_diff <- as.data.frame(ext_anode$mu_ep - ext_sham$mu_ep); names(mu_ep_diff) <- 'mu_ep'
mcmc_areas(mu_ep_diff, prob = 0.95, prob_outer = 1)
mcmc_intervals_data(mu_ep_diff, prob = 0.95, prob_outer = 1)

mu_b_diff <- as.data.frame(ext_anode$mu_b - ext_sham$mu_b); names(mu_b_diff) <- 'mu_b'
mcmc_areas(mu_b_diff, prob = 0.95, prob_outer = 1) + vline_0(linetype="dotted")
mcmc_intervals_data(mu_b_diff, prob = 0.95, prob_outer = 1)

mu_piRew_diff <- as.data.frame(ext_anode$mu_piRew - ext_sham$mu_piRew); names(mu_piRew_diff) <- 'mu_piRew'
mcmc_areas(mu_piRew_diff, prob = 0.95, prob_outer = 1)
mcmc_intervals_data(mu_piRew_diff, prob = 0.95, prob_outer = 1)

mu_piPun_diff <- as.data.frame(ext_anode$mu_piPun - ext_sham$mu_piPun); names(mu_piPun_diff) <- 'mu_piPun'
mcmc_areas(mu_piPun_diff, prob = 0.95, prob_outer = 1) + vline_0(linetype="dotted")
mcmc_intervals_data(mu_piPun_diff, prob = 0.95, prob_outer = 1)

mu_rhoRew_diff <- as.data.frame(ext_anode$mu_rhoRew - ext_sham$mu_rhoRew); names(mu_rhoRew_diff) <- 'mu_rhoRew'
mcmc_areas(mu_rhoRew_diff, prob = 0.95, prob_outer = 1)
mcmc_intervals_data(mu_rhoRew_diff, prob = 0.95, prob_outer = 1)

mu_rhoPun_diff <- as.data.frame(ext_anode$mu_rhoPun - ext_sham$mu_rhoPun); names(mu_rhoPun_diff) <- 'mu_rhoPun'
mcmc_areas(mu_rhoPun_diff, prob = 0.95, prob_outer = 1)
mcmc_intervals_data(mu_rhoPun_diff, prob = 0.95, prob_outer = 1)
