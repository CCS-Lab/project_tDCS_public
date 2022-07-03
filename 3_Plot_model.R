
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
GITDIR = 'PATH/TO/YOUR/GIT/project_tDCS_public'
SAVEDIR = 'PATH/TO/YOUR/SAVE/project_tDCS_public'
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
mcmc_intervals_data(mu_xi , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_xi , prob = 0.95, prob_outer = 1) + xlim(0,0.25)

# mu_ep
Sham <- ext_sham$mu_ep; Anode <- ext_anode$mu_ep; mu_ep <- data.frame(Sham,Anode)
mcmc_intervals_data(mu_ep , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_ep , prob = 0.95, prob_outer = 1)  + xlim(0,0.71)

# mu_b
Sham <- ext_sham$mu_b; Anode <- ext_anode$mu_b; mu_b <- data.frame(Sham,Anode)
mcmc_intervals_data(mu_b , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_b , prob = 0.95, prob_outer = 1) + xlim(-2,5)

# mu_piRew
Sham <- ext_sham$mu_piRew; Anode <- ext_anode$mu_piRew; mu_piRew <- data.frame(Sham,Anode)
mcmc_intervals_data(mu_piRew , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_piRew , prob = 0.95, prob_outer = 1) + xlim(-1,1)

# mu_piPun
Sham <- ext_sham$mu_piPun; Anode <- ext_anode$mu_piPun; mu_piPun <- data.frame(Sham,Anode)
mcmc_intervals_data(mu_piPun , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_piPun , prob = 0.95, prob_outer = 1) + xlim(-1,1)

# mu_rhoRew
Sham <- ext_sham$mu_rhoRew; Anode <- ext_anode$mu_rhoRew; mu_rhoRew <- data.frame(Sham,Anode)
mcmc_intervals_data(mu_rhoRew , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_rhoRew , prob = 0.95, prob_outer = 1) + xlim(0,41)

# mu_rhoPun
Sham <- ext_sham$mu_rhoPun; Anode <- ext_anode$mu_rhoPun; mu_rhoPun <- data.frame(Sham,Anode)
mcmc_intervals_data(mu_rhoPun , prob = 0.95, prob_outer = 1)
mcmc_areas(mu_rhoPun , prob = 0.95, prob_outer = 1) + xlim(0,41)
