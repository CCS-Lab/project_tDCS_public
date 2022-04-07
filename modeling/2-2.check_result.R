
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
N = '27'

# Load data (N = 27)
load(str_glue('{SAVE_DIR}/fit_m4_diff_exp_2102_qc.RData'))
load(str_glue('{SAVE_DIR}/fit_m4_rew.pun_2102_qc.RData'))
load(str_glue('{SAVE_DIR}/fit_RData/fit_m4_rew.pun_iter4000_prior-fixed.RData'))

# Check parameter estimation
## Parameter lists
params_nod <- c('xi', 'ep','b','pi','rhoRew', 'rhoPun')

params_Go <- c('xi', 'ep','b_0','b_delta','pi','rhoRew', 'rhoPun')
params_Pav <- c('xi', 'ep','b','pi_0','pi_delta','rhoRew', 'rhoPun')
params_GoPav <- c('xi', 'ep','b_0', 'b_delta','pi_0','pi_delta','rhoRew', 'rhoPun')

params_GoPav_rew.pun <- c('xi', 'ep','b_0', 'b_delta','pi_0_rew','pi_delta_rew', 'pi_0_pun','pi_delta_pun','rhoRew', 'rhoPun')

## Save images
fits = c(fit_m4_GoPav_rew.pun_N25)
fits_n = c('fit_m4_GoPav_rew.pun_N25')
params = list(params_GoPav_rew.pun)

# Make plots.
for (i in 1:length(fits)){
  print(fits_n[i])
  trace_list = list()
  for (n in 1:length(params[[i]])){
    p = traceplot(fits[[i]], params[[i]][n])
    trace_list[[n]] = p
  }
  
  pdf(str_glue("{SAVE_DIR}/traceplots_{fits_n[i]}_N{N}_rewpun_N25.pdf"))
  for (j in 1:length(trace_list)) {
    print(trace_list[[j]])
  }
  dev.off()
  
}


for (i in 1:length(fits)){
  print(fits_n[i])
  rhat_list = list()
  for (n in 1:length(params[[i]])){
    q = stan_rhat(fits[[i]], params[[i]][n])
    rhat_list[[n]] = q
  }
  
  pdf(str_glue("{SAVE_DIR}/rhatplots_{fits_n[i]}_N{N}_rewpun_N25.pdf"))
  for (j in 1:length(rhat_list)) {
    print(rhat_list[[j]])
  }
  dev.off()
}

# Compare models
#loo(extract_log_lik(fit_m4_nod_qc))
#loo(extract_log_lik(fit_m4_Go_qc))
#loo(extract_log_lik(fit_m4_Pav_qc))
#loo(extract_log_lik(fit_m4_GoPav_qc))
#loo(extract_log_lik(fit_m4_Go_sep_qc))
#loo(extract_log_lik(fit_m4_Pav_sep_qc))
#loo(extract_log_lik(fit_m4_GoPav_sep_qc))

loo_compare(loo(extract_log_lik(fit_m4_nod_qc)),
            loo(extract_log_lik(fit_m4_Go_qc)),
            loo(extract_log_lik(fit_m4_Pav_qc)),
            loo(extract_log_lik(fit_m4_GoPav_qc)),
            loo(extract_log_lik(fit_m4_GoPav_rew.pun)))

# fit_m4_GoPav_sep_qc is the lowest but the estimation is bad (trace, r_hat).
# So, fit_m4_GoPav_qc is the best model

# individual parameter plot
## parameters related to GoBias
pdf(str_glue("{SAVE_DIR}/b_plots_N{N}.pdf"))
mcmc_areas(fit_m4_GoPav_qc,
           regex_pars = "b_0\\[.*",
           prob = 0.95) +
  xlim(-4,4)

mcmc_areas(fit_m4_GoPav_qc,
           regex_pars = "b_delta\\[.*",
           prob = 0.95) +
  xlim(-4,4)
mcmc_areas(fit_m4_GoPav_qc,
           regex_pars = "b_0$",
           prob = 0.95) +
  xlim(-4,4)
mcmc_areas(fit_m4_GoPav_qc,
           regex_pars = "b_delta$",
           prob = 0.8) +
  xlim(-4,4)
dev.off()

mcmc_areas(fit_m4_GoPav_qc,
           regex_pars = "b_0$",
           prob_outer = 0.95) +
  xlim(-4,4)
mcmc_areas(fit_m4_GoPav_qc,
           regex_pars = "b_delta$",
           prob_outer = 0.8) +
  xlim(-4,4)

fit_m4_GoPav_rewpun <- fit_m4_GoPav_rew.pun

## parameters related to pavBias
pdf(str_glue("{SAVE_DIR}/pi_plots_N{N}.pdf"))
mcmc_areas(fit_m4_GoPav_pun,
           regex_pars = "b_0$",
           prob = 0.95, prob_outer = 1) +
  xlim(-2,2)
mcmc_areas(fit_m4_GoPav_pun,
           regex_pars = "b_delta$",
           prob = 0.95, prob_outer = 1) +
  xlim(-2,2)
mcmc_areas(fit_m4_GoPav_rew.pun,
           regex_pars = "pi_0_rew\\[.*",
           prob = 0.95, prob_outer = 1) +
  xlim(-3,3)
mcmc_areas(fit_m4_GoPav_rew.pun,
           regex_pars = "pi_delta_rew\\[.*",
           prob = 0.95) +
  xlim(-3,3)
mcmc_areas(fit_m4_GoPav_rew.pun,
           regex_pars = "pi_0_pun\\[.*",
           prob = 0.95) +
  xlim(-3,3)
mcmc_areas(fit_m4_GoPav_rew.pun,
           regex_pars = "pi_delta_pun\\[.*",
           prob = 0.95, prob_outer = 1) +
  xlim(-1,1)
mcmc_areas(fit_m4_GoPav_rewpun_sep,
           regex_pars = "b_2$",
           prob = 0.95, prob_outer = 1) +
  xlim(-1,2)
mcmc_areas(fit_m4_GoPav_pun,
           regex_pars = "pi_delta_rew$",
           prob = 0.95, prob_outer = 1) +
  xlim(-1,1)

mcmc_areas(fit_m4_GoPav_pun,
           regex_pars = "pi_0_pun$",
           prob = 0.95, prob_outer = 1) +
  xlim(-1,1)
mcmc_areas(fit_m4_GoPav_pun,
           regex_pars = "pi_delta_pun$",
           prob = 0.95, prob_outer = 1) +
  xlim(-1,1)

mcmc_areas(fit_m4_Pav_pun,
           regex_pars = "ep$",
           prob = 0.95, prob_outer = 1) +
  xlim(0,1)

dev.off()

mcmc_areas(fit_m4_GoPav_rew.pun_N25,
           regex_pars = "pi_0_rew$",
           prob = 0.95) +
  xlim(-3,3)
mcmc_areas(fit_m4_GoPav_rew.pun_N25,
           regex_pars = "pi_delta_rew$",
           prob = 0.95) +
  xlim(-3,3)

mcmc_areas(fit_m4_GoPav_rew.pun_N25,
           regex_pars = "pi_0_pun$",
           prob = 0.95) +
  xlim(-3,3)
mcmc_areas(fit_m4_GoPav_rew.pun_N25,
           regex_pars = "pi_delta_pun$",
           prob = 0.95) +
  xlim(-3,3)

## other parameters
pdf(str_glue("{SAVE_DIR}/otherparams_plots_N{N}.pdf"))
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "xi\\[.*", prob = 0.8) + xlim(0,0.4)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "xi$", prob = 0.8) + xlim(0,0.4)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "ep\\[.*", prob = 0.8) + xlim(0,1)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "ep$", prob = 0.8) + xlim(0,1)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "rhoRew\\[.*", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "rhoRew$", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "rhoPun\\[.*", prob = 0.8) + xlim(0,50)
mcmc_areas(fit_m4_GoPav_qc, regex_pars = "rhoPun$", prob = 0.8) + xlim(0,50)
dev.off()

#stan_plot(fit_m4_Pav_qc, pars = "pi_0", show_density = T)
#stan_plot(fit_m4_Pav_qc, pars = "pi_delta", show_density = T)
#stan_plot(fit_m4_Pav_p2_N7, pars = "pi_0", show_density = T)

# Check correlation
## between b_0 and b_delta
ext_m4_GoPav_qc <- rstan::extract(fit_m4_GoPav_qc)

pdf(str_glue("{SAVE_DIR}/corr_plots_N{N}.pdf"))
df_corr <- data.frame(b_0=colMeans(ext_m4_GoPav_qc$b_0),
                      b_delta=colMeans(ext_m4_GoPav_qc$b_delta))

ggplot(df_corr, aes(x=b_0, y=b_delta)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation b_0 & b_delta") + 
  theme_bw() +
  annotate("text", x = 1, y = 1, label = cor(df_corr$b_0, df_corr$b_delta))
  #xlim(-1.5, 1.5) + ylim(-1.5, 1.5)
  #geom_text(label=rownames(df_corr))

## between pi_0 and pi_delta
df_corr <- data.frame(pi_0=colMeans(ext_m4_GoPav_qc$pi_0),
                      pi_delta=colMeans(ext_m4_GoPav_qc$pi_delta))

ggplot(df_corr, aes(x=pi_0, y=pi_delta)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation pi_0 & pi_delta") + 
  theme_bw() +
  annotate("text", x = 1, y = 0.2, label = cor(df_corr$pi_0, df_corr$pi_delta))
#xlim(-1.5, 1.5) + ylim(-1.5, 1.5)
#geom_text(label=rownames(df_corr))

dev.off()

# Check correlation
## between b_0 and b_0 + b_delta
ext_m4_GoPav_qc <- rstan::extract(fit_m4_GoPav_qc)

pdf(str_glue("{SAVE_DIR}/corr_plots_N{N}.pdf"))
df_corr <- data.frame(b_0=colMeans(ext_m4_GoPav_qc$b_0),
                      b_1=colMeans(ext_m4_GoPav_qc$b_0)+colMeans(ext_m4_GoPav_qc$b_delta))

ggplot(df_corr, aes(x=b_0, y=b_1)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation b_0 & b_1") + 
  theme_bw() +
  annotate("text", x = 1, y = 1, label = cor(df_corr$b_0, df_corr$b_1)) +
  xlim(-2.3, 2.3) + ylim(-2.3, 2.3)
#geom_text(label=rownames(df_corr))

t.test(df_corr$b_0, df_corr$b_1, paired = TRUE, alternative = "two.sided")
c(mean(df_corr$b_0), mean(df_corr$b_1))

## between pi_0 and pi_delta
df_corr <- data.frame(pi_0=colMeans(ext_m4_GoPav_qc$pi_0),
                      pi_1=colMeans(ext_m4_GoPav_qc$pi_0)+colMeans(ext_m4_GoPav_qc$pi_delta))

ggplot(df_corr, aes(x=pi_0, y=pi_1)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation pi_0 & pi_1") + 
  theme_bw() +
  annotate("text", x = 1, y = 0.2, label = cor(df_corr$pi_0, df_corr$pi_1)) +
  xlim(-0.5, 1.5) + ylim(-0.5, 1.5)
#geom_text(label=rownames(df_corr))

t.test(df_corr$pi_0, df_corr$pi_1, paired = TRUE, alternative = "two.sided")
c(mean(df_corr$pi_0), mean(df_corr$pi_1))

dev.off()

barplot(c(mean(df_corr$pi_0),mean(df_corr$pi_1)))

ggplot(data = df_corr, aes(y = pavBias, x = db)) +
  geom_bar(data = data_mean_se_all, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (N = {n})"))

# Plot difference between Go bias nad Pav bias
#plotHDI(rowMeans(ext_m4_GoPav_sep_qc$b_2) - rowMeans(ext_m4_GoPav_sep_qc$b_1)) + xlim(-1,1)
#plotHDI(rowMeans(ext_m4_GoPav_sep_qc$pi_2) - rowMeans(ext_m4_GoPav_sep_qc$pi_1)) + xlim(-1,1)

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
