
rm(list=ls())

# Load packages
library(beeswarm)
library(ggplot2)
library(stringr)
library(tidyverse)
library(ggpubr)

set.seed(123)

# Set work dir and data dir
GITDIR = 'PATH/TO/YOUR/GIT/project_tDCS_public'
setwd(GITDIR)

# Load data
data = read.table(str_glue('{GITDIR}/Data.txt') , header=T, sep = '\t')

## Sham
data_sham <- data %>% filter(tdcs == 1)
## Anode
data_anode <- data %>% filter(tdcs == 2)

# Number of subjects
n = length(unique(data[,"subject"]))
n_sham = length(unique(data_sham[,"subject"]))
n_anode = length(unique(data_anode[,"subject"]))

# Make accuracy data
data_subj <- data %>% group_by(subject, cond, tdcs) %>% summarise(accuracy = mean(is_correct))
data_mean_se <- data_subj %>% group_by(tdcs, cond) %>% summarise(mean = mean(accuracy),
                                                               se = sd(accuracy) / sqrt(n))

## Accuracy by condition, sham
data_subj_1 <- data_subj %>% 
  filter(tdcs == 1) %>% ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_1 <-data_subj %>%
  group_by(tdcs, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>% 
  filter(tdcs == 1) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

# For comparisons
comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))
comp_all <- list(c('sham', 'anode')) 

beeswarm_1 <- beeswarm(data = data_subj_1, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_1, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_1, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity") + #, color = "black") +
  geom_errorbar(data = data_mean_se_1, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_1, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.2)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Sham: accuracy by condition (N = {n_sham})")) +
  theme_classic2() + scale_fill_brewer(palette="Pastel1") 

## Accuracy by condition, anode
data_subj_2 <- data_subj %>% filter(tdcs == 2) %>% ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_2 <-data_subj %>%
  group_by(tdcs, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>%
  filter(tdcs == 2) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

beeswarm_2 <- beeswarm(data = data_subj_2, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_2, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_2, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity") + #, color = "black") +
  geom_errorbar(data = data_mean_se_2, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_2, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.2)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Anode: accuracy by condition (N = {n_anode})"))  +
  theme_classic2() + scale_fill_brewer(palette="Pastel1") 

## pavBias by condition 
data_subj_all <- data %>% group_by(subject, tdcs, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  group_by(subject, tdcs) %>%
  summarize(pavBias_rew = accuracy[cond == 'go_reward'] - accuracy[cond == 'nogo_reward'],
            pavBias_pun = accuracy[cond == 'nogo_punish'] - accuracy[cond == 'go_punish'])

data_mean_se_all <- data_subj_all %>% group_by(tdcs) %>%
  summarise(mean_rew = mean(pavBias_rew), se_rew = sd(pavBias_rew) / sqrt(n),
            mean_pun = mean(pavBias_pun), se_pun = sd(pavBias_pun) / sqrt(n)) %>%#ungroup(tdcs) %>%
  mutate(tdcs = factor(tdcs,
                       c(1,2),
                       c('sham', 'anode')))

data_subj_all <- data_subj_all %>%
  mutate(tdcs = factor(tdcs,
                       c(1,2),
                       c('sham', 'anode')))

### pavBias punish
beeswarm_all <- beeswarm(data = data_subj_all, pavBias_pun ~ tdcs, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias_pun, x = tdcs, fill = 'pp')) +
  geom_bar(data = data_mean_se_all, aes(y = mean_pun, x = tdcs), stat = "identity") +#, color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean_pun, x = tdcs, ymin = mean_pun - se_pun, ymax=mean_pun + se_pun), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", 
                     label = "p.signif", label.y = 1.0) +
  coord_cartesian(ylim=c(-0.5, 1.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 12)) +
  ggtitle(str_glue("Pavlovian bias (punish) (Sham N = {n_sham}, Anode N = {n_anode})")) +
  theme_classic2() + scale_fill_brewer(palette="Set3") 
