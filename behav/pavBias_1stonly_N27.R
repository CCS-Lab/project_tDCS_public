
rm(list=ls())

# Load packages
library(beeswarm)
library(reshape2)
library(ggplot2)
library(stringr)
library(tidyverse)
library(ggpubr)

# Set work dir and data dir
setwd('/home/kimhj9502/Project/project_tDCS/analysis/behav')
DATA_DIR = '/data2/project_BRL/project_tDCS/behav/data'
SAVE_DIR = '/data2/project_BRL/project_tDCS/behav/plots'

# Load data
rawdata = read.table(str_glue('{DATA_DIR}/all_2102.txt') , header=T, sep = '\t')

# Make accuracy data
rawdata_subj <- rawdata %>% group_by(subject, cond, db) %>% summarise(accuracy = mean(is_correct))

# Check go_reward = 0 subjects
go_reward <- rawdata_subj %>% filter(cond == 'go_reward') %>% filter(accuracy < 0.1)

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

# Number of subjects
n = length(unique(data[,"subject"]))
nsham <- data %>% filter(db == 1); dim(nsham)[1] / 180
nanode <- data %>% filter(db == 2); dim(nanode)[1] / 180

# Make accuracy data
data_subj <- data %>% group_by(subject, cond, db) %>% summarise(accuracy = mean(is_correct))
data_mean_se <- data_subj %>% group_by(db, cond) %>% summarise(mean = mean(accuracy),
                                                               se = sd(accuracy) / sqrt(n))

## pavBias All 
data_subj_all <- data %>% group_by(subject, db, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, db, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject, db) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0])

data_mean_se_all <- data_subj_all %>% group_by(db) %>%
  summarise(mean = mean(pavBias), se = sd(pavBias) / sqrt(n)) %>% #ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_subj_all <- data_subj_all %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

comp_all <- list(c('sham', 'anode')) 

#pdf(str_glue("{SAVE_DIR}/pavBias_N{n}.pdf"))
beeswarm_all <- beeswarm(data = data_subj_all, pavBias ~ db, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias, x = db)) +
  geom_bar(data = data_mean_se_all, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (N = {n})"))

## pavBias by condition 
data_subj_all <- data %>% group_by(subject, db, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  group_by(subject, db) %>%
  summarize(pavBias_rew = accuracy[cond == 'go_reward'] - accuracy[cond == 'nogo_reward'],
            pavBias_pun = accuracy[cond == 'nogo_punish'] - accuracy[cond == 'go_punish'])

data_mean_se_all <- data_subj_all %>% group_by(db) %>%
  summarise(mean_rew = mean(pavBias_rew), se_rew = sd(pavBias_rew) / sqrt(n),
            mean_pun = mean(pavBias_pun), se_pun = sd(pavBias_pun) / sqrt(n)) %>%#ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_subj_all <- data_subj_all %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

comp_all <- list(c('sham', 'anode')) 

### pavBias reward
beeswarm_all <- beeswarm(data = data_subj_all, pavBias_rew ~ db, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias_rew, x = db)) +
  geom_bar(data = data_mean_se_all, aes(y = mean_rew, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean_rew, x = db, ymin = mean_rew - se_rew, ymax=mean_rew + se_rew), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (reward condition) (N = {n})"))

### pavBias punish
beeswarm_all <- beeswarm(data = data_subj_all, pavBias_pun ~ db, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias_pun, x = db)) +
  geom_bar(data = data_mean_se_all, aes(y = mean_pun, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean_pun, x = db, ymin = mean_pun - se_pun, ymax=mean_pun + se_pun), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (punish condition) (N = {n})"))

## All - check order effect
### Sham first
data_subj_all <- data %>% group_by(subject, db, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, db, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject, db) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0])

data_subj_all_sham_first <- data %>%
  filter(subject %in% sham_first) %>%
  group_by(subject, db, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, db, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject, db) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0])

data_mean_se_all_sham_first <- data_subj_all %>% 
  filter(subject %in% sham_first) %>%
  group_by(db) %>%
  summarise(mean = mean(pavBias), se = sd(pavBias) / sqrt(n_sham_first)) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_subj_all_sham_first <- data_subj_all_sham_first %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

comp_all <- list(c('sham', 'anode'))

beeswarm_all_sham_first <- beeswarm(data = data_subj_all_sham_first, pavBias ~ db, method = 'swarm')
ggplot(data = data_subj_all_sham_first, aes(y = pavBias, x = db)) +
  geom_bar(data = data_mean_se_all_sham_first, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all_sham_first, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all_sham_first, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (sham first) (N = {n_sham_first})"))

### anode first
data_subj_all_anode_first <- data %>%
  filter(subject %in% anode_first) %>%
  group_by(subject, db, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, db, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject, db) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0])

data_mean_se_all_anode_first <- data_subj_all %>% 
  filter(subject %in% anode_first) %>%
  group_by(db) %>%
  summarise(mean = mean(pavBias), se = sd(pavBias) / sqrt(n_anode_first)) %>%
  mutate(db = factor(db,
                     c(2,1),
                     c('anode', 'sham')))

data_subj_all_anode_first <- data_subj_all_anode_first %>%
  mutate(db = factor(db,
                     c(2, 1),
                     c('anode', 'sham')))

comp_all <- list(c('anode', 'sham'))

beeswarm_all_anode_first <- beeswarm(data = data_subj_all_anode_first, pavBias ~ db, method = 'swarm')
ggplot(data = data_subj_all_anode_first, aes(y = pavBias, x = db)) +
  geom_bar(data = data_mean_se_all_anode_first, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all_anode_first, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all_anode_first, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (anode first) (N = {n_anode_first})"))

dev.off()

# Add results in BRL-indices-all.csv
IDX_DIR = '/home/kimhj9502/Project/project_WMDM/behavior_analysis/data-organized'
idxAll = read_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))

pav_bias_overall_tdcs <- data %>% group_by(subject, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0]) %>%
  rename(pav_bias_overall_tdcs = pavBias)

pav_bias_sham_tdcs <- data %>% 
  filter(db == 1) %>%
  group_by(subject, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0]) %>%
  rename(pav_bias_sham_tdcs = pavBias)

pav_bias_anode_tdcs <- data %>% 
  filter(db == 2) %>%
  group_by(subject, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0]) %>%
  rename(pav_bias_anode_tdcs = pavBias)

merge <- left_join(pav_bias_overall_tdcs, pav_bias_sham_tdcs, by = "subject") %>%
  left_join(pav_bias_anode_tdcs , by = "subject") %>%
  mutate(pav_bias_diff_tdcs = pav_bias_anode_tdcs - pav_bias_sham_tdcs)

left_join(idxAll, merge, by = "subject") %>%
  write_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))

