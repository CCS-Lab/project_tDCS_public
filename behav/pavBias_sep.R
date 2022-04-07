
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
rawdata = read.table(str_glue('{DATA_DIR}/all_2106.txt') , header=T, sep = '\t')

# Make accuracy data
rawdata_subj <- rawdata %>% group_by(subject, cond, db) %>% summarise(accuracy = mean(is_correct))

# Check go_reward = 0 subjects
go_reward <- rawdata_subj %>% filter(cond == 'go_reward') %>% filter(accuracy < 0.1)

# First session only
## IRB
data = rawdata %>% filter(subject != 4) %>% # IRB issue
  filter(subject != 14)

## N = 27
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

# Number of subjects
n = length(unique(data[,"subject"]))
n_first = dim(data %>% filter(ses == 1))[1] / 180
n_second = dim(data %>% filter(ses == 2))[1] / 180

# Make accuracy data
data_subj <- data %>% group_by(subject, cond, ses) %>% summarise(accuracy = mean(is_correct))
data_mean_se <- data_subj %>% group_by(ses, cond) %>% summarise(mean = mean(accuracy),
                                                               se = sd(accuracy) / sqrt(n))

## pavBias All 
data_subj_all <- data %>% group_by(subject, ses, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, ses, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject, ses) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0])

data_mean_se_all <- data_subj_all %>% group_by(ses) %>%
  summarise(mean = mean(pavBias), se = sd(pavBias) / sqrt(n)) %>% #ungroup(ses) %>%
  mutate(ses = factor(ses,
                     c(1,2),
                     c('first', 'second')))

data_subj_all <- data_subj_all %>%
  mutate(ses = factor(ses,
                     c(1,2),
                     c('first', 'second')))

comp_all <- list(c('first', 'second')) 

#pdf(str_glue("{SAVE_DIR}/pavBias_N{n}.pdf"))
#beeswarm_all <- beeswarm(data = data_subj_all, pavBias ~ ses, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias, x = ses)) +
  geom_bar(data = data_mean_se_all, aes(y = mean, x = ses), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean, x = ses, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  #geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  #geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 2.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias (First N = {n_first}, Second N = {n_second})"))

## pavBias by condition 
data_subj_all <- data %>% group_by(subject, ses, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  group_by(subject, ses) %>%
  summarize(pavBias_rew = accuracy[cond == 'go_reward'] - accuracy[cond == 'nogo_reward'],
            pavBias_pun = accuracy[cond == 'nogo_punish'] - accuracy[cond == 'go_punish'])

data_mean_se_all <- data_subj_all %>% group_by(ses) %>%
  summarise(mean_rew = mean(pavBias_rew), se_rew = sd(pavBias_rew) / sqrt(n),
            mean_pun = mean(pavBias_pun), se_pun = sd(pavBias_pun) / sqrt(n)) %>%#ungroup(ses) %>%
  mutate(ses = factor(ses,
                     c(1,2),
                     c('first','second')))

data_subj_all <- data_subj_all %>%
  mutate(ses = factor(ses,
                     c(1,2),
                     c('first','second')))

comp_all <- list(c('first','second')) 

### pavBias reward
#beeswarm_all <- beeswarm(data = data_subj_all, pavBias_rew ~ ses, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias_rew, x = ses)) +
  geom_bar(data = data_mean_se_all, aes(y = mean_rew, x = ses), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean_rew, x = ses, ymin = mean_rew - se_rew, ymax=mean_rew + se_rew), width=.2, position = position_dodge(.9)) +
  #geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  #geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", 
                     label = "p.signif", label.y = 1.0) +
  coord_cartesian(ylim=c(-0.5, 1.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 13)) +
  ggtitle(str_glue("Pavlovian bias (reward) (First N = {n_first}, Second N = {n_second})"))

### pavBias punish
beeswarm_all <- beeswarm(data = data_subj_all, pavBias_pun ~ ses, method = 'swarm')
ggplot(data = data_subj_all, aes(y = pavBias_pun, x = ses)) +
  geom_bar(data = data_mean_se_all, aes(y = mean_pun, x = ses), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean_pun, x = ses, ymin = mean_pun - se_pun, ymax=mean_pun + se_pun), width=.2, position = position_dodge(.9)) +
  #geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  #geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = F, hide.ns = F, method = "t.test", 
                     label = "p.signif", label.y = 1.0) +
  coord_cartesian(ylim=c(-0.5, 1.1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 12)) +
  ggtitle(str_glue("Pavlovian bias (punish) (First N = {n_first}, Second N = {n_second})"))

## All - check order effect
### Sham first
data_subj_all <- data %>% group_by(subject, ses, cond) %>% 
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

