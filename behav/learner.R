
rm(list=ls())

# Load packages
library(beeswarm)
library(reshape2)
library(ggplot2)
library(stringr)
library(tidyverse)
library(ggpubr)
library(purrr)

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

# Order effect subjects
# 69, 103, 116 - drop-out 
#anode_first = c(28, 18, 33, 61, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127, 126) # 14, 103, 116
#sham_first = c(30, 54, 43, 11, 73, 79, 101, 102, 108, 110, 104, 125) # 4, 15, 27, 69, 94, 113

## N = 27
anode_first = c(28, 18, 33, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127) # 14, 61, 103, 116, 126
sham_first = c(30, 54, 43, 11, 73, 79, 101, 108, 110, 104, 125) # 4, 15, 27, 69, 94, 102, 113

# Number of subjects
n = length(unique(data[,"subject"]))
n_sham_first = length(sham_first)
n_anode_first = length(anode_first)

# learner threshold
learnThres = c(0.6, 0.8)

# Make accuracy data
data_subj <- data %>% group_by(subject, cond, db) %>%
  summarise(accuracy_1st = mean(is_correct[trial <= 90]), accuracy_2nd = mean(is_correct[trial > 90]))

learner <- data_subj %>%
  mutate(tmp_learner = ifelse(accuracy_1st>learnThres[1] & accuracy_2nd>learnThres[2], 1, 0)) %>%
  group_by(subject, db) %>%
  summarise(learner = ifelse(sum(tmp_learner) == 4, 1, 0)) %>% 
  ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

learner_wide <- learner %>% spread(db, learner) %>% rename(sham_learn = sham, anode_learn = anode)
sum(learner_wide['sham_learn']==0)
sum(learner_wide['anode_learn']==0)

## pavBias All 
data_subj_all <- data %>% 
  group_by(subject, db, cond) %>% 
  summarise(accuracy = mean(is_correct))  %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  group_by(subject, db, Con) %>%
  summarise(pav = sum(accuracy)) %>%
  group_by(subject, db) %>%
  summarize(pavBias = pav[Con == 1] - pav[Con == 0])

data_subj_all_wide <- data_subj_all %>% spread(db, pavBias) %>% rename(sham = 2, anode = 3)

data_cond_learn <- left_join(learner_wide, data_subj_all_wide, by = "subject")

# PavBias and SE of each group
sham_learn <- data_cond_learn %>%
  filter(sham_learn == 1) %>%
  summarise(mean = mean(sham),
            se = sd(sham) / sqrt(sum(sham_learn == 1)))

sham_nonlearn <- data_cond_learn %>%
  filter(sham_learn == 0) %>%
  summarise(mean = mean(sham), se = sd(sham) / sqrt(sum(sham_learn == 0)))
anode_learn <- data_cond_learn %>%
  filter(anode_learn == 1) %>%
  summarise(mean = mean(anode), se = sd(anode) / sqrt(sum(anode_learn == 1)))
anode_nonlearn <- data_cond_learn %>%
  filter(anode_learn == 0) %>%
  summarise(mean = mean(anode), se = sd(anode) / sqrt(sum(anode_learn == 0)))

# Make them into one df - learner
df <- sham_learn
df <- rbind(df, anode_learn)
df['type'] = c('sham_learn', 'anode_learn')

## Plot
#comp_all <- list(c('sham_learn', 'anode_learn', 'sham_nonlearn', 'anode_nonlearn')) 
ggplot(data = df, aes(y = mean, x = type)) +
  geom_bar(data = df, aes(y = mean, x = type), stat = "identity", color = "black") +
  geom_errorbar(data = df, aes(y = mean, x = type, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  #geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  #geom_line(aes(group = subject), alpha = 0.1) +
  #stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias"))

## t-test
sham_learn <- data_cond_learn %>%
  filter(sham_learn == 1)
sham_learn_vec <- sham_learn$sham
anode_learn <- data_cond_learn %>%
  filter(anode_learn == 1)
anode_learn_vec <- anode_learn$anode

t.test(sham_learn_vec, anode_learn_vec) # not significant 0.2663


# Make them into one df - nonlearner
df <- sham_nonlearn
df <- rbind(df, anode_nonlearn)
df['type'] = c('sham_nonlearn', 'anode_nonlearn')

# Plot
#comp_all <- list(c('sham_learn', 'anode_learn', 'sham_nonlearn', 'anode_nonlearn')) 
ggplot(data = df, aes(y = mean, x = type)) +
  geom_bar(data = df, aes(y = mean, x = type), stat = "identity", color = "black") +
  geom_errorbar(data = df, aes(y = mean, x = type, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  #geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  #geom_line(aes(group = subject), alpha = 0.1) +
  #stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 2) +
  coord_cartesian(ylim=c(-1, 1)) +
  ylab("Pavlovian bias")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Pavlovian bias"))

## t-test
sham_nonlearn <- data_cond_learn %>%
  filter(sham_learn == 0)
sham_nonlearn_vec <- sham_nonlearn$sham
anode_nonlearn <- data_cond_learn %>%
  filter(anode_learn == 0)
anode_nonlearn_vec <- anode_nonlearn$anode

t.test(sham_nonlearn_vec, anode_nonlearn_vec) # not significant 0.2663

# Add results in BRL-indices-all.csv
IDX_DIR = '/home/kimhj9502/Project/project_WMDM/behavior_analysis/data-organized'
idxAll = read_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))

## learner
learner_wide <- learner %>% spread(db, learner) %>% rename(sham_learn = sham, anode_learn = anode)

left_join(idxAll, learner_wide, by = "subject") %>%
  write_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))
