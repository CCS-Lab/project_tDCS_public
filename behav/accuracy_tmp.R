
rm(list=ls())

# Load packages
library(beeswarm)
library(reshape2)
library(ggplot2)
library(stringr)
library(tidyverse)
library(car)
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

# Sham
data_sham = data %>% filter(db == 1)
#test_sham <- data_sham %>% group_by(subject, cond) %>% summarise(go = sum(key_pressed)) #%>%
#  filter(outcome == -1)
test_sham <- data_sham %>% group_by(subject, outcome) %>%
  summarise(go = sum(key_pressed), nogo = sum(1- key_pressed)) #%>%
  filter(outcome == -1)

data <- data_sham
# Number of subjects
n = length(unique(data[,"subject"]))

# Make accuracy data
data_subj <- data %>% group_by(subject, cond) %>% summarise(accuracy = mean(is_correct))
data_mean_se <- data_subj %>% group_by(db, cond) %>% summarise(mean = mean(accuracy),
                                                               se = sd(accuracy) / sqrt(n))
## Accuracy All 
data_subj_all <- data %>% group_by(subject, db) %>% 
  summarise(accuracy = mean(is_correct))

data_mean_se_all <- data_subj_all %>% group_by(db) %>%
  summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>% #ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_subj_all <- data_subj_all %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))
  
# Anode
data_anode = data %>% filter(db == 2)
test_anode <- data_anode %>% group_by(subject, outcome) %>% summarise(nogo = sum(1- key_pressed)) %>%
  filter(outcome == -1)



## N = 27
anode_first = c(28, 18, 33, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127) # 14, 61, 103, 116, 126
sham_first = c(30, 54, 43, 11, 73, 79, 101, 108, 110, 104, 125) # 4, 15, 27, 69, 94, 102, 113

# Number of subjects
n = length(unique(data[,"subject"]))
n_sham_first = length(sham_first)
n_anode_first = length(anode_first)

# Make accuracy data
data_subj <- data %>% group_by(subject, cond, db) %>% summarise(accuracy = mean(is_correct))
data_mean_se <- data_subj %>% group_by(db, cond) %>% summarise(mean = mean(accuracy),
                                                               se = sd(accuracy) / sqrt(n))
## Accuracy All 
data_subj_all <- data %>% group_by(subject, db) %>% 
  summarise(accuracy = mean(is_correct))

data_mean_se_all <- data_subj_all %>% group_by(db) %>%
  summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>% #ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_subj_all <- data_subj_all %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

comp_all <- list(c('sham', 'anode')) 

pdf(str_glue("{SAVE_DIR}/accuracy_N{n}.pdf"))
beeswarm_all <- beeswarm(data = data_subj_all, accuracy ~ db, method = 'swarm')
ggplot(data = data_subj_all, aes(y = accuracy, x = db)) +
  geom_bar(data = data_mean_se_all, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Overall accuracy (N = {n})"))

## All - check order effect
data_subj_all <- data %>% group_by(subject, ses) %>% 
  summarise(accuracy = mean(is_correct))

data_mean_se_all <- data_subj_all %>% group_by(ses) %>%
  summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>% #ungroup(db) %>%
  mutate(ses = factor(ses,
                     c(1,2),
                     c('first', 'second')))

data_subj_all <- data_subj_all %>%
  mutate(ses = factor(ses,
                     c(1,2),
                     c('first', 'second')))

comp_all <- list(c('first', 'second'))

beeswarm_all <- beeswarm(data = data_subj_all, accuracy ~ ses, method = 'swarm')
ggplot(data = data_subj_all, aes(y = accuracy, x = ses)) +
  geom_bar(data = data_mean_se_all, aes(y = mean, x = ses), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean, x = ses, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Overall accuracy (by order) (N = {n})"))

### Sham first
data_subj_all_sham_first <- data %>%
  filter(subject %in% sham_first) %>%
  group_by(subject, db) %>% 
  summarise(accuracy = mean(is_correct)) %>% #ungroup(cond) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_mean_se_all_sham_first <- data_subj %>% group_by(db) %>%
  summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n_sham_first)) %>% #ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

comp_all <- list(c('sham', 'anode'))

beeswarm_all_sham_first <- beeswarm(data = data_subj_all_sham_first, accuracy ~ db, method = 'swarm')
ggplot(data = data_subj_all_sham_first, aes(y = accuracy, x = db)) +
  geom_bar(data = data_mean_se_all_sham_first, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all_sham_first, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all_sham_first, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Overall accuracy (sham first) (N = {n_sham_first})"))

### Anode first
data_subj_all_anode_first <- data %>%
  filter(subject %in% anode_first) %>%
  group_by(subject, db) %>% 
  summarise(accuracy = mean(is_correct)) %>% #ungroup(cond) %>%
  mutate(db = factor(db,
                     c(2,1),
                     c('anode', 'sham')))

data_mean_se_all_anode_first <- data_subj %>% group_by(db) %>%
  summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n_anode_first)) %>% #ungroup(db) %>%
  mutate(db = factor(db,
                     c(2,1),
                     c('anode', 'sham')))

comp_all <- list(c('anode', 'sham'))

beeswarm_all_anode_first <- beeswarm(data = data_subj_all_anode_first, accuracy ~ db, method = 'swarm')
ggplot(data = data_subj_all_anode_first, aes(y = accuracy, x = db)) +
  geom_bar(data = data_mean_se_all_anode_first, aes(y = mean, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all_anode_first, aes(y = mean, x = db, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_all_anode_first, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Overall accuracy (anode first) (N = {n_anode_first})"))

## Accuracy by condition, sham
data_subj_0 <- data_subj %>% 
  filter(db == 1) %>% ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_0 <-data_subj %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>% 
  filter(db == 1) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))
comp_c <- list(c('Go to win', 'Go to avoid'))
comp_d <- list(c('Nogo to win', 'Nogo to avoid'))

beeswarm_0 <- beeswarm(data = data_subj_0, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_0, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_0, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_0, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_0, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Sham: accuracy by condition (N = {n})"))

## Accuracy by condition, anode
data_subj_1 <- data_subj %>% filter(db == 2) %>% ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_1 <-data_subj %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>%
  filter(db == 2) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))

beeswarm_1 <- beeswarm(data = data_subj_1, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_1, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_1, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_1, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_1, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Anode: accuracy by condition (N = {n})"))

######### Check distribution
sham_GoWin <- data_subj_0 %>% filter(cond == 'Go to win')
anode_GoWin <- data_subj_1 %>% filter(cond == 'Go to win')
sham_NogoWin <- data_subj_0 %>% filter(cond == 'Nogo to win')
anode_NogoWin <- data_subj_1 %>% filter(cond == 'Nogo to win')
sham_GoAvoid <- data_subj_0 %>% filter(cond == 'Go to avoid')
anode_GoAvoid <- data_subj_1 %>% filter(cond == 'Go to avoid')
sham_NogoAvoid <- data_subj_0 %>% filter(cond == 'Nogo to avoid')
anode_NogoAvoid <- data_subj_1 %>% filter(cond == 'Nogo to avoid')

ggplot(sham_GoWin, aes(x=accuracy)) +geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)
  #geom_density(fill = NA, colour=NA, alpha=0.8) + geom_line(stat="density") + expand_limits(y=0) +
  #xlim(0,1) + ylim(0,10)
ggplot(anode_GoWin, aes(x=accuracy)) + geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)

ggplot(sham, aes(x=sham_NogoWin$accuracy)) +geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)
ggplot(sham, aes(x=anode_NogoWin$accuracy)) +geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)

ggplot(sham, aes(x=sham_GoAvoid$accuracy)) + geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)
ggplot(sham, aes(x=anode_GoAvoid$accuracy)) + geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)

ggplot(sham, aes(x=sham_NogoAvoid$accuracy)) + geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)
ggplot(sham, aes(x=anode_NogoAvoid$accuracy)) + geom_histogram() +
  coord_cartesian(xlim = c(0, 1)) + ylim(0,10)
#########

## Check order effect - sham first
## Accuracy by condition, sham
data_subj_0 <- data_subj %>% filter(subject %in% sham_first) %>% 
  filter(db == 1) %>% ungroup(cond) %>% # filter(subject %in% sham_first) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_0 <-data_subj %>% filter(subject %in% sham_first) %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n_sham_first)) %>% 
  filter(db == 1) %>% # filter(subject %in% sham_first) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))

beeswarm_0 <- beeswarm(data = data_subj_0, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_0, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_0, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_0, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_0, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Sham: accuracy by condition (sham first) (N = {n_sham_first})"))

## Accuracy by condition, anode
data_subj_1 <- data_subj %>% filter(subject %in% sham_first) %>% filter(db == 2) %>% ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_1 <-data_subj %>% filter(subject %in% sham_first) %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n_sham_first)) %>%
  filter(db == 2) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))

beeswarm_1 <- beeswarm(data = data_subj_1, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_1, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_1, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_1, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_1, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Anode: accuracy by condition (sham first) (N = {n_sham_first})"))

## Check order effect - anode first
## Accuracy by condition, sham
data_subj_0 <- data_subj %>% filter(subject %in% anode_first) %>% 
  filter(db == 1) %>% ungroup(cond) %>% # filter(subject %in% anode_first) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_0 <-data_subj %>% filter(subject %in% anode_first) %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n_anode_first)) %>% 
  filter(db == 1) %>% # filter(subject %in% anode_first) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))

beeswarm_0 <- beeswarm(data = data_subj_0, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_0, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_0, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_0, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_0, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Sham:accuracy by condition (anode first) (N = {n_anode_first})"))

## Accuracy by condition, anode
data_subj_1 <- data_subj %>% filter(subject %in% anode_first) %>% filter(db == 2) %>% ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

data_mean_se_1 <-data_subj %>% filter(subject %in% anode_first) %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n_anode_first)) %>%
  filter(db == 2) %>%
  mutate(Con = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  ungroup(cond) %>%
  mutate(cond = factor(cond,
                       c('go_reward', 'nogo_reward', 'go_punish', 'nogo_punish'),
                       c('Go to win', 'Nogo to win', 'Go to avoid', 'Nogo to avoid')))

comp_a <- list(c('Go to win', 'Nogo to win'))
comp_b <- list(c('Go to avoid', 'Nogo to avoid'))

beeswarm_1 <- beeswarm(data = data_subj_1, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj_1, aes(y = accuracy, x = cond)) +
  geom_bar(data = data_mean_se_1, aes(y = mean, x = cond, fill = factor(Con)), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_1, aes(y = mean, x = cond, ymin = mean - se, ymax=mean + se), width=.2, position = position_dodge(.9)) +
  geom_point(data = beeswarm_1, aes(y = y, x = x), color = "black", alpha = 0.5) +
  geom_line(aes(group = subject), alpha = 0.1) +
  stat_compare_means(comparisons = comp_a, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  stat_compare_means(comparisons = comp_b, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Anode: accuracy by condition (anode first) (N = {n_anode_first})"))

# Accuracy anode and sham together
data_subj <- data_subj %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

data_mean_se <- data_mean_se %>%
  #ungroup(db) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

#comp <- list(c('go_reward.sham', 'go_reward.anode'),
#             c('nogo_reward.sham', 'nogo_reward.anode'),
#             c('go_punish.sham', 'go_punish.anode'),
#             c('nogo_punish.sham', 'nogo_punish.anode')) 

#beeswarm_all <- beeswarm(data = data_subj, accuracy ~ cond, method = 'swarm')
ggplot(data = data_subj, aes(y = accuracy, x = db)) +
  geom_bar(data = data_mean_se, aes(y = mean, x = db, fill = cond), stat = "identity", color = "black", position="dodge") +
  geom_errorbar(data = data_mean_se, aes(y = mean, x = db, fill = cond, ymin = mean - se, ymax=mean + se),
                width=.2, position = position_dodge(.9)) +
  #geom_point(data = beeswarm_all, aes(y = y, x = x), color = "black", alpha = 0.5) #+
  #geom_line(aes(group = subject), alpha = 0.1) +
  facet_wrap(~cond)+  
  stat_compare_means(paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 1.05) +
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  #theme_bw() + theme(legend.position = "none",
  #                   axis.text.x = element_text(size = 11),
  #                   axis.title.y = element_text(size = 13),
  #                   title = element_text(size = 15)) +
  ggtitle(str_glue("Overall accuracy (N = {n})"))

dev.off()


# Add results in BRL-indices-all.csv
IDX_DIR = '/home/kimhj9502/Project/project_WMDM/behavior_analysis/data-organized'
idxAll = read_csv(str_glue('{IDX_DIR}/BRL-indices-all.csv'))

## GNG acc
GNG_acc_overall_tdcs <- data %>% group_by(subject) %>%
  summarise(accuracy = mean(is_correct)) %>%
  rename(GNG_acc_overall_tdcs = accuracy)

GNG_acc_sham_tdcs <- data %>% 
  filter(db == 1) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(is_correct)) %>%
  rename(GNG_acc_sham_tdcs = accuracy)

GNG_acc_anode_tdcs <- data %>% 
  filter(db == 2) %>%
  group_by(subject) %>%
  summarise(accuracy = mean(is_correct)) %>%
  rename(GNG_acc_anode_tdcs = accuracy)

cond_sham_tdcs <- data %>% 
  filter(db == 1) %>%
  group_by(subject, cond) %>%
  summarise(accuracy = mean(is_correct)) %>%
  spread(cond, accuracy) %>%
  rename(go_punish_sham_tdcs = go_punish,
         go_reward_sham_tdcs = go_reward,
         nogo_punish_sham_tdcs = nogo_punish,
         nogo_reward_sham_tdcs = nogo_reward) 

cond_anode_tdcs <- data %>% 
  filter(db == 2) %>%
  group_by(subject, cond) %>%
  summarise(accuracy = mean(is_correct)) %>%
  spread(cond, accuracy) %>%
  rename(go_punish_anode_tdcs = go_punish,
         go_reward_anode_tdcs = go_reward,
         nogo_punish_anode_tdcs = nogo_punish,
         nogo_reward_anode_tdcs = nogo_reward) 

merge <- left_join(GNG_acc_overall_tdcs, GNG_acc_sham_tdcs, by = "subject") %>%
  left_join(GNG_acc_anode_tdcs, by = "subject") %>%
  left_join(cond_sham_tdcs, by = "subject") %>%
  left_join(cond_anode_tdcs, by = "subject")

left_join(idxAll, merge, by = "subject") %>%
  write_csv(str_glue('{IDX_DIR}/BRL-indices-all-hjEdits.csv'))

