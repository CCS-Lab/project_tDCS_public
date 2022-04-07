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

# Make accuracy data
data_subj <- data %>% group_by(subject, cond, db) %>% summarise(accuracy = mean(is_correct))
data_mean_se <- data_subj %>% group_by(db, cond) %>% summarise(mean = mean(accuracy),
                                                               se = sd(accuracy) / sqrt(n))

## Accuracy by condition, sham
data_subj_int <- data_subj %>% 
  mutate(Con = case_when(cond == 'go_reward' & db == 1 ~ 'sham.go_reward',
                         cond == 'nogo_reward' & db == 1 ~ 'sham.nogo_reward',
                         cond == 'go_punish' & db == 1 ~ 'sham.go_punish',
                         cond == 'nogo_punish' & db == 1 ~ 'sham.nogo_punish',
                         cond == 'go_reward' & db == 2 ~ 'anode.go_reward',
                         cond == 'nogo_reward' & db == 2 ~ 'anode.nogo_reward',
                         cond == 'go_punish' & db == 2 ~ 'anode.go_punish',
                         cond == 'nogo_punish' & db == 2 ~ 'anode.nogo_punish'))
  
data_mean_se_int <-data_subj %>%
  group_by(db, cond) %>% summarise(mean = mean(accuracy), se = sd(accuracy) / sqrt(n)) %>% 
  mutate(Cong = ifelse(cond == 'go_reward' | cond == 'nogo_punish', 1, 0)) %>%
  mutate(Con = case_when(cond == 'go_reward' & db == 1 ~ 'sham.go_reward',
                         cond == 'nogo_reward' & db == 1 ~ 'sham.nogo_reward',
                         cond == 'go_punish' & db == 1 ~ 'sham.go_punish',
                         cond == 'nogo_punish' & db == 1 ~ 'sham.nogo_punish',
                         cond == 'go_reward' & db == 2 ~ 'anode.go_reward',
                         cond == 'nogo_reward' & db == 2 ~ 'anode.nogo_reward',
                         cond == 'go_punish' & db == 2 ~ 'anode.go_punish',
                         cond == 'nogo_punish' & db == 2 ~ 'anode.nogo_punish'))

#beeswarm <- beeswarm(data = data_subj_int, accuracy ~ Con, method = 'center')

level_order <- c('sham.go_reward','sham.nogo_reward','sham.go_punish','sham.nogo_punish',
                 'anode.go_reward','anode.nogo_reward','anode.go_punish','anode.nogo_punish')

ggplot(data = data_subj_int, aes(y = accuracy, x = factor(Con, level =level_order))) +
  geom_bar(data = data_mean_se_int,
         aes(y = mean, x = factor(Con, level =level_order), fill = factor(Cong)), 
         stat = "identity", color = "black") +
  geom_point() +
  geom_line(aes(group = subject, alpha = 0.1))+
  coord_cartesian(ylim=c(0, 1.1)) +
  ylab("Choice Accuracy")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("Sham & Anode: accuracy by condition (N = {n})"))

data_subj_low <- data_subj_int %>% 
  filter(accuracy < 0.3)
