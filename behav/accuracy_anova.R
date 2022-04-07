rm(list=ls())

# Load packages
library(stringr)
library(tidyverse)
library(rstatix)


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

## N = 27
anode_first = c(28, 18, 33, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127) # 14, 61, 103, 116, 126
sham_first = c(30, 54, 43, 11, 73, 79, 101, 108, 110, 104, 125) # 4, 15, 27, 69, 94, 102, 113

# Number of subjects
n = length(unique(data[,"subject"]))
n_sham_first = length(sham_first)
n_anode_first = length(anode_first)

# action x valence x tdcs
# Make accuracy data
data_subj_ <- data %>% group_by(subject, cond, db) %>%
  summarise(accuracy = mean(is_correct))

data_subj1 <- data_subj_  %>% #ungroup() %>%
  dplyr::mutate(tdcs=ifelse(db==1, 'sham', 'anode')) %>%
  dplyr::mutate(action=ifelse(str_detect(cond, "nogo_"), 'nogo', 'go')) %>%
  dplyr::mutate(valence=ifelse(str_detect(cond, "reward"), 'reward', 'punish')) %>% ungroup() %>%
  #select(subject, tdcs, action, valence) %>%
  convert_as_factor(subject, tdcs, action, valence)

res.aov <- rstatix::anova_test(
  data = data_subj1, dv = accuracy, wid = subject,
  within = c(tdcs, action, valence)
)

get_anova_table(res.aov)

# Post-hoc
one.way <- data_subj1 %>%
  group_by(action,tdcs) %>%
  anova_test(dv = accuracy, wid = subject, within = valence) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way <- data_subj1 %>%
  group_by(valence,tdcs) %>%
  anova_test(dv = accuracy, wid = subject, within = action) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# action x valence x order
# Make accuracy data
data_subj_ <- data %>% group_by(subject, cond, ses) %>%
  summarise(accuracy = mean(is_correct))

data_subj1 <- data_subj_  %>% #ungroup() %>%
  dplyr::mutate(order=ifelse(ses==1, 'first', 'second')) %>%
  dplyr::mutate(action=ifelse(str_detect(cond, "nogo_"), 'nogo', 'go')) %>%
  dplyr::mutate(valence=ifelse(str_detect(cond, "reward"), 'reward', 'punish')) %>% ungroup() %>%
  #select(subject, tdcs, action, valence) %>%
  convert_as_factor(subject, order, action, valence)

res.aov <- rstatix::anova_test(
  data = data_subj1, dv = accuracy, wid = subject,
  within = c(order, action, valence)
)

get_anova_table(res.aov)

# Post-hoc
pwc <- data_subj1 %>%
  pairwise_t_test(
    accuracy ~ order, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

res <- t.test(accuracy ~ order, data = data_subj1, paired = TRUE)
res

one.way <- data_subj1 %>%
  group_by(action,order) %>%
  anova_test(dv = accuracy, wid = subject, within = valence) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way <- data_subj1 %>%
  group_by(valence,order) %>%
  anova_test(dv = accuracy, wid = subject, within = action) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# action x valence x order (three-way interaction) 은 없다.

# action x valence x order x tdcs --> 안된다.
# Make accuracy data
data_subj_ <- data %>% group_by(subject, cond, ses, db) %>%
  summarise(accuracy = mean(is_correct))

data_subj1 <- data_subj_  %>% #ungroup() %>%
  dplyr::mutate(order=ifelse(ses==1, 'first', 'second')) %>%
  dplyr::mutate(tdcs=ifelse(db==1, 'sham', 'anode')) %>%
  dplyr::mutate(action=ifelse(str_detect(cond, "nogo_"), 'nogo', 'go')) %>%
  dplyr::mutate(valence=ifelse(str_detect(cond, "reward"), 'reward', 'punish')) %>% ungroup() %>%
  #select(subject, tdcs, action, valence) %>%
  convert_as_factor(subject, order, tdcs, action, valence)

res.aov <- rstatix::anova_test(
  data = data_subj1, dv = accuracy, wid = subject,
  within = c(order, tdcs, action, valence)
)

get_anova_table(res.aov)

# Post-hoc
pwc <- data_subj1 %>%
  pairwise_t_test(
    accuracy ~ order, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

res <- t.test(accuracy ~ order, data = data_subj1, paired = TRUE)
res

one.way <- data_subj1 %>%
  group_by(action,order) %>%
  anova_test(dv = accuracy, wid = subject, within = valence) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way <- data_subj1 %>%
  group_by(valence,order) %>%
  anova_test(dv = accuracy, wid = subject, within = action) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# action x valence x order (three-way interaction) 은 없다.