
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

data_subj_all <- data_subj_all %>% gather(colname, value, starts_with('pavBias_')) %>% 
  mutate(colname = str_c(db, '_', colname)) %>% 
  select(-db) %>% spread(colname, value)
write.csv(data_subj_all, file = '/home/kimhj9502/Project/project_tDCS/analysis/behav/pavBias.csv')
###########
shamPavAll <- data_subj_all %>% filter(db == 'sham')
shamPavAll_ls <- shamPavAll$pavBias

anodePavAll <- data_subj_all %>% filter(db == 'anode')
anodePavAll_ls <- anodePavAll$pavBias

behav_PavAll_del <- anodePavAll_ls - shamPavAll_ls
###########

## pavBias by valence condition (tdcs)
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

###########
shamPav <- data_subj_all %>% filter(db == 'sham')
shamPavRew <- shamPav$pavBias_rew
shamPavPun <- shamPav$pavBias_pun

anodePav <- data_subj_all %>% filter(db == 'anode')
anodePavRew <- anodePav$pavBias_rew
anodePavPun <- anodePav$pavBias_pun

behav_PavRew_del <- anodePavRew - shamPavRew
behav_PavPun_del <- anodePavPun - shamPavPun

###########

#----------------------------
# PPB 
data_subj_all <- data %>% group_by(subject, db, cond) %>% 
  summarise(go_count = sum(key_pressed==1), nogo_count = sum(key_pressed==0))  %>%
  #summarise(nogo_count = sum(key_pressed==0))  %>%
  #group_by(subject, db) %>%
  #summarise(go_sum_all = sum(go_count)) %>% #ungroup()  %>%
  mutate(rew_val = ifelse(cond == 'go_reward' | cond == 'nogo_reward', 1, 0)) %>%
  group_by(subject, db, rew_val) %>%
  summarise(go_sum = sum(go_count), nogo_sum = sum(nogo_count)) %>%
  #summarise(nogo_sum = sum(nogo_count)) %>%
  #summarise(go_sum_all = sum(go_sum)) %>%
  group_by(subject, db) %>%
  summarize(RBI = go_sum[rew_val == 1]/(go_sum[rew_val == 1] + go_sum[rew_val == 0]),
            PBS = nogo_sum[rew_val == 0]/(nogo_sum[rew_val == 1] + nogo_sum[rew_val == 0]),
            PPB = (RBI + PBS) / 2) %>%
  mutate(db = factor(db,
                     c(1,2),
                     c('sham', 'anode')))

#######
shamPPB <- data_subj_all %>% filter(db == 'sham') %>% select(c('RBI','PBS','PPB'))
anodePPB <- data_subj_all %>% filter(db == 'anode') %>% select(c('RBI','PBS','PPB'))

PPB_del <- anodePPB - shamPPB
pavBias <- data.frame(behav_PavAll_del,
                      behav_PavRew_del,
                      behav_PavPun_del)
all <- cbind(PPB_del, pavBias, df, df2)

corr <- cbind(all, df3)
write.csv(corr, 'for_corr.csv')
#########
data_mean_se_all_RBI <- data_subj_all %>% 
  group_by(db) %>%
  summarise(mean_RBI = mean(RBI), se_RBI = sd(RBI) / sqrt(n)) 

data_mean_se_all_PBS <- data_subj_all %>% 
  group_by(db) %>%
  summarise(mean_PBS = mean(PBS), se_PBS = sd(PBS) / sqrt(n)) 

data_mean_se_all_PPB <- data_subj_all %>% 
  group_by(db) %>%
  summarise(mean_PPB = mean(PPB), se_PPB = sd(PPB) / sqrt(n)) 

# Plot
comp_all <- list(c('sham', 'anode')) 
ggplot(data = data_subj_all, aes(y = RBI, x = db)) +
  geom_bar(data = data_mean_se_all, aes(y = mean_RBI, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all, aes(y = mean_RBI, x = db, ymin = mean_RBI - se_RBI, ymax=mean_RBI + se_RBI), width=.2, position = position_dodge(.9)) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 0.65) +
  coord_cartesian(ylim=c(0, 0.7)) +
  ylab("RBI")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("RBI (N = {n})"))

ggplot(data = data_subj_all, aes(y = PBS, x = db)) +
  geom_bar(data = data_mean_se_all_PBS, aes(y = mean_PBS, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all_PBS, aes(y = mean_PBS, x = db, ymin = mean_PBS - se_PBS, ymax=mean_PBS + se_PBS), width=.2, position = position_dodge(.9)) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 0.65) +
  coord_cartesian(ylim=c(0, 0.7)) +
  ylab("PBS")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("PBS (N = {n})"))

ggplot(data = data_subj_all, aes(y = PPB, x = db)) +
  geom_bar(data = data_mean_se_all_PPB, aes(y = mean_PPB, x = db), stat = "identity", color = "black") +
  geom_errorbar(data = data_mean_se_all_PPB, aes(y = mean_PPB, x = db, ymin = mean_PPB - se_PPB, ymax=mean_PPB + se_PPB), width=.2, position = position_dodge(.9)) +
  stat_compare_means(comparisons = comp_all, paired = T, hide.ns = F, method = "t.test", label = "p.signif", label.y = 0.65) +
  coord_cartesian(ylim=c(0, 0.7)) +
  ylab("PPB")+xlab("") +
  theme_bw() + theme(legend.position = "none",
                     axis.text.x = element_text(size = 11),
                     axis.title.y = element_text(size = 13),
                     title = element_text(size = 15)) +
  ggtitle(str_glue("PPB (N = {n})"))

######### Check distribution
sham_RBI <- data_subj_all %>% filter(db == 'sham')
anode_RBI <- data_subj_all %>% filter(db == 'anode')
ggplot(sham_RBI, aes(x=RBI)) + geom_histogram() +
  coord_cartesian(xlim = c(-0, 1)) + ylim(0,10)
  #geom_density(fill = NA, colour=NA, alpha=0.8) + geom_line(stat="density") + expand_limits(y=0) + 
  #xlim(0,1) + ylim(0,15)
ggplot(anode_RBI, aes(x=RBI)) + geom_histogram() +
  coord_cartesian(xlim = c(-0, 1)) + ylim(0,10)

sham_PBS <- data_subj_all %>% filter(db == 'sham')
anode_PBS <- data_subj_all %>% filter(db == 'anode')
ggplot(sham_PBS, aes(x=PBS)) + geom_histogram()+
  coord_cartesian(xlim = c(-0, 1)) + ylim(0,10)
ggplot(anode_PBS, aes(x=PBS)) + geom_histogram() +
  coord_cartesian(xlim = c(-0, 1)) + ylim(0,10)

sham_PPB <- data_subj_all %>% filter(db == 'sham')
anode_PPB <- data_subj_all %>% filter(db == 'anode')
ggplot(sham_PPB, aes(x=PPB)) + geom_histogram()+
  coord_cartesian(xlim = c(-0, 1)) + ylim(0,10)
ggplot(anode_PPB, aes(x=PPB)) + geom_histogram() +
  coord_cartesian(xlim = c(-0, 1)) + ylim(0,10)
#########
# Pav bias and PPB correlation
sham_RBI <- data_subj_all %>% filter(db == 'sham') %>% select('RBI')
anode_RBI <- data_subj_all %>% filter(db == 'anode') %>% select('RBI')
sham_PBS <- data_subj_all %>% filter(db == 'sham') %>% select('PBS')
anode_PBS <- data_subj_all %>% filter(db == 'anode') %>% select('PBS')
sham_PPB <- data_subj_all %>% filter(db == 'sham') %>% select('PPB')
anode_PPB <- data_subj_all %>% filter(db == 'anode') %>% select('PPB')

sham_pavRew <- data_subj_all %>% filter(db == 'sham') %>% select('pavBias_rew')
anode_pavRew <- data_subj_all %>% filter(db == 'anode') %>% select('pavBias_rew')
sham_pavPun <- data_subj_all %>% filter(db == 'sham') %>% select('pavBias_pun')
anode_pavPun <- data_subj_all %>% filter(db == 'anode') %>% select('pavBias_pun')
sham_pav <- data_subj_all %>% filter(db == 'sham') %>% select('pavBias')
anode_pav <- data_subj_all %>% filter(db == 'anode') %>% select('pavBias')

df_corr <- data.frame(sham_RBI=sham_RBI$RBI,
                      anode_RBI=anode_RBI$RBI,
                      sham_PBS=sham_PBS$PBS,
                      anode_PBS=anode_PBS$PBS,
                      sham_PPB=sham_PPB$PPB,
                      anode_PPB=anode_PPB$PPB,
                      sham_pavRew=sham_pavRew$pavBias_rew,
                      anode_pavRew=anode_pavRew$pavBias_rew,
                      sham_pavPun=sham_pavPun$pavBias_pun,
                      anode_pavPun=anode_pavPun$pavBias_pun,
                      sham_pav=sham_pav$pavBias,
                      anode_pav=anode_pav$pavBias)
cormat <- round(cor(df_corr),2)
melted_cormat <- melt(cormat)

ggplot(data =melted_df_corr, aes(x=variable, y=variable, fill=value)) + 
  geom_tile()
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(-0.13, 0),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

ggplot(df_corr, aes(x=b_0, y=b_delta)) + 
  geom_point(aes(colour=rownames(df_corr)))+
  geom_smooth(method=lm, color="lightblue") +
  ggtitle("Correlation b_0 & b_delta") + 
  theme_bw() +
  annotate("text", x = 1, y = 1, label = cor(df_corr$b_0, df_corr$b_delta))

#########
# PPB (order)
data_subj_all <- data %>% group_by(subject, ses, cond) %>% 
  summarise(go_count = sum(key_pressed==1), nogo_count = sum(key_pressed==0))  %>%
  mutate(rew_val = ifelse(cond == 'go_reward' | cond == 'nogo_reward', 1, 0)) %>%
  group_by(subject, ses, rew_val) %>%
  summarise(go_sum = sum(go_count), nogo_sum = sum(nogo_count)) %>%
  group_by(subject, ses) %>%
  summarize(RBI = go_sum[rew_val == 1]/(go_sum[rew_val == 1] + go_sum[rew_val == 0]),
            PBS = nogo_sum[rew_val == 0]/(nogo_sum[rew_val == 1] + nogo_sum[rew_val == 0]),
            PPB = (RBI + PBS) / 2)

# Paired t-test
res <- t.test(PPB ~ ses, data = data_subj_all, paired = TRUE)
res

# Paired t-test
res <- t.test(RBI ~ ses, data = data_subj_all, paired = TRUE)
res

# Paired t-test
res <- t.test(PBS ~ ses, data = data_subj_all, paired = TRUE)
res

