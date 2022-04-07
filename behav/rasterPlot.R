
# Load pacakges
rm(list=ls())
library(raster)
library(tidyverse)

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
dat = rawdata %>% filter(subject != 4) %>% # IRB issue
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
## N = 27
anode_first = c(28, 18, 33, 60, 63, 64, 76, 91, 86, 59, 105, 107, 109, 112, 121, 127) # 14, 61, 103, 116, 126
sham_first = c(30, 54, 43, 11, 73, 79, 101, 108, 110, 104, 125) # 4, 15, 27, 69, 94, 102, 113

# Number of subjects
n = length(unique(data[,"subject"]))
n_sham_first = length(sham_first)
n_anode_first = length(anode_first)

########################################
# To check by anode or sham            #
dat <- dat %>% filter(db==1) # sham  #
# dat <- dat %>% filter(db==2) # anode #
########################################

# Set variables
subjList =  unique(dat[,"subject"])  # list of subjects x blocks
numSubjs = length(subjList)  # number of subjects
numConds = 4
maxTrials = dim(dat)[1] / numSubjs
maxtrialsPerCond = maxTrials/ numConds
trialsPerCond = maxTrials/ numConds

# condition: go_reward, nogoreward, go_punish, nogo_punish
# create behavRaster matrix
behavRaster = array(NA, c(numSubjs, trialsPerCond, numConds))

goWin <- raster(nrow=numSubjs, ncol=trialsPerCond, xmn=1, xmx=trialsPerCond, ymn=0, ymx=1)
goAvoid <- raster(nrow=numSubjs, ncol=trialsPerCond, xmn=1, xmx=trialsPerCond, ymn=0, ymx=1)
nogoWin <- raster(nrow=numSubjs, ncol=trialsPerCond, xmn=1, xmx=trialsPerCond, ymn=0, ymx=1)
nogoAvoid <- raster(nrow=numSubjs, ncol=trialsPerCond, xmn=1, xmx=trialsPerCond, ymn=0, ymx=1)

#goWin = array(NA, c(numSubjs, trialsPerCond))

for (i in 1:numSubjs) {
  curSubj = subjList[i]
  tmp = subset(dat, subject == curSubj)
  goWinTmp = subset(tmp, cond=='go_reward')
  goAvoidTmp = subset(tmp, cond=='go_punish')
  nogoWinTmp = subset(tmp, cond=='nogo_reward')
  nogoAvoidTmp = subset(tmp, cond=='nogo_punish')
  
  goWin[i, 1:trialsPerCond] = pmax(goWinTmp$key_pressed)
  goAvoid[i, 1:trialsPerCond] = goAvoidTmp$key_pressed
  nogoWin[i, 1:trialsPerCond] = nogoWinTmp$key_pressed
  nogoAvoid[i, 1:trialsPerCond] = nogoAvoidTmp$key_pressed
}

#pdf(str_glue("{SAVE_DIR}/raster_anode_N{n}.pdf"))
pdf(str_glue("{SAVE_DIR}/raster_sham_N{n}.pdf"))

quartz(width=12, height=4); par(mai=c(0.6732, 0.6, 0.5412, 0.2772))
layout( matrix(1:numConds, 1, numConds, byrow=F))
## GoWin condition
image(goWin, col = gray.colors(n=10, start=0.7, end=1), main="Go to win", xlab="Trial", ylab="Probability(Go)", cex.lab=2, cex.main=2, cex.axis=1.5)
groupMean = apply( as.array(goWin), 2, mean)
lines(1:trialsPerCond, groupMean, lwd=5, col=rgb(0,0,0,.8))
if (loadedSimulationRBS) lines(1:trialsPerCond, pGoMean[1,], lwd=5, col=rgb(1,0,0,0.6)) 

## GoAvoid condition
image(goAvoid, col = gray.colors(n=10, start=0.7, end=1), main="Go to avoid", xlab="Trial", ylab="", cex.lab=2, cex.main=2, cex.axis=1.5 )
groupMean = apply( as.array(goAvoid), 2, mean)
lines(1:trialsPerCond, groupMean, lwd=5, col=rgb(0,0,0,.8))
if (loadedSimulationRBS) lines(1:trialsPerCond, pGoMean[3,], lwd=5, col=rgb(1,0,0,0.6)) 

## nogoWin condition
image(nogoWin, col = gray.colors(n=10, start=0.7, end=1), main="Nogo to win", xlab="Trial", ylab="", cex.lab=2, cex.main=2, cex.axis=1.5 )
groupMean = apply( as.array(nogoWin), 2, mean)
lines(1:trialsPerCond, groupMean, lwd=5, col=rgb(0,0,0,.8))
if (loadedSimulationRBS) lines(1:trialsPerCond, pGoMean[2,], lwd=5, col=rgb(1,0,0,0.6)) 

## NogoAvoid condition
image(nogoAvoid, col = gray.colors(n=10, start=0.7, end=1), main="Nogo to avoid", xlab="Trial", ylab="", cex.lab=2, cex.main=2, cex.axis=1.5 )
groupMean = apply( as.array(nogoAvoid), 2, mean)
lines(1:trialsPerCond, groupMean, lwd=5, col=rgb(0,0,0,.8))
if (loadedSimulationRBS) lines(1:trialsPerCond, pGoMean[4,], lwd=5, col=rgb(1,0,0,0.6)) 

dev.off()