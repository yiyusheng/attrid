#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mcf201406_classification.R
#
# Description: comparison of mcf-age and mcf-dutytime on server type and disk model
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-23 09:32:55
#
# Last   modified: 2017-05-23 09:32:57
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/duty_time/');source('~/rhead')
source('~/Code/R/Disk_Workload/Failure/mcfFunc.R')
load(file.path(dir_data,'mcf201406_data.Rda'))

# S1. classify by dev_class_id
load(file.path(dir_dataCF,'serverInfo.Rda'))
cmdb_dci <- setNames(cmdb[,c('svr_asset_id','dev_class_id')],c('svrid','dev_class_id'))
cmdb_dci$dci <- 'None'
cmdb_dci$dci[cmdb_dci$dev_class_id == 'C1'] <- 'C'
cmdb_dci$dci[cmdb_dci$dev_class_id %in% c('TS1','TS3','TS4','TS5','TS6')] <- 'TS'
cmdb_dci$dci <- factor(cmdb_dci$dci)
mcfD_dci <- mcf_group(duty_event,duty_censored,cmdb_dci,'dci',30)
mcfA_dci <- mcf_group(age_event,age_censored,cmdb_dci,'dci',30)

# S2. plot mcf of dt/age
# duty time
p1 <- ggplot(subset(mcfD_dci,dci == 'TS'),aes(x = life,y = mcf,linetype = dci)) + geom_line() + geom_hline(yintercept = 1,linetype = 4,color = 'red') +
  geom_smooth(method='lm',color = 'red')
# age
p2 <- ggplot(subset(mcfA_dci,dci == 'TS'),aes(x = life,y = mcf,linetype = dci)) + geom_line() + geom_hline(yintercept = 1,linetype = 4,color = 'red') +
  geom_smooth(method='lm',color = 'red')
multiplot(p1,p2,cols = 1)

# S3. Linear analysis
lmD <- glm(mcf~life,subset(mcfD_dci,dci == 'TS'),family = 'gaussian')
lmA <- glm(mcf~life,subset(mcfA_dci,dci == 'TS'),family = 'gaussian')

# S4. number of atrisk in dt/age
myDCI <- 'TS'
mcfD <- subset(mcfD_dci,dci == myDCI)
mcfA <- subset(mcfA_dci,dci == myDCI)
mcf_atrisk <- merge(mcfD[,c('life','count','countF','mcf','rate_day')],mcfA[,c('life','count','countF','mcf','rate_day')],by = 'life',all = T)
mcf_atrisk <- replace_value(mcf_atrisk)
names(mcf_atrisk) <- c('life','atriskD','failD','mcfD','ratioD','atriskA','failA','mcfA','ratioA')

# number
n1 <- ggplot(mcf_atrisk,aes(x = life,y = atriskD)) + geom_bar(stat = 'identity')
n2 <- ggplot(mcf_atrisk,aes(x = life,y = atriskA)) + geom_bar(stat = 'identity')
n3 <- ggplot(mcf_atrisk,aes(x = life,y = failD)) + geom_bar(stat = 'identity')
n4 <- ggplot(mcf_atrisk,aes(x = life,y = failA)) + geom_bar(stat = 'identity')
multiplot(n1,n2,n3,n4,cols = 2)

# distribution
ma_dist <- with(mcf_atrisk,rbind(
  data.frame(life = life,value = cumsum(array_rate(atriskD)),group = 'atrisk',lifetype = 'DT'),
  data.frame(life = life,value = cumsum(array_rate(atriskA)),group = 'atrisk',lifetype = 'AGE'),
  data.frame(life = life,value = cumsum(array_rate(failD)),group = 'fail',lifetype = 'DT'),
  data.frame(life = life,value = cumsum(array_rate(failA)),group = 'fail',lifetype = 'AGE')))

d <- ggplot(ma_dist,aes(x = life,color = group,y = value,linetype = lifetype)) + geom_line() + geom_vline(xintercept = 5,linetype = 3)
d

# S5. ratio of number of fails and number of atrisks per day/week/10 days
r1 <- ggplot(mcf_atrisk,aes(x = life,y = ratioD)) + geom_bar(stat = 'identity') + ylim(c(0,.25))
r2 <- ggplot(mcf_atrisk,aes(x = life,y = ratioA)) + geom_bar(stat = 'identity') + ylim(c(0,.13)) +scale_x_continuous(breaks = seq(0,60,12))
multiplot(r1,r2,cols = 1)
