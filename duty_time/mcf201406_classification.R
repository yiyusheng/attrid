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

mcfD_dci <- mcf_group(duty_event,duty_censored,cmdb_dci,'dci')
mcfA_dci <- mcf_group(age_event,age_censored,cmdb_dci,'dci')

# duty time
p1 <- ggplot(subset(mcfD_dci,dci == 'TS'),aes(x = life,y = mcf,linetype = dci)) + geom_line() + geom_hline(yintercept = 1,linetype = 4,color = 'red') +
  geom_smooth(method='lm',color = 'red')
# age
p2 <- ggplot(subset(mcfA_dci,dci == 'TS'),aes(x = life,y = mcf,linetype = dci)) + geom_line() + geom_hline(yintercept = 1,linetype = 4,color = 'red') +
  geom_smooth(method='lm',color = 'red')
p3 <- ggplot(subset(mcfD_dci,dci == 'TS'),aes(x = life,y = c(0,diff(mcf)),linetype = dci)) + geom_line()
p4 <- ggplot(subset(mcfA_dci,dci == 'TS'),aes(x = life,y = c(0,diff(mcf)),linetype = dci)) + geom_line()
multiplot(p1,p2,p3,p4,cols = 2)

# S2. Linear analysis
lmD <- glm(mcf~life,subset(mcfD_dci,dci == 'TS'),family = 'gaussian')
lmA <- glm(mcf~life,subset(mcfA_dci,dci == 'TS'),family = 'gaussian')
