#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: MCF.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-21 15:35:30
#
# Last   modified: 2017-04-21 15:35:32
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('Failure/mcfFunc.R')

# S1. Load
load(file.path(dir_dataCF,'uwork2013.Rda')) #39158
load(file.path(dir_dataCF,'serverInfo.Rda'))
dateS <- as.p('2012-12-01')
dateE <- as.p('2013-12-01')

# S2. preprocess of fail record
f2013 <- revise_ftype(f2013)
f2013 <- filter_ip(f2013) #39141
f2013 <- check_disk_replacement(f2013) #23964
f2013 <- subset(f2013,ftime > dateS & ftime < dateE) #23249
f2013$utime <- cmdb$use_time[match(f2013$svrid,cmdb$svr_asset_id)]
f2013 <- subsetX(f2013,!is.na(utime)) #21554
f2013$life <- round(as.numeric(difftime(f2013$ftime,f2013$utime,tz = 'UTC',units = 'days')))
f2013$event <- 1
event_time <- f2013$life

# S3. preprocess of all objects
cmdb_censor <- cmdb[,c('svr_asset_id','use_time')]
names(cmdb_censor) <- c('svrid','utime')
cmdb_censor$ageS <- round(as.numeric(difftime(dateS,cmdb_censor$utime)))
cmdb_censor$ageS[cmdb_censor$ageS < 0] <- 0
cmdb_censor$ageE <- round(as.numeric(difftime(dateE,cmdb_censor$utime)))
life_censored <- cmdb_censor[,c('ageS','ageE')]

# S4. generate mcf
mcf_df <- mcf_cencored(f2013$life,cmdb_censor)
ggplot(mcf_df,aes(x = age,y = mcf)) + geom_line()
