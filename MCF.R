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
require('reda')
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')

load(file.path(dir_dataCF,'uwork2013.Rda')) #39158
load(file.path(dir_dataCF,'serverInfo.Rda'))
fstart <- as.p('2012-12-01')
fend <- as.p('2013-12-01')

f2013 <- revise_ftype(f2013)
f2013 <- filter_ip(f2013) #39141
f2013 <- check_disk_replacement(f2013) #23964
f2013 <- subset(f2013,ftime > fstart & ftime < fend) #23249
f2013$utime <- cmdb$use_time[match(f2013$svrid,cmdb$svr_asset_id)];f2013 <- subsetX(f2013,!is.na(utime)) #21554
f2013$life <- as.numeric(difftime(f2013$ftime,f2013$utime,tz = 'UTC',units = 'days'))
f2013$event <- 1

cmdb_censor_left <- cmdb[,c('svr_asset_id','use_time')];names(cmdb_censor_left) <- c('svrid','utime')
# cmdb_censor_left$event <- 0;cmdb_censor_left$life <- as.numeric(difftime(fstart,cmdb_censor_left$utime,tz = 'UTC',units = 'days'))
cmdb_censor_right <- cmdb_censor_left;cmdb_censor_right$event <- 0;cmdb_censor_right$life <- as.numeric(difftime(fend,cmdb_censor_left$utime,tz = 'UTC',units = 'days'))
cmdb_censor_left <- NULL
cmdb_censor <- rbind(cmdb_censor_left,cmdb_censor_right);cmdb_censor$life[cmdb_censor$life < 0] <- 0

DT <- rbind(f2013[,c('svrid','life','event')],cmdb_censor[,c('svrid','life','event')])
valueMcf <- mcf(Survr(svrid, life, event) ~ 1, data = DT)
