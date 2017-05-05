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
load(file.path(dir_dataCF,'uwork2014_06_09.Rda')) #52376
load(file.path(dir_dataCF,'serverInfo.Rda'))
load(file.path(dir_dataDW,'duty_time_groupby_mean.Rda'))
dateS <- as.p('2014-06-01')
dateE <- as.p('2014-10-01')

# S2. preprocess of fail record
FR <- f2014_06_09
FR <- revise_ftype(FR)
FR <- filter_ip(FR) #52273
FR <- check_disk_replacement(FR) #9413
FR$utime <- cmdb$use_time[match(FR$svrid,cmdb$svr_asset_id)]
FR <- subsetX(FR,!is.na(utime)) #8988
FR$life <- round(as.numeric(difftime(FR$ftime,FR$utime,tz = 'UTC',units = 'days')))


# S3. preprocess of all objects
cmdb_censor <- cmdb[,c('svr_asset_id','use_time')]
names(cmdb_censor) <- c('svrid','utime')
cmdb_censor$ageS <- round(as.numeric(difftime(dateS,cmdb_censor$utime)))
cmdb_censor$ageS[cmdb_censor$ageS < 0] <- 0
cmdb_censor$ageE <- round(as.numeric(difftime(dateE,cmdb_censor$utime)))

# S4. generate mcf
event_time <- FR[,c('svrid','life')]
event_time <- subsetX(event_time,svrid %in% DT_mean$svrid) #4143

life_censored <- cmdb_censor[,c('svrid','ageS','ageE')]
life_censored <- subsetX(life_censored,svrid %in% DT_mean$svrid) #129114
mcf_df <- mcf_cencored(FR$life,cmdb_censor)
ggplot(mcf_df,aes(x = age,y = mcf)) + geom_line()
