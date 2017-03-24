#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: bs_virtdisk.R
#
# Description: bootstraping of virtdisk to simulate all disks
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-17 11:31:00
#
# Last   modified: 2017-01-17 11:31:02
#
#
#
rm(list = ls())
source('head.R')
dir_ten <- '/home/yiyusheng/Data/tencProcess'
load(file.path(dir_data,'virtDisk.Rda'))
load(file.path(dir_ten,'diskInfo0902.Rda'))

bt_start <- as.p('2014-07-01')
bt_end <- as.p('2014-10-01')
disk_num <- 332842

vt_fail <- factorX(subset(virtDisk,status == 'failed' & f_time >= bt_start & f_time < bt_end & use_time < bt_end))
vt_fail$group <- 'positive'

first_time_fail <- melt(tapply(virtDisk$f_time,virtDisk$sid,min))
first_time_fail$value <- as.POSIXct(first_time_fail$value,tz = 'UTC',origin = '1970-01-01')
vt_norm <- subset(virtDisk,sid %in% first_time_fail$Var1[first_time_fail$value > bt_end + 86400 * 90] & use_time < bt_end)
vt_norm_smp <- factorX(vt_norm[sample(1:nrow(vt_norm),size = disk_num - nrow(vt_fail),replace = T),])
vt_norm_smp$group <- 'negative'

vt_smp <- rbind(vt_fail,vt_norm_smp)

save(vt_smp,file = file.path(dir_data,'bs_virtdisk.Rda'))
