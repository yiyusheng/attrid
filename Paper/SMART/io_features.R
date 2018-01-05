#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: all_features.R
#
# Description: merge all features together for each servers/disks
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-28 09:13:09
#
# Last   modified: 2017-11-28 09:13:11
#
#
#

# S1. Load function and data ------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'adc_level_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_ratio.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

# S2. Merge data ------------------------------------
io_features <- level_dutycycle[,c('svrid','L9900','L9950','L10000')]
io_features$adc <- quantile_dutycycle$mean[match(io_features$svrid,quantile_dutycycle$svrid)]
io_features$abw <- quan_xps$mean[match(io_features$svrid,quan_xps$svrid)]
io_features$ratio <- quan_ratio$mean[match(io_features$svrid,quan_ratio$svrid)]
io_features$asi <- quan_random$mean[match(io_features$svrid,quan_random$svrid)]

io_features <- subsetX(io_features,svrid %in% cmdbSMP$svrid)
nrc_col <- names(io_features)[sapply(io_features,class)=='numeric']
io_features[,nrc_col] <- apply(io_features[,nrc_col],2,roundX)

save(io_features,file=file.path(dir_data,'io_features.Rda'))
