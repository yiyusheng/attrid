#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: frac0.R
#
# Description: failure rate producted by fraction of duty cycle equals to zero.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-08 15:13:46
#
# Last   modified: 2017-11-08 15:13:48
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle_frac0.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
attr_main <- 'frac0rev'
quantile_dutycycle$frac0rev <- 1-quantile_dutycycle$frac0
DT_quan <- gen_data(quantile_dutycycle[,c('svrid',attr_main)],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# S2. analysis ------------------------------------
list[data_fr,p_fr,p_count,corr,data_adc] <- gen_result_feature(DT_quan,attr_main,1)
