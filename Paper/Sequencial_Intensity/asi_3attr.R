#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: asi_3attr.R
#
# Description: find the relationship between asi and adc/abw/frac0
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-08 16:00:55
#
# Last   modified: 2017-11-08 16:00:57
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data -----
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle_frac0.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

DT_raw <- quan_random
am <- 400

DT_quan <- gen_data(DT_raw[,c('svrid','mean')],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$frac0 <- quantile_dutycycle$frac0[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]
DT_quan <- binning_data(DT_quan,'mean',am,T,20)

# S2. Plot
data.plot <- subset(DT_quan,frac0 < 10)
p_asi_adc <- plot_relationship(data.plot,'mean_level','adc')+xlab('ASI')+ylab('ADC(%)')
p_asi_abw <- plot_relationship(data.plot,'mean_level','abw')+xlab('ASI')+ylab('ABW(KB/s)')
p_asi_frac0 <- plot_relationship(data.plot,'mean_level','frac0')+xlab('ASI')+ylab('frac0(%)')
multiplot(plotlist = list(p_asi_adc,p_asi_abw,p_asi_frac0),cols = 3)
