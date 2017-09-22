#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: ratio_rps_wps.R
#
# Description: Ratio of rps and wps
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-22 20:20:59
#
# Last   modified: 2017-08-22 20:21:00
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_ratio.Rda'))
attr_main <- 'mean'
DT_quan <- quan_ratio[,c('svrid',attr_main)]
DT_quan <- gen_data(DT_quan,expand=T)
DT_quan$mean <- DT_quan$mean*100
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# S2. Failure rate ------------------------------------
list[data_fr,p_fr,p_count,data_corr,data_ratio]<- gen_result_feature(DT_quan,attr_main,100)


# S_end. plot ------------------------------------
p_ratio_fr <- p_fr+xlab('The Ratio(%)')+ylim(c(0,6))
p_ratio_dist <- p_count+xlab('The Ratio(%)')+coord_cartesian(ylim=c(0,35))
p_ratio_abw <- plot_relationship(data_ratio,'mean_level','abw')+xlab('The Ratio(%)')+ylab('The Average Bandwidth(kB/s)')

save_fig(p_ratio_fr,'ratio_fr')
save_fig(p_ratio_dist,'ratio_dist')
save_fig(p_ratio_abw,'ratio_abw')

save(p_ratio_fr,p_ratio_dist,p_ratio_abw,file=file.path(dir_data,'Paper','Rda','p_ratio.Rda'))