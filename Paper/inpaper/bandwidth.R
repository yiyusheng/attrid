#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: bandwidth.R
#
# Description: bandwidth
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-01 11:52:59
#
# Last   modified: 2017-09-01 11:53:00
#
#
#

rm(list = ls());source('~/rhead');setwd(file.path(dir_c,'Disk_Workload/Paper'));
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_ratio.Rda'))

attr_main <- 'mean'
DT_quan_all <- gen_data(quan_xps[,c('svrid',attr_main)],expand=T)
DT_quan <- subset(DT_quan_all,numD==12) # update 2018-01-04

# S2. Failure rate ------------------------------------
list[data_fr,p_fr,p_count,data_corr,data_abw]<- gen_result_feature(DT_quan,attr_main,9000,bins=20)
p <- plot_result_feature_addmodel(DT_quan,attr_main,9000)+xlab('ABW');print(p)


# S_end. plot ------------------------------------
p_abw_fr <- p_fr+xlab('ABW (KB/s)')+ylim(c(0,6))+theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))

p_abw_dist <- p_count+xlab('ABW (KB/s)')+coord_cartesian(ylim=c(0,15))+
  annotate("text", x=2300, y=14,label= sprintf('%.2f%%',data_fr$percentage[which.min(data_fr$mean_level)]),size=10)+
  geom_segment(aes(x = 1500, y = 14, xend = 800, yend = 14), size=0.2,arrow = arrow(length = unit(0.2, "cm"))) + ylab('Percentage (%)')

save_fig(p_abw_fr,'abw_fr')
save_fig(p_abw_dist,'abw_dist')
save(p_abw_fr,p_abw_dist,p_abw_adc,file=file.path(dir_data,'Paper','Rda','p_abw.Rda'))
