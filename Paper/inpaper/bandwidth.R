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

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
attr_main <- 'mean'
DT_quan <- gen_data(quan_xps[,c('svrid',attr_main)],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# S2. Failure rate ------------------------------------
list[data_fr,p_fr,p_count,data_corr,data_abw]<- gen_result_feature(DT_quan,attr_main,9000)

# S_end. plot ------------------------------------
p_abw_fr <- p_fr+xlab('ABW (KB/s)')+ylim(c(0,6))+theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))+
  scale_fill_manual(values=c('grey60','grey20'))+ ylab('Failure Rate (%)')

p_abw_dist <- p_count+xlab('ABW (KB/s)')+coord_cartesian(ylim=c(0,15))+
  annotate("text", x=2300, y=14,label= '62.32%',size=10)+
  geom_segment(aes(x = 1500, y = 14, xend = 800, yend = 14), size=0.2,arrow = arrow(length = unit(0.2, "cm"))) + ylab('Percentage (%)')

# p_abw_adc <- plot_relationship(subset(data_abw,numD==12),'mean_level','adc')+ylab('Average Duty Cycle(%)')+xlab('Average Bandwidth(KB/s)')

p_abw_adc <- plot_relationship(subset(data_abw,1==1),'mean_level','adc')+ylab('ADC (%)')+xlab('ABW (KB/s)')

save_fig(p_abw_fr,'abw_fr')
save_fig(p_abw_dist,'abw_dist')
save_fig(p_abw_adc,'abw_adc')

save(p_abw_fr,p_abw_dist,p_abw_adc,file=file.path(dir_data,'Paper','Rda','p_abw.Rda'))
