#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: bandwidth.R
#
# Description: Duty Cycle
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
DT_quan <- gen_data(quantile_dutycycle[,c('svrid',attr_main)],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# S2. analysis ------------------------------------
# gen_failure
list[data_fr,p_fr,p_count,corr,data_adc] <- gen_result_feature(DT_quan,'mean',100)

# # 20% vs 80%#
# df_perc <- data.frame(value = sort(DT_quan$mean))
# df_perc$cumsum <- cumsum(df_perc$value)
# which.min(abs(df_perc$cumsum-0.1*sum(df_perc$value)))/nrow(df_perc)#
# 1-df_perc$cumsum[round(0.8*nrow(df_perc))]/sum(df_perc$value)#
# rank_value(DT_quan$mean,6)#
# 
# # 60% dividing line#
# sum(data_fr$fCount[data_fr$mean_level>60])/sum(data_fr$count[data_fr$mean_level>60])*600
# sum(data_fr$fCount[data_fr$mean_level<=60])/sum(data_fr$count[data_fr$mean_level<=60])*600

# S3. Plot ------------------------------------
p_adc_fr <- p_fr+xlab('The Average Duty Cycle(%)')+ylim(c(0,25))

p_adc_dist <- p_count+xlab('The Average Duty Cycle(%)')+coord_cartesian(ylim=c(0,10))+
  annotate("text", x=20, y=10,label= '67.30%',size=6)+
  geom_segment(aes(x = 12, y = 10, xend = 8, yend = 10.1), size=0.2,arrow = arrow(length = unit(0.2, "cm")))
p_adc_abw <- plot_relationship(subset(data_adc,numD=='1'),'mean_level','abw')+xlab('The Average Duty Cycle(%)')+ylab('The Average Bandwidth(kB/s)')

save_fig(p_adc_fr,'adc_fr')
save_fig(p_adc_dist,'adc_dist')
save_fig(p_adc_abw,'adc_abw')

save(p_adc_fr,p_adc_dist,p_adc_abw,file=file.path(dir_data,'Paper','Rda','p_adc.Rda'))
# load(file.path(dir_data,'Paper','Rda','p_adc.Rda'))
