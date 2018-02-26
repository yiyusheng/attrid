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

rm(list = ls());source('~/rhead');setwd(file.path(dir_c,'Disk_Workload/Paper'));
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
attr_main <- c('cv','mean','Q90')
quantile_dutycycle$cv <- with(quantile_dutycycle,sd/mean)
DT_quan <- gen_data(quantile_dutycycle[,c('svrid',attr_main)],expand=T)
DT_quan <- subset(DT_quan,numD==12) #add 2018-01-04

# S2. analysis ------------------------------------
# gen_failure
list[data_fr,p_fr,p_count,corr,data_adc] <- gen_result_feature(DT_quan,'mean',100)
list[data_fr_cv,p_fr1,p_count_cv,corr_cv,data_adc_cv] <- gen_result_feature(subset(DT_quan,is.numeric(cv) & cv > 0.04),'cv',1)

# S3. Plot ------------------------------------
p_adc_fr <- p_fr+xlab('ADC (%)')+ylim(c(0,25))

p_fr_cv <- p_fr1+xlab('CV')


p_adc_dist <- p_count+xlab('ADC (%)')+coord_cartesian(ylim=c(0,10))+
  annotate("text", x=22, y=10,label= paste(max(data_fr$percentage),'%',sep=''),size=10)+
  geom_segment(aes(x = 12, y = 10, xend = 8, yend = 10), size=0.2,arrow = arrow(length = unit(0.2, "cm"))) + ylab('Percentage (%)')


save_fig(p_adc_fr,'adc_fr')
save_fig(p_adc_dist,'adc_dist')
save_fig(p_fr_cv,'adc_cv_fr')

save(p_adc_fr,p_adc_dist,p_fr_cv,file=file.path(dir_data,'Paper','Rda','p_adc.Rda'))
