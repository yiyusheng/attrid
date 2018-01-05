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
attr_main <- c('cv','mean')
quantile_dutycycle$cv <- with(quantile_dutycycle,sd/mean)
DT_quan <- gen_data(quantile_dutycycle[,c('svrid',attr_main)],expand=T)
DT_quan <- subset(DT_quan,numD==12) #add 2018-01-04

# S2. analysis ------------------------------------
# gen_failure
list[data_fr,p_fr,p_count,corr,data_adc] <- gen_result_feature(DT_quan,'mean',100)
list[data_fr_cv,p_fr1,p_count_cv,corr_cv,data_adc_cv] <- gen_result_feature(subset(DT_quan,is.numeric(cv) & cv > 0.04),'cv',1)


# failure rate reduction
# data_adc_dedup <- factorX(data_adc[!duplicated(data_adc$svrid_old),])
# table_rate <- setNames(as.data.frame.matrix(table(data_adc_dedup$mean_level,data_adc_dedup$numD)),c('n1','n12'))
# table_rate$rate <- with(table_rate,n12/(n1+n12))

# S3. Plot ------------------------------------
p_adc_fr <- p_fr+xlab('ADC (%)')+ylim(c(0,25))+scale_fill_manual(values=c('grey60','grey20'))+ ylab('Failure Rate (%)')

p_fr_cv <- p_fr1+xlab('Coefficient of Variation')+scale_fill_manual(values=c('grey60','grey20'))+ ylab('Failure Rate (%)')


p_adc_dist <- p_count+xlab('ADC (%)')+coord_cartesian(ylim=c(0,10))+
  annotate("text", x=22, y=10,label= '67.30%',size=10)+
  geom_segment(aes(x = 12, y = 10, xend = 8, yend = 10), size=0.2,arrow = arrow(length = unit(0.2, "cm"))) + ylab('Percentage (%)')

# p_adc_abw <- plot_relationship(subset(data_adc,numD=='1'),'mean_level','abw')+xlab('ADC (%)')+ylab('ABW (KB/s)')
p_adc_abw <- plot_relationship(subset(data_adc,1==1),'mean_level','abw')+xlab('ADC (%)')+ylab('ABW (KB/s)')


save_fig(p_adc_fr,'adc_fr')
save_fig(p_adc_dist,'adc_dist')
save_fig(p_adc_abw,'adc_abw')
save_fig(p_fr_cv,'cvdc_fr')

save(p_adc_fr,p_adc_dist,p_adc_abw,p_fr_cv,file=file.path(dir_data,'Paper','Rda','p_adc.Rda'))
# load(file.path(dir_data,'Paper','Rda','p_adc.Rda'))
