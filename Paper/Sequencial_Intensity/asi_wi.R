#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: asi_wi.R
#
# Description: ASI and the workload intensity (adc and abw). We test disk drives with given ASI and find their failure rate of adc/abw.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-14 09:45:48
#
# Last   modified: 2017-11-14 09:47:02
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1.data prepare ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

DT_raw <- quan_random[,c('svrid','count','mean','sd')]
DT_raw$adc <- quantile_dutycycle$mean[match(DT_raw$svrid,quantile_dutycycle$svrid)]
DT_raw$abw <- quan_xps$mean[match(DT_raw$svrid,quan_xps$svrid)]
DT_raw <- binning_data(DT_raw,'mean',400,T,4)
DT_epd <- gen_data(DT_raw,expand=T)


# S2. gen failure rate
cut_value <- 100
DT_givenASIA <- subset(DT_epd,mean < cut_value)
DT_givenASIB <- subset(DT_epd,mean >= cut_value)

list[data_fr1,p_fr1,p_count,corr,DT_adc] <- gen_result_feature(DT_givenASIA,'adc',50)
list[data_fr2,p_fr2,p_count,corr,DT_abw] <- gen_result_feature(DT_givenASIA,'abw',3000)
list[data_fr3,p_fr3,p_count,corr] <- gen_result_feature(DT_givenASIB,'adc',50)
list[data_fr4,p_fr4,p_count,corr] <- gen_result_feature(DT_givenASIB,'abw',6000)

multiplot(plotlist = list(p_fr1+ylim(c(0,15)),p_fr2+ylim(c(0,15)),
                          p_fr3+ylim(c(0,15)),p_fr4+ylim(c(0,15))),
          cols = 2)

# S3. asi for abw/adc
ggplot(smp_df(DT_abw,2e3),aes(x=factor(abw_level)))+geom_boxplot(aes(y = mean),outlier.shape = NA) + geom_jitter(aes(y = mean),width = 0.2,alpha = 0.1)
