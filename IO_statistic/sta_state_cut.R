#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_state_cut.R
#
# Description: cut attributes in to three statas referring low, median and high in order to abstract data and analysize the trend
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-09 15:30:02
#
# Last   modified: 2017-03-09 15:30:03
#
#
#

rm(list = ls());source('head.R');library(plyr)

load(file.path(dir_data,'permonth_201608_dcast.Rda'));rm(r_sum,r_sd);r_mean_month <- r_mean

r_mean_day$date <- as.p(r_mean_day$date)
r_mean_month$date <- as.p(r_mean_month$date)

# apply(r_mean_month[,-c(1,2)],2,function(x)quantile(x[x != 0],seq(0,1,1/3),na.rm = T))
# xps_cut <- unique(c(-1,quantile(r_mean_month$xps,seq(0,1,1/num_cut),na.rm = T),1e7))
# iops_cut <- unique(c(-1,quantile(r_mean_month$iops,seq(0,1,1/num_cut),na.rm = T),1e7))
# util_cut <- unique(c(-1,quantile(r_mean_month$util,seq(0,1,1/num_cut),na.rm = T),101))
xps_cut <- c(-1,0,100,3000,1e7)
iops_cut <- c(-1,0,1000,12000,1e7)
util_cut <- c(-1,0,1,6,101)

r_mean_month$xps_level <- cut(r_mean_month$xps,xps_cut,seq_len(length(xps_cut)-1),right = T)
r_mean_month$util_level <- cut(r_mean_month$util,util_cut,seq_len(length(util_cut)-1),right = T)
r_mean_month$iops_level <- cut(r_mean_month$iops,iops_cut,seq_len(length(iops_cut)-1),right = T)

# data_month_state <- r_mean_month[,grepl('svrid|time|level',names(r_mean_month))]
data_month_state <- r_mean_month
save(data_month_state,file = file.path(dir_data,'data_month_state.Rda'))
