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
load(file.path(dir_data,'uniform_data.Rda'))

# S1. ratio distribution and failure rate ------------------------------------------------------------------------
col_bw <- c('rps','wps','xps')
itv <- 1
attr_bw <- data.frame(attr = col_bw,maxlimit = c(4000,5000,8000))
object_data <- add_average_bandwidth(io14, attr_bw, itv=1000)
object_data$ratio_wps_level <- with(object_data,ceiling(sum_wps/sum_xps*100/itv)*itv)
object_data <- replace_value(object_data)
list[data_fr,p_fr,p_count,p2,object_data,p3] <- gen_fr(object_data,attr='ratio_wps_level',prt = F,countLimit = 100)
p_fr <- p_fr+annotate("text", x=25, y=8, size = 8,label= sprintf('corr:%f',roundX(cor(data_fr[,1],data_fr$AFR))))
p_count
# S2. correlation between ratio and AMD ------------------------------------------------------------------------
itv <- 5
object_data$ratio_wps_level <- with(object_data,ceiling(sum_wps/sum_xps*100/itv)*itv)
object_data <- replace_value(object_data)
p_amd_ratio <- plot_amd_diff(object_data,'ratio_wps_level')
multiplot(plotlist = p_amd_ratio[2:4],cols=3)
