#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: extreme_bandwidth.R
#
# Description: We extract the maximum bandwidth for each disk drive.
# Then we plot the distribution and failure rate of the maximum bandwidth.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-25 10:43:35
#
# Last   modified: 2017-08-28 20:03:30
#
#
#

# S1. Load function and data ------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_bandwidth_truncate_10000abw.Rda'))

# S2. Extract the maximum duty cycle
max_bandwidth_profiling <- function(r){
  attr <- 'Q100'
  object_data <- r[,c('svrid',attr)]
  itv <- 450
  object_data$extreme <- ceiling(object_data[[attr]]/itv)*itv 
  object_data$extreme[object_data$extreme>18000] <- 18000
  list[data_fr,p_fr,p_count] <- gen_fr(object_data,'extreme',prt=F,countLimit = 100)
  p1 <- p_count+xlab('extreme duty cycle(%)')
  p2 <- p_fr + xlab('extreme duty cycle(%)')+ geom_smooth(aes(y=AFR),method='lm',color='red',linetype=2)
  corr <- with(data_fr,cor(extreme,AFR))
  print(corr)
  summary(glm(AFR~extreme,data = data_fr,family = 'gaussian'))
}

