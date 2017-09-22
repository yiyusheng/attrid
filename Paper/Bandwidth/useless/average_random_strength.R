#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_random_strength.R
#
# Description: 
# We present the distribution of the strength of request random and correlate it and the failure rate
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 16:32:08
#
# Last   modified: 2017-08-29 21:58:23
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

gen_result_random_strength <- function(i,attr_max){
  attr_level <- paste(col_name[i],'level',sep='_')
  cat(sprintf('[%s]\t %s SATRT!!!\n',date(),attr_level))
  quan_random[[attr_level]] <- trunc_level(quan_random,col_name[i],attr_max,20)
  
  list[data_fr,p_fr,p_count,p_countf,object_data] <- gen_fr(quan_random,attr_level,prt=F)
  corr <- cor(data_fr[,1],data_fr$AFR)
  cat(sprintf('[%s]\t %d corr:%.4f\tEND!!!\n',date(),i,corr))
  
  return(list(corr,data_fr,p_fr,p_count))
}

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))
col_name <- c('mean','Q100')

r_mean <- gen_result_random_strength(1,1)
r_Q100 <- gen_result_random_strength(2,2)
list[data_fr,p_fr,p_count] <- r_mean
list[data_fr,p_fr,p_count] <- r_Q100
