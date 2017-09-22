#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: quantile_ratio.R
#
# Description: generate quantile of the ratio of rps and wps
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:07:44
#
# Last   modified: 2017-09-01 10:43:54
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

quantile_ratio <- function(i,vt){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100,truncate=F)
  DT <- factorX(subset(DT,wps_trunc!=0))
  
  splitDT <- split(DT,DT$svrid)
  
  r <- lapplyX(splitDT,function(df){
    arr <- with(df,wps_trunc/xps_trunc)
    c(mean(arr),sd(arr),quantileX(arr),mean(1/arr),sd(1/arr),quantileX(1/arr))
  })
  r <- cbind(row.names(r),data.frame(r))
  names(r)[1] <- 'svrid'
  r$svrid <- fct2ori(r$svrid)
 
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))

r <- foreachX(idx,'quantile_ratio',frac_cores = 0.9,vt=value_trunc)
quan_ratio <- do.call(rbind,r)
quan_ratio$svrid <- factor(quan_ratio$svrid)
names(quan_ratio) <- c('svrid','mean','sd',paste('Q',0:100,sep = ''),'mean_rec','sd_rec',paste('Q_rec',0:100,sep = ''))

save(quan_ratio,file = file.path(dir_data,'quantile_ratio.Rda'))
