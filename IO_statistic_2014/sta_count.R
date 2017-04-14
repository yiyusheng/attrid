#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_count.R
#
# Description: count items and days for each file of IO workload.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-20 14:45:03
#
# Last   modified: 2017-03-20 14:45:05
#
#
#
rm(list = ls());source('~/rhead')

sta_count14 <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  DT$time <- factor(as.Date(DT$time))
  
  r_sta_day <- aggregate(DT[,3],by = list(DT$svrid,DT$time),function(x)length(x))
  names(r_sta_day) <- c('svrid','time','count')
  r_sta_day$fn <- fname[i]
  
  r_sta_svrid <- aggregate(DT[,3],by = list(DT$svrid),function(x)length(x))
  names(r_sta_svrid) <- c('svrid','count')
  r_sta_svrid$fn <- fname[i]  
  
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  list(r_sta_day,r_sta_svrid)
}


###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_count14',frac_cores = 0.3)
r_sta_day <- do.call(rbind,lapply(r,'[[',1))
r_sta_svrid <- do.call(rbind,lapply(r,'[[',2))
day_count <- tapply(r_sta_day$svrid,r_sta_day$svrid,length)
save(r_sta_day,r_sta_svrid, file = file.path(dir_data,'sta_count14.Rda'))

