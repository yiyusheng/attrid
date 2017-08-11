#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_base.R
#
# Description: generate sum and count of each attributes for each server on each day.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-18 11:23:54
#
# Last   modified: 2017-07-18 11:23:55
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic/');source('~/rhead')
require('lubridate')


sta_basic <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))

  # DT1 <- DT;
  # DT <- smp_df(DT1,10000)
  DT$time <- as.Date(DT$time)
  
  # per day
  sta_day <- aggregate(DT[,c('rps','wps','util')],by = list(DT$svrid,DT$time),sum)
  tmp_count <- aggregate(DT$util,by = list(DT$svrid,DT$time),count)
  sta_day <- merge(sta_day,tmp_count)
  names(sta_day) <- c('svrnum','date','sum_rps','sum_wps','sum_util','count')
  
  # per server
  sta_svrid <- aggregate(sta_day[,c('sum_rps','sum_wps','sum_util')],by = list(sta_day$svrnum),sum)
  tmp_count <- aggregate(sta_day$count,by = list(sta_day$svrnum),sum)
  sta_svrid <- merge(sta_svrid,tmp_count)
  names(sta_svrid) <- c('svrnum','sum_rps','sum_wps','sum_util','count')
  
  # per month
  sta_day$month <- floor_date(sta_day$date,'month')
  sta_month <- aggregate(sta_day[,c('sum_rps','sum_wps','sum_util')],by = list(sta_day$svrnum,sta_day$month),sum)
  tmp_count <- aggregate(sta_day$count,by = list(sta_day$svrnum,sta_day$month),sum)
  sta_month <- merge(sta_month,tmp_count)
  names(sta_month) <- c('svrnum','month','sum_rps','sum_wps','sum_util','count')
  
  sta_day$fn <- fname[i];sta_svrid$fn <- fname[i];sta_month$fn <- fname[i]
  cat(sprintf('[%s]\t END!!!\n',fn))
  list(sta_day,sta_svrid,sta_month)
}

###### STA:MAIN ######
dir_dataset <- dir_data15ADC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_basic',frac_cores = 0.8)

sta_day <- do.call(rbind,lapply(r,'[[',1))
sta_svrid <- do.call(rbind,lapply(r,'[[',2))
sta_month <- do.call(rbind,lapply(r,'[[',3))

save(sta_day,sta_svrid,sta_month,file = file.path(dir_data,'sta_base15.Rda'))
