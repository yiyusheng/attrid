#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_base.R
#
# Description: statistic basic information for each server
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 17:03:40
#
# Last   modified: 2017-07-07 17:30:41
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')
require('lubridate')


sta_basic <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  # DT1 <- DT;
  # DT <- smp_df(DT1,10000)
  DT$time <- as.Date(DT$time)
  
  # sum per day
  sta_day <- aggregate(DT[,c('rps','wps','util')],by = list(DT$svrid,DT$time),sum)
  tmp_count <- aggregate(DT$util,by = list(DT$svrid,DT$time),length)
  sta_day <- merge(sta_day,tmp_count)
  names(sta_day) <- c('svrid','date','sum_rps','sum_wps','sum_util','count')
  
  # sum per server
  sta_svrid <- aggregate(sta_day[,c('sum_rps','sum_wps','sum_util')],by = list(sta_day$svrid),sum)
  tmp_count <- aggregate(sta_day$count,by = list(sta_day$svrid),sum)
  sta_svrid <- merge(sta_svrid,tmp_count)
  names(sta_svrid) <- c('svrid','sum_rps','sum_wps','sum_util','count')
  
  # sum per month
  sta_day$month <- floor_date(sta_day$date,'month')
  sta_month <- aggregate(sta_day[,c('sum_rps','sum_wps','sum_util')],by = list(sta_day$svrid,sta_day$month),sum)
  tmp_count <- aggregate(sta_day$count,by = list(sta_day$svrid,sta_day$month),sum)
  sta_month <- merge(sta_month,tmp_count)
  names(sta_month) <- c('svrid','month','sum_rps','sum_wps','sum_util','count')
  
  # sd 
  sd_svrid <- aggregate(DT[,c('rps','wps','util')],by = list(DT$svrid),sd)
  names(sd_svrid) <- c('svrid','sd_rps','sd_wps','sd_util')
  
  sta_day$fn <- fname[i];sta_svrid$fn <- fname[i];sta_month$fn <- fname[i];sd_svrid$fn <- fname[i]
  cat(sprintf('[%s]\t END!!!\n',fn))
  list(sta_day,sta_svrid,sta_month,sd_svrid)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_basic',frac_cores = 0.8)

sta_day <- do.call(rbind,lapply(r,'[[',1))
sta_svrid <- do.call(rbind,lapply(r,'[[',2))
sta_month <- do.call(rbind,lapply(r,'[[',3))
sd_svrid <- do.call(rbind,lapply(r,'[[',4))

save(sta_day,sta_svrid,sta_month,sd_svrid,file = file.path(dir_data,'sta_base14.Rda'))


# 
# 
# 
# rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')
# 
# sta_basic <- function(i){
#   fn <- fname[i]
#   cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
#   load(file.path(dir_dataset,fn))
#   
#   r_sta_svrid <- data.frame(svrid = levels(DT$svrid),
#                             count = as.numeric(tapply(DT$svrid,DT$svrid,length)),
#                             sum_rps = as.numeric(tapply(DT$rps,DT$svrid,sum)),
#                             sum_wps = as.numeric(tapply(DT$wps,DT$svrid,sum)),
#                             sum_util = as.numeric(tapply(DT$util,DT$svrid,sum)),
#                             fn = fname[i])
#   
#   
#   cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
#   r_sta_svrid
# }
# 
# ###### STA:MAIN ######
# dir_dataset <- dir_data14DC
# fname <- list.files(dir_dataset)
# idx <- seq_len(length(fname))
# r <- foreachX(idx,'sta_basic',frac_cores = 0.8)
# 
# r_sta_svrid <- do.call(rbind,r)
# save(r_sta_svrid,file = file.path(dir_data,'sta_base14.Rda'))

#ggplot(r_sta_svrid) + stat_ecdf(aes(x = log2(sum_wps))) + stat_ecdf(aes(x = log2(sum_rps)))  + stat_ecdf(aes(x = log2(xps)))
