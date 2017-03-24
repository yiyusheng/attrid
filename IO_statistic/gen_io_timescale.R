#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_io_timescale.R
#
# Description: generate mean value of io on different time scale(day/week/month)
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-02-13 11:04:22
#
#
#

rm(list = ls())
source('head.R')
dir_datatendcast <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcast/'
fname <- setdiff(list.files(dir_datatendcast),'d1test.Rda')

# dir_dataten <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid/'
# fname <- setdiff(fname,list.files(file.path(dir_data,'timescale_dd')))

timescale_dd <- function(i){
  load(file.path(dir_datatendcast,fname[i]))
  dd <- iops_dcast_clear(dt_dcast)
  
  # days
  dd_day <- dd
  dd_day$time <- round.POSIXt(dd_day$time)
  dd_day <- aggregate(dd_day[grepl('ps|util|iops',names(dd_day))],by = list(dd_day$svrid,dd_day$time),
                      function(x)round(mean(x,na.rm = T),digits = 4))
  names(dd_day) <- names(dd)
  
  # weeks
  cut_weeks <- seq.POSIXt(as.p('2014-07-01'),as.p('2015-07-01'),by = 'weeks')
  cut_weeks[length(cut_weeks)] <- as.p('2015-07-01')
  dd_week <- dd
  dd_week$time <- cut.POSIXt(dd_week$time,cut_weeks)
  dd_week <- aggregate(dd_week[grepl('ps|util|iops',names(dd_week))],by = list(dd_week$svrid,dd_week$time),
                       function(x)round(mean(x,na.rm = T),digits = 4))
  names(dd_week) <- names(dd)
  
  # months
  cut_months <- seq.POSIXt(as.p('2014-07-01'),as.p('2015-07-01'),by = 'months')
  dd_month <- dd
  dd_month$time <- cut.POSIXt(dd_month$time,cut_months)
  dd_month <- aggregate(dd_month[grepl('ps|util|iops',names(dd_month))],by = list(dd_month$svrid,dd_month$time),
                       function(x)round(mean(x,na.rm = T),digits = 4))
  names(dd_month) <- names(dd)
  
  cat(sprintf('%s\n',fname[i]))
  # save(dd_day,dd_week,dd_month,file = file.path(dir_data,'timescale_dd',fname[i]))
  return(list(d1 = dd_day,d2 = dd_week,d3 = dd_month))
}

require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(30,length(idx)),type = 'FORK',outfile = file.path(dir_data,'out_git'))
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% timescale_dd(i)
stopCluster(ck)
save(r,file = file.path(dir_data,'io_timescale.Rda'))

###### LOAD MONTH DATA ######
fname <- list.files(file.path(dir_data,'timescale_dd'))
dd_month_all <- lapply(seq_len(length(fname)),function(i){
  load(file.path(dir_data,'timescale_dd',fname[i]))
  iops_aggragate(dd_month)
})
data_month_dcast <- do.call(rbind,dd_month_all)
save(data_month_dcast,file = file.path(dir_data,'data_month_dcast.Rda'))
