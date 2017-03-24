#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: iops.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-16 15:56:46
#
# Last   modified: 2016-12-16 15:56:48
#
#
#
rm(list = ls())
source('head.R')
source('iopsFunc.R')
dir_iops <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid'
fname <- list.files(dir_iops)
# fname <- 'd1.Rda'

sta_iops <- function(fn){
  load(file.path(dir_iops,fn))
  dt <- subset(data,value > 0)
  names(dt)[names(dt) == 'svr_id'] <- 'svrid'
  dt$svrid <- factor(dt$svrid)
  dt$attrid <- factor(dt$attrid)
  
  # length of records for each server
  sta_time <- list2df(tapply(dt$date,dt$svrid,function(x)list(min(x),max(x))),n = c('start','end','svrid'))
  sta_time$start <- as.POSIXct(as.character(sta_time$start),tz = 'UTC',format = '%Y%m%d')
  sta_time$end <- as.POSIXct(as.character(sta_time$end),tz = 'UTC',format = '%Y%m%d')
  sta_time$period <- as.numeric(difftime(sta_time$end,sta_time$start,units = 'days'))
  sta_time <- sta_time[,c('svrid',setdiff(names(sta_time),'svrid'))]
  
  # total amount of requests in the period for each disk
  sta_amount <- data.frame(tapply(dt$value,list(dt$svrid,dt$attrid),function(x)sum(as.numeric(x))))
  sta_amount[is.na(sta_amount)] <- 0
  sta_amount$svrid <- levels(dt$svrid)
  row.names(sta_amount) <- NULL
  sta_amount <- sta_amount[,c('svrid',setdiff(names(sta_amount),'svrid'))]
  
  # maximum of ratio of each disk's iops in the total amount of iops 
  numDevide <- 5
  sta_max_ratio <- data.frame(svrid = sta_amount$svrid,
                              read_max_ratio = class_max_fraction_iops(sta_amount,paste('X',36810:36833,sep=''),numDevide),
                              write_max_ratio = class_max_fraction_iops(sta_amount,paste('X',36834:36857,sep=''),numDevide))
  
  # merge and return
  sta_max_ratio$svrid <- fct2ori(sta_max_ratio$svrid)
  sta_time_ratio <- merge(sta_time,sta_max_ratio,by = 'svrid')
  return(list(sa = sta_amount,str = sta_time_ratio))
  
}

require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_iops')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% {
  sta_iops(fname[i])
}
stopCluster(ck)

# sta_amount <- do.call(rbind,lapply(r,'[[','sa'))
sta_time_ratio <- do.call(rbind,lapply(r,'[[','str'))
save(r,sta_time_ratio,file = file.path(dir_data,'iops.Rda'))
