#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_dcastClear.R
#
# Description: 
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
rm(list = ls());source('~/rhead');require(plyr)

sta_dc <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  # data <- factorX(subset(dt_dcast,svrid %in% levels(dt_dcast$svrid)[1:10]))
  data <- dt_dcast
  data$time <- factor(as.Date(data$time))

  tmp1 <- aggregate(data[,3],by = list(data$svrid,data$time),function(x)length(x))
  tmp2 <- aggregate(data[,attrName],by = list(data$svrid,data$time),function(x)sum(x == 0,na.rm = T))
  r_sta_day <- cbind(tmp2,tmp1[,3])
  names(r_sta_day) <- c('svrid','time',paste('zero_',attrName,sep=''),'count')
  r_sta_day$fn <- fname[i]
  
  r_sta_svrid <- ddply(r_sta_day,.(svrid),function(df)data.frame(dayCount = length(unique(df$time)),count = sum(df$count,na.rm = T),
                                                                 zero_util = sum(df$zero_util,na.rm = T ),zero_rps = sum(df$zero_rps,na.rm = T),zero_iopsr = sum(df$zero_iopsr,na.rm = T),
                                                                 zero_wps = sum(df$zero_wps,na.rm = T), zero_iopsw = sum(df$zero_iopsw,na.rm = T),fn = fname[i]))
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  list(r_sta_day,r_sta_svrid)
}


###### STA:MAIN ######
fname <- list.files(dir_datatendcastClear)
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_dc')
r_sta_day <- do.call(rbind,lapply(r,'[[',1))
r_sta_svrid <- do.call(rbind,lapply(r,'[[',2))
save(r_sta_day,r_sta_svrid, file = file.path(dir_data,'sta_dcastClear.Rda'))

###### ANALYSIS ######
load(file.path(dir_data,'sta_dcastClear.Rda'))
quan_wps <- cbind(quantileX(r_sta_svrid$zero_wps),quantileX(r_sta_day$zero_wps),
                  quantileX(r_sta_svrid$zero_iopsw),quantileX(r_sta_day$zero_iopsw))

invalid_wps <- factorX(subset(r_sta_svrid,zero_wps > 0,c('svrid','dayCount','count','fn')))
invalid_iopsw <- factorX(subset(r_sta_svrid,zero_iopsw > 0, c('svrid','dayCount','count','fn')))

save(invalid_wps,invalid_iopsw, file = file.path(dir_data,'sta_dcastClear_result.Rda'))
