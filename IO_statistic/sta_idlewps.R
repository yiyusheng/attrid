#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_idlewps.R
#
# Description: We find idle wps(16-64kB/s written) occupied more than 23% server whose 80% value of wps falls in this range
# Thus, we statsistic value of other 4 attributes for these 23% servers to find some insights.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-24 08:58:34
#
# Last   modified: 2017-03-24 08:58:36
#
#
#

rm(list = ls());source('~/rhead')

sta_idlewps <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  dt_dcast$sizer <- dt_dcast$rps/dt_dcast$iopsr;dt_dcast$sizew <- dt_dcast$wps/dt_dcast$iopsw
  dd <- filter_badiops_NA(dt_dcast,attrName)
  dd$time <- as.Date(dd$time)
  dd <- factorX(subset(dd,svrid %in% wps_idle$svrid & wps >= 0 & wps < 128))
  # dd <- factorX(subset(dd,svrid %in% levels(dd$svrid)[1:2]))
  
  r1 <- aggregate(dd[,attrName],by = list(dd$svrid,dd$time),function(x)summary(x)[1:6])
  r2 <- aggregate(dd[,'util'],by = list(dd$svrid,dd$time),length)
  r <- cbind(r2[,c(1:3)],r1$util,r1$rps,r1$iopsr,r1$sizer,r1$wps,r1$iopsw,r1$sizew)
  sumName <- c('min','Q1','Q2','mean','Q3','max')
  names(r) <- c('svrid','time','count',paste(rep(attrName,each = length(sumName)),rep(sumName,length(attrName)),sep = '_'))
  r
}

###### MAIN ######
load(file.path(dir_data,'sc_wps_idle.Rda'))
wps_idle <- factorX(wps_idle[,c('svrid','count','bts_wtn','maxN','frac_idle','fn','fclass')])

fname <- list.files(dir_datatendcastClear)
fname <- fname[!grepl('a\\d.*',fname)]
attrName <- c('util','rps','iopsr','sizer','wps','iopsw','sizew')
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_idlewps')
sta_idlewps <- do.call(rbind,r)
save(sta_idlewps,file = file.path(dir_data,'sta_idlewps_size_0-128.Rda'))

###### ANALYSIS Of IDEL INDICATED BY WPS######
load(file.path(dir_data,'sta_dcastClear.Rda'))
load(file.path(dir_data,'sta_idlewps_16-64.Rda'))

sta_wps <- sta_idlewps[,grepl(c('svrid|time|count|wps|iopsw'),names(sta_idlewps))]
ggplot(subset(smp_df(sta_wps,1e5),iopsw_mean < 2000 & wps_mean > 16 & wps_mean < 64),aes(x = wps_mean,y = iopsw_mean)) + geom_point(alpha = 0.01)
sta_wps$fn <- r_sta_svrid$fn[match(sta_wps$svrid,r_sta_svrid$svrid)]

# count of values in the idle range. the peak in the middle results from some in-out type of wps (value in the range preceded by a value out of range)
count_wps1 <- melt(table(sta_wps$count))
ggplot(count_wps1,aes(x = Var1)) + geom_line(aes(y = log2(value)),color = 'red')
sid_count <- melt(table(factor(sta_wps$svrid[sta_wps$count >=142 & sta_wps$count < 146])));names(sid_count) <- c('svrid','count')
sid_select <- subsetX(sta_wps,svrid %in% c('4398','4420','4429','4430'))

# trend of wps in the range
sta_wps_svrid <- list2df(tapply(sta_wps$count,sta_wps$svrid,mean),n = c('mean_count','svrid'))
quan_count <- quantileX(sta_wps_svrid$mean_count)
ggplot(sta_wps_svrid,aes(mean_count)) + geom_histogram(binwidth = 4,fill = cbPalette[4])

wps_range90 <- subsetX(sta_wps,svrid %in% sta_wps_svrid$svrid[sta_wps_svrid > 288 *0.9] & fn == 'd9.Rda')
wps_range50 <- subsetX(sta_wps,svrid %in% sta_wps_svrid$svrid[sta_wps_svrid > 288 *0.45 & sta_wps_svrid < 288 *0.55] & fn == 'd9.Rda')
wps_range10 <- subsetX(sta_wps,svrid %in% sta_wps_svrid$svrid[sta_wps_svrid < 288 *0.1] & fn == 'd9.Rda')
save(wps_range10,wps_range50,wps_range90,file = file.path(dir_data,'wps_range_d9.Rda'))
