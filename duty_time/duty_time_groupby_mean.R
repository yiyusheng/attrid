#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: duty_time_group.R
#
# Description: we use util to represent duty time and tag each util as real (real workload) or basic (basic workload).
# then we statistic count of util and sum of util for real and basic for each servers.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-12 10:44:32
#
# Last   modified: 2017-04-12 10:44:34
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')

duty_time_groupby_mean <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  
  DT_mean <- data.frame(svrid = levels(DT$svrid),
                        utilM = as.numeric(tapply(DT$util,DT$svrid,mean)),
                        utilS = as.numeric(tapply(DT$util,DT$svrid,sd)),
                        rpsM = as.numeric(tapply(DT$rps,DT$svrid,mean)),
                        wpsM = as.numeric(tapply(DT$wps,DT$svrid,mean)),
                        count = as.numeric(tapply(DT$util,DT$svrid,length)))
  DT_mean$utilCV <- with(DT_mean,utilS/utilM)
  DT_mean <- DT_mean[,c('svrid','count','utilM','utilS','utilCV','rpsM','wpsM')]
  DT_mean$fn <- fn
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  return(DT_mean)
}

###### MAIN1: Generate feature for svrid ######
dir_dataset <- dir_data14DC
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)))

fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'duty_time_groupby_mean')
DT_mean <- do.call(rbind,r)
save(DT_mean,file = file.path(dir_data,'duty_time_groupby_mean.Rda'))
