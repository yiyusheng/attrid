#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: fraction_100_duty_cycle.R
#
# Description: calculate the fraction of the 100 duty cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-17 23:23:09
#
# Last   modified: 2017-08-23 21:35:46
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')

fraction_100_duty_cycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  # DT <- factorX(subset(DT,svrid %in% levels(DT$svrid)[sample(1:length(levels(DT$svrid)),100)]))
  r <- data.frame(svrid = levels(DT$svrid),
                  frac100 = as.numeric(tapply(DT$util,DT$svrid,function(arr)sum(arr==100)/length(arr))))
  r$svrid <- fct2ori(r$svrid)
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'fraction_100_duty_cycle',frac_cores = 0.9)
r <- do.call(rbind,r)
r$svrid <- factor(r$svrid)
save(r,file = file.path(dir_data,'fraction_100_duty_cycle.Rda'))
