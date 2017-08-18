#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: visual_hist.R
#
# Description: visualize histgraph of disk drives for duty cycle/read bandwidth/write bandwidth
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-16 22:06:18
#
# Last   modified: 2017-08-16 22:06:19
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')

visual_hist<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  splitDT <- split(DT,DT$svrid)
  attr <- 'util'
  
  par(mfrow = c(5,5),mar=c(1,1,1,1))
  for (i in sample(1:length(splitDT),25)) {
    hist(splitDT[[i]][[attr]], breaks = 100,main = names(splitDT)[i])
  }
  
  shape_levelcat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'visual_hist',frac_cores = 0.9)
save(r,file = file.path(dir_data,'visual_hist'))
