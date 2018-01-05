#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: diff_duty_cycle.R
#
# Description: generate the sum of diff to evaluate the vibration
#
# Copyright (c) 2018, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2018-01-04 17:23:20
#
# Last   modified: 2018-01-04 17:23:21
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

diff_dutycycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  # DT$util[DT$util==0] <- 1
  splitDT <- split(DT,DT$svrid)
  r <- lapplyX(splitDT,function(df){
    arr <- df$util
    arr.diff <- diff(arr)
    arr.diff.neg <- arr.diff[arr.diff<0]
    arr.diff.pos <- arr.diff[arr.diff>0]
    c(sum(abs(arr.diff))/length(arr.diff),
      sum(arr.diff.neg)/length(arr.diff.neg),
      sum(arr.diff.pos)/length(arr.diff.pos))
  })
  r <- cbind(row.names(r),data.frame(r))
  names(r)[1] <- 'svrid'
  r$svrid <- fct2ori(r$svrid)
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'diff_dutycycle',frac_cores = 0.9)
diff_duty_cycle <- do.call(rbind,r)
names(diff_duty_cycle) <- c('svrid','diff','diff_neg','diff_pos')
diff_duty_cycle$svrid <- factor(diff_duty_cycle$svrid)
save(diff_duty_cycle,file = file.path(dir_data,'diff_dutycycle.Rda'))
