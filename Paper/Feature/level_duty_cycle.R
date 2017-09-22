#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: large_quantile_duty_cycle.R
#
# Description: to get the top rank duty cycle, we generate the adc of duty cycle large than a special quantile.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:07:44
#
# Last   modified: 2017-08-30 11:48:47
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')

adc_level_dutycycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  splitDT <- split(DT,DT$svrid)
  r <- lapplyX(splitDT,function(df){
    # sort_arr <- sort(df$util)
    # len <- length(sort_arr)
    # quan <- quantile(sort_arr,div)
    # sapply(quan,function(i)mean(sort_arr[which.min(sort_arr==i):len]))
    sapply(div,function(i)mean(df$util[df$util>=quantile(df$util,i)]))
  })
  r <- cbind(row.names(r),data.frame(r))
  names(r)[1] <- 'svrid'
  r$svrid <- fct2ori(r$svrid)
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
step <- 0.0001
div <- c(seq(0,0.98,0.01),seq(0.99,1,step))
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'adc_level_dutycycle',frac_cores = 0.9)

r <- do.call(rbind,r)
r$svrid <- factor(r$svrid)
names(r) <- c('svrid',paste('L',div/step,sep = ''))
level_dutycycle <- r
save(level_dutycycle,file = file.path(dir_data,'adc_level_dutycycle.Rda'))
