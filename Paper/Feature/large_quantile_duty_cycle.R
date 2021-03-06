#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: large_quantile_duty_cycle.R
#
# Description: detailed duty cycle
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

large_quantile_dutycycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  splitDT <- split(DT,DT$svrid)
  r <- lapplyX(splitDT,function(df)quantile(df$util,div))
  r <- cbind(row.names(r),data.frame(r))
  names(r)[1] <- 'svrid'
  r$svrid <- fct2ori(r$svrid)
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
step <- 0.0001
div <- seq(0,1,step)
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'large_quantile_dutycycle',frac_cores = 0.9)

r <- do.call(rbind,r)
r$svrid <- factor(r$svrid)
names(r) <- c('svrid',paste('Q',0:(1/step),sep = ''))
quan_largedc <- r
save(quan_largedc,file = file.path(dir_data,'large_quantile_dutycycle.Rda'))
