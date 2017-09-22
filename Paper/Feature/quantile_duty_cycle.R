#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: quantile_duty_cycle.R
#
# Description: generate duty cycle quantile from 0% to 100% binned by 1% for each disks
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:07:44
#
# Last   modified: 2017-08-11 11:07:45
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

quantile_dutycycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  splitDT <- split(DT,DT$svrid)
  r <- lapplyX(splitDT,function(df)c(mean(df$util),sd(df$util),quantileX(df$util)))
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
r <- foreachX(idx,'quantile_dutycycle',frac_cores = 0.9)

r <- do.call(rbind,r)
r$svrid <- factor(r$svrid)
names(r) <- c('svrid','mean','sd',paste('Q',0:100,sep = ''))
quantile_dutycycle <- r
save(quantile_dutycycle,file = file.path(dir_data,'quantile_dutycycle.Rda'))
