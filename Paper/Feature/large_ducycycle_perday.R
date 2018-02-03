#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: large_ducycycle_perday.R
#
# Description: generate the fraction of large duty cycle (greater than a threshold) for disk per day.
#
# Copyright (c) 2018, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2018-01-11 11:34:42
#
# Last   modified: 2018-01-11 11:34:43
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))

large_ducytcycle_perday <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100,truncate = F)
  DT <- factorX(subset(DT,wps_trunc!=0))
  DT$date <- as.Date(DT$time)
  splitDT <- split(DT,DT$svrid)
  rrr <- lapply(splitDT,function(df){
    rr <- tapply(df$util,df$date,function(arr){
        r <- sapply(thresholds,function(t){
          sum(arr>t)
        })
        r/length(arr)
    })
  })
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(rrr)
}

# MAIN ------
attr <- 'util'
thresholds <- seq(10,90,10)
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'large_ducytcycle_perday',frac_cores = 0.9)
large_ducytcycle_perday <- do.call(c,r)
save(large_ducytcycle_perday,file = file.path(dir_data,'large_ducytcycle_perday.Rda'))
