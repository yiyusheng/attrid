#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: quantile_random_strength.R
#
# Description: generate the strength of random request by duty cycle divided by the bandwidth
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:07:44
#
# Last   modified: 2017-08-29 17:46:57
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

quantile_random_strength <- function(i,vt){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100,truncate = F)
  DT <- factorX(subset(DT,wps_trunc!=0))
  
  splitDT <- split(DT,DT$svrid)
  
  r <- lapplyX(splitDT,function(df){
    arr <- with(subset(df,util>0),xps/util)
    if(length(arr)==0){
      return(c(rep(0,1004)))
    }else{
      return(c(length(arr),mean(arr),sd(arr),quantile(arr,seq(0,1,0.001))))
    }
  })
  r <- cbind(row.names(r),data.frame(r))
  names(r)[1] <- 'svrid'
  r$svrid <- fct2ori(r$svrid)
 
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'quantile_random_strength',frac_cores = 0.9,vt=value_trunc)
quan_random <- do.call(rbind,r)
quan_random$svrid <- factor(quan_random$svrid)
names(quan_random) <- c('svrid','count','mean','sd',paste('Q',0:1000,sep = ''))
save(quan_random,file = file.path(dir_data,'quantile_random_strength.Rda'))
