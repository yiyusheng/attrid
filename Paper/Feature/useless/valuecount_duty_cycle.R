#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: valuecount_duty_cycle.R
#
# Description: count duty cycle value for each disk 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-21 22:07:12
#
# Last   modified: 2017-08-30 11:48:30
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')

count_duty_cycle_value<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  # splitDT <- split(DT,DT$svrid)
  # DT <- subset(DT,svrid %in% levels(DT$svrid)[1:100])
  
  r <- tapply(DT$util,DT$svrid,table)
  
  r <- table(DT$svrid,DT$util)
  r <- cbind(row.names(r),as.data.frame.matrix(r))
  names(r)[1] <- 'svrid'
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'count_duty_cycle_value',frac_cores = 0.9)
r <- do.call(rbind,r[-which(sapply(r,length)!=102)])
r$svrid <- fct2ori(r$svrid)
names(r)[2:102] <- paste('V',0:100,sep='')
duty_cycle_value_count <- r
save(duty_cycle_value_count,file = file.path(dir_data,'count_duty_cycle_value.Rda'))
