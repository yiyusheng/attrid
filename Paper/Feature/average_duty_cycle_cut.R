#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_duty_cycle_cut.R
#
# Description: calculate the average duty cycle for partial data cut by the quantile of duty cycle.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-17 23:23:09
#
# Last   modified: 2017-08-17 23:23:10
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')

average_duty_cycle_cut<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  # cut_point <- seq(0,1,0.25)
  # cut_point <- seq(0,1,0.1)
  cut_point <- c(0,0.05,0.1,0.2,0.8,0.9,0.95,1)
  DT <- subsetX(DT,svrid %in% levels(DT$svrid)[1:100])
  r <- list2df(tapply(DT$util,DT$svrid,function(arr){
    quan5 <- quantile(arr,cut_point)
    # cat(sprintf('%d\n',length(unique(quan5))))
    sapply(2:length(quan5),function(i){
      p <- quan5[i-1];q <- quan5[i]
      if(i==2 | p==q){
        return(c(mean(arr[arr<=q]),mean(arr[arr>=p & arr<=q]),mean(arr[arr>=p])))
        # return(mean(arr[arr>=p & arr<=q])
      }else {
        return(c(mean(arr[arr<=q]),mean(arr[arr>p & arr<=q]),mean(arr[arr>=p])))
        # return(mean(arr[arr>p & arr<=q]))
      }
    })
  }))
  n <- paste('Q',cut_point[-length(cut_point)]*100,sep='')
  n <- paste(rep(n,each=3),rep(c('L','M','R'),3),sep='')
  names(r) <- c(n,'svrid')
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'average_duty_cycle_cut',frac_cores = 0.9)

r <- do.call(rbind,r)
r$svrid <- factor(r$svrid)
r <- replace_value(r)

save(r,file = file.path(dir_data,'average_duty_cycle_cut(0-5-10-20-80-90-95-100).Rda'))
