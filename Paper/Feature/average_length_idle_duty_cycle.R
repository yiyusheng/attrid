#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_length_idle_duty_cycle.R
#
# Description: generate the length of idle length and working length of duty cycle for each disk drives.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-10 17:22:15
#
# Last   modified: 2017-08-10 17:22:17
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')
require('lubridate')

idle_thred <- function(i,splitDT){
  r <- lapply(splitDT,function(df){
    df$level <- as.numeric(df$util > i)
    r <- rle(df$level)
    r <- data.frame(len = r$lengths,value = r$values)
    r <- tapply(r$len,r$value,function(v){
      list(length(v),roundX(mean(v)))
    })
    
    if(length(r)==2){
      r1 <- data.frame(svrid = df$svrid[1],
                       idle_count = r['0'][[1]][[1]],idle_len = r['0'][[1]][[2]],idle_mean = roundX(mean(df$util[df$level==0])),
                       busy_count= r['1'][[1]][[1]],busy_len = r['1'][[1]][[2]],busy_mean = roundX(mean(df$util[df$level==1])))
    }else if(names(r)=='0'){
      r1 <- data.frame(svrid = df$svrid[1],
                       idle_count = r['0'][[1]][[1]],idle_len = r['0'][[1]][[2]],idle_mean = roundX(mean(df$util[df$level==0])),
                       busy_count= 0,busy_len = 0,busy_mean = 0)
    }else if(names(r)=='1'){
      r1 <- data.frame(svrid = df$svrid[1],
                       idle_count = 0,idle_len = 0,idle_mean = 0,
                       busy_count= r['1'][[1]][[1]],busy_len = r['1'][[1]][[2]],busy_mean = roundX(mean(df$util[df$level==1])))
    }
  })
  r <- do.call(rbind,r)
  r$thred <- i
  return(r)
}

length_continue_dutycycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  splitDT <- split(DT,DT$svrid)
  
  r <- lapply(0:20,idle_thred,splitDT)
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'length_continue_dutycycle',frac_cores = 0.9)
r <- lapply(1:21,function(i)do.call(rbind,lapply(r,'[[',i)))
save(r,file = file.path(dir_data,'average_legnth_idle_duty_cycle.Rda'))
