#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: read_kernel_stats.R
#
# Description: read the vmstat and diskstats to find difference between pgpgin/out of vmstat and xps of diskstats
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-20 10:40:18
#
# Last   modified: 2017-03-20 10:40:20
#
#
#

rm(list = ls());source('head.R')

read_stats <- function(){
  vmstat <- read.table('/proc/vmstat')[41:42,2]
  diskstats <- as.numeric(read.table('/proc/diskstats')[1,c(6,10)])
  return(list(vmstat[1],vmstat[2],diskstats[1],diskstats[2]))
}

df <- data.frame(vr = 0,vw = 0,dr = 0,dw = 0)
while(1){
  df <- rbind(df,read_stats())
  len <- dim(df)[1]
  dif_sta <- as.numeric(df[len,] - df[(len-1),])
  cat(sprintf('[%s]\tvr:%i\tdr:%i\tvw:%i\tdw:%i\n',date(),dif_sta[1]*2,dif_sta[3],dif_sta[2]*2,dif_sta[4]))
  Sys.sleep(10)
}


