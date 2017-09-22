#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: visual_hist.R
#
# Description: visualize histgraph of disk drives for duty cycle/read bandwidth/write bandwidth
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-16 22:06:18
#
# Last   modified: 2017-08-16 22:06:19
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

# S1. Load data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
fn <- fname[10]
cat(sprintf('[%s]\t SATRT!!!\n',fn))
load(file.path(dir_dataset,fn))

# S2. Prepare data ------------------------------------
DT <- format_bandwidth(DT)
DT <- factorX(subset(DT,!is.na(xps) & xps>0 & svrid %in% io14$svrid))
DT$rs <- with(DT,util/xps)
splitDT <- split(DT,DT$svrid)

# S3. Plot ------------------------------------
attr <- 'rs'
prow <- 4;pcol <- 4;pmar <- 2
par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))

smp_ind <- data.frame(ind=sample(1:length(splitDT),prow*pcol))
smp_ind$numD <- sapply(splitDT[smp_ind$ind],function(df)df$numD[1])
smp_ind <- smp_ind[order(smp_ind$numD),]
for (i in smp_ind$numD) {
  arr <- splitDT[[i]][[attr]][splitDT[[i]][[attr]]!=0]
  title <- sprintf('%s[%d]',df$svrid[1],df$numD[1])
  if(length(arr)==0)arr<-rep(0,1000)
  hist(arr, breaks = 100,main = title,ylim = c(0,5e3))
}
