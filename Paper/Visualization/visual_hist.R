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
fn <- fname[which(fname=='data108.Rda')]
flag_rs <- F
cat(sprintf('[%s]\t SATRT!!!\n',fn))
load(file.path(dir_dataset,fn))
io14$util <- with(io14,sum_util/count)

# S2. Prepare data ------------------------------------
DT <- format_bandwidth(DT)
DT <- factorX(subset(DT,!is.na(xps) & xps>0 & svrid %in% io14$svrid))
if(flag_rs == T){
  DT$utilX <- DT$util
  DT$utilX[DT$utilX==0] <- 1
  DT$si <- with(DT,xps/utilX)
  load(file.path(dir_data,'quantile_random_strength.Rda'))
  load(file.path(dir_data,'quantile_dutycycle.Rda'))
  load(file.path(dir_data,'quantile_bandwidth.Rda'))
  quan_random <- quan_random[,c('svrid','count','mean','sd','Q100')]
  quan_random <- gen_data(quan_random,expand = T)
}
a1 <- subset(io14,util<=5)
DT_truncate <- factorX(subset(DT,svrid %in% a1$svrid))
splitDT <- split(DT_truncate,DT_truncate$svrid)

# S3. Plot hist------------------------------------



# S4. Plot line------------------------------------
attr <- 'util'
prow <- 4;pcol <- 4;pmar <- 2
par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))

smp_ind <- data.frame(ind=sample(1:length(splitDT),prow*pcol))
smp_ind$svrid <- names(splitDT)[smp_ind$ind]
smp_ind$numD <- model_svrid$numD[match(smp_ind$svrid,model_svrid$svrid)]
smp_ind$adc <- io14$util[match(smp_ind$svrid,io14$svrid)]
smp_ind <- smp_ind[order(smp_ind$adc),]
for (i in seq_len(prow*pcol)) {
  df <- splitDT[[smp_ind$ind[i]]]
  uni.date <- sort(unique(as.Date(df$time)))
  start.date <- uni.date[round(length(uni.date)*0.2)]
  df <- subset(df,as.Date(time)>start.date & as.Date(time) <= start.date+2)
  title <- sprintf('%s[%s]\n[%.2f]',df$svrid[1],df$numD[1],mean(df$util))
  plot(df$time,df[[attr]],ylim=c(0,20),main=title,type='l')
}