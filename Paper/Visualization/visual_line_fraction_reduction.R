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
flag_rs <- F
fname <- list.files(dir_dataset)
io14$util <- with(io14,sum_util/count)

load(file.path(dir_data,'fraction_reduction_svrid.Rda'))
svrid.t80_frac60 <- c(fct2ori(t80_frac70$svrid_old),fct2ori(t80_frac90$svrid_old))

load_allfile <- function(i){
  load(file.path(dir_dataset,fname[i]))
  DT <- subset(DT,svrid %in% svrid.t80_frac60)
  DT$svrid <- fct2ori(DT$svrid)
  DT
}
idx <- seq_len(length(fname))
r <- foreachX(idx,'load_allfile',outname = -1)
r <- do.call(rbind,r)
r$svrid <- factor(r$svrid)

# S2. Prepare data ------------------------------------
DT <- format_bandwidth(r)
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
DT.frac70 <- factorX(subset(DT,svrid %in% t80_frac70$svrid_old))
DT.frac90 <- factorX(subset(DT,svrid %in% t80_frac90$svrid_old))

splitDT.frac70 <- split(DT.frac70,DT.frac70$svrid)
splitDT.frac90 <- split(DT.frac90,DT.frac90$svrid)
splitDT <- splitDT.frac90



# S3. Plot ------------------------------------
attr <- 'util'
prow <- 4;pcol <- 4;pmar <- 2
par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))

smp_ind <- data.frame(ind=sample(1:length(splitDT),prow*pcol))
smp_ind$svrid <- names(splitDT)[smp_ind$ind]
smp_ind$numD <- sapply(splitDT[smp_ind$ind],function(df)df$numD[1])
smp_ind$adc <- io14$util[match(smp_ind$svrid,io14$svrid)]
smp_ind <- smp_ind[order(smp_ind$adc),]
for (i in smp_ind$ind) {
  df <- splitDT[[i]]
  uni.date <- sort(unique(as.Date(df$time)))
  start.date <- uni.date[round(length(uni.date)*0.2)]
  df <- subset(df,as.Date(time)>start.date & as.Date(time) <= start.date+10)
  title <- sprintf('%s[%d]\n[%.2f]',df$svrid[1],df$numD[1],mean(df$util))
  plot(df$time,df[[attr]],ylim=c(0,105),main=title)
}
