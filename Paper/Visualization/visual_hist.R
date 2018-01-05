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
flag_rs <- T
cat(sprintf('[%s]\t SATRT!!!\n',fn))
load(file.path(dir_dataset,fn))

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
io14$util <- with(io14,sum_util/count)
a1 <- subset(io14,util>60 & util<80)
DT <- subset(DT,DT %in% a1$svrid)
splitDT <- split(DT,DT$svrid)

# S3. Plot ------------------------------------
attr <- 'util'
prow <- 4;pcol <- 4;pmar <- 2
par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))

smp_ind <- data.frame(ind=sample(1:length(splitDT),prow*pcol))
smp_ind$numD <- sapply(splitDT[smp_ind$ind],function(df)df$numD[1])
smp_ind <- smp_ind[order(smp_ind$numD),]
for (i in smp_ind$ind) {
  df <- splitDT[[i]]
  arr <- df[[attr]]
  arr <- arr[arr < quantile(arr,0.95)]
  # title <- sprintf('%s[%d]\n[%.2f][%.1f]',df$svrid[1],df$numD[1],sum(df$util==0)/nrow(df),mean(df$si))
  title <- sprintf('%s[%d]\n[%.2f]',df$svrid[1],df$numD[1],mean(df$util))
  if(length(arr)==0)arr<-rep(0,1000)
  if(df$svrid[1] %in% f201409$svrid){
    hist(arr, breaks = 100,main = title,color = 'red')
  }else{
    hist(arr, breaks = 100,main = title)
  }
}