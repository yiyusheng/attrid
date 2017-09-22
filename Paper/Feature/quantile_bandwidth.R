#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: quantile_bandwidth.R
#
# Description: generate duty cycle quantile from 0% to 100% binned by 1% for each disks
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:07:44
#
# Last   modified: 2017-08-30 20:08:03
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')


quantile_bandwidth<- function(i,vt){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,truncate=F)
  splitDT <- split(DT,DT$svrid)
  
  r1 <- lapplyX(splitDT,function(df)c(mean(df$rps_trunc),sd(df$rps_trunc),quantileX(df$rps_trunc)))
  r1 <- cbind(row.names(r1),data.frame(r1))
  names(r1)[1] <- 'svrid'
  r1$svrid <- fct2ori(r1$svrid)
 
  r2 <- lapplyX(splitDT,function(df)c(mean(df$wps_trunc),sd(df$wps_trunc),quantileX(df$wps_trunc)))
  r2 <- cbind(row.names(r2),data.frame(r2))
  names(r2)[1] <- 'svrid'
  r2$svrid <- fct2ori(r2$svrid)
  
  r3 <- lapplyX(splitDT,function(df)c(mean(df$xps_trunc),sd(df$xps_trunc),quantileX(df$xps_trunc)))
  r3 <- cbind(row.names(r3),data.frame(r3))
  names(r3)[1] <- 'svrid'
  r3$svrid <- fct2ori(r3$svrid)
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(list(r1,r2,r3))
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'quantile_bandwidth',frac_cores = 0.9,vt=value_trunc)

r1 <- do.call(rbind,lapply(r,'[[',1));r1$svrid <- factor(r1$svrid);names(r1) <- c('svrid','mean','sd',paste('Q',0:100,sep = ''))
r2 <- do.call(rbind,lapply(r,'[[',2));r2$svrid <- factor(r2$svrid);names(r2) <- c('svrid','mean','sd',paste('Q',0:100,sep = ''))
r3 <- do.call(rbind,lapply(r,'[[',3));r3$svrid <- factor(r3$svrid);names(r3) <- c('svrid','mean','sd',paste('Q',0:100,sep = ''))

quan_rps <- r1
quan_wps <- r2
quan_xps <- r3

save(quan_rps,quan_wps,quan_xps,file = file.path(dir_data,'quantile_bandwidth.Rda'))
# save(quan_rps,quan_wps,quan_xps,file = file.path(dir_data,sprintf('quantile_bandwidth_truncate_%iabw.Rda',time_trunc)))
