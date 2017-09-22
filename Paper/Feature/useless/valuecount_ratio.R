#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: valuecount_ratio.R
#
# Description: count ratio of wps to xps
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-21 22:07:12
#
# Last   modified: 2017-09-01 10:43:43
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

count_ratio_value<- function(i,vt,bins){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100,truncate=F)
  DT <- factorX(subset(DT,wps_trunc!=0))
  
  DT$ratio <- with(DT,wps_trunc/xps_trunc)
  DT$ratio_level <- trunc_level(DT,'ratio',1,100)
  DT$ratio_rec <- with(DT,xps_trunc/wps_trunc)
  DT$ratio_rec_level <- trunc_level(DT,'ratio_rec',5,100)
  
  # splitDT <- split(DT,DT$svrid)
  # DT <- subset(DT,svrid %in% levels(DT$svrid)[1:100])
  
  r <- table(DT$svrid,DT$ratio_level)
  r <- cbind(row.names(r),as.data.frame.matrix(r))
  names(r)[1] <- 'svrid'
  
  r_rec <- table(DT$svrid,DT$ratio_level)
  r_rec <- cbind(row.names(r_rec),as.data.frame.matrix(r_rec))
  names(r_rec)[1] <- 'svrid'
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(list(r,r_rec))
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))

r <- foreachX(idx,'count_ratio_value',frac_cores = 0.9,vt=value_trunc,bins=100)

format_list <- function(i,full_names){
  lapply(lapply(r,'[[',i),function(df){
    lack_names <- setdiff(full_names,names(df))
    if(length(lack_names)!=0){
      for(i in 1:length(lack_names)){
        df[[lack_names[i]]] <- 0
      }
    }
    df <- df[,c('svrid',full_names)]
    return(df)
  })
}

r1 <- format_list(1,as.character(seq(0,1,0.01)))
ratio_value_count <- do.call(rbind,r1)
names(ratio_value_count)[2:102] <- paste('V',names(ratio_value_count)[2:102],sep='')

r2 <- format_list(2,as.character(seq(1,5,4/100)))
ratio_rec_value_count <- do.call(rbind,r2)
names(ratio_rec_value_count)[2:102] <- paste('V',names(ratio_rec_value_count)[2:102],sep='')

save(ratio_value_count,ratio_rec_value_count,file = file.path(dir_data,'count_ratio_value.Rda'))
