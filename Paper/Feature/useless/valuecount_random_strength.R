#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: valuecount_random_strength.R
#
# Description: random strength value count to evalute the strength of random request
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-21 22:07:12
#
# Last   modified: 2017-08-30 20:08:46
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

count_random_strength_value<- function(i,vt,bins){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100)
  DT <- factorX(subset(DT,wps_trunc!=0))
  DT$random_strength <- with(DT,util/xps)
  DT$random_strength[DT$random_strength>1] <- 1
  DT$random_strength_level <- ceiling(DT$random_strength*100)/100
  
  # splitDT <- split(DT,DT$svrid)
  # DT <- subset(DT,svrid %in% levels(DT$svrid)[1:100])
  
  r <- table(DT$svrid,DT$random_strength_level)
  r <- cbind(row.names(r),as.data.frame.matrix(r))
  names(r)[1] <- 'svrid'
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))

r <- foreachX(idx,'count_random_strength_value',frac_cores = 0.9,vt=value_trunc,bins=100)

full_names <- as.character(seq(0,1,0.01))
r1 <- lapply(r,function(df){
  lack_names <- setdiff(full_names,names(df))
  if(length(lack_names)!=0){
    for(i in 1:length(lack_names)){
      df[[lack_names[i]]] <- 0
      
    }
  }
  df <- df[,c('svrid',full_names)]
  return(df)
})

random_strength_value_count <- do.call(rbind,r1)
names(random_strength_value_count)[2:102] <- paste('V',names(random_strength_value_count)[2:102],sep='')

save(random_strength_value_count,file = file.path(dir_data,'count_random_strength_value.Rda'))
