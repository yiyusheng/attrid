#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: length_large_value.R
#
# Description: length of continuous value greater/less than a threshold.
# For each object, we use a list to save the value of length
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-10 09:23:33
#
# Last   modified: 2017-11-10 09:23:34
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))


length_value_type<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100,truncate = F)
  DT <- factorX(subset(DT,wps_trunc!=0))
  DT$utilX <- DT$util
  DT$utilX[DT$utilX==0] <- 1
  DT$si <- with(DT,xps/utilX)
  splitDT <- split(DT,DT$svrid)
  
  rr <- lapply(thresholds,function(thred){
    r <- lapply(splitDT,function(df){
      arr <- df[[attr]]
      if(grt == 1){
        arr_level <- rle(as.numeric(arr > thred))
      }else{
        arr_level <- rle(as.numeric(arr <= thred))
      }
      list_pos <- arr_level$lengths[arr_level$values==1]
      list_neg <- arr_level$lengths[arr_level$values==0]
      return(list(list_pos,list_neg))
    })
    
    # The commont code is used to locate the position of each value last from 1 to length of last value
    #   arr_level$levels <- arr_level$values
    #   arr_level$levels[arr_level$levels == 1] <- seq_len(length(arr_level$levels[arr_level$levels == 1]))
    #   arr.level <- rep(arr_level$levels,times=arr_level$lengths)
    #   splitDF <- split(arr,arr.level)
    #   splitDF[['0']] <- NULL
    #   if(length(splitDF)==0){
    #     return(rep(0,12))
    #   }else{
    #     len_set <- sapply(splitDF,length)
    #     mean_set <- sapply(splitDF,mean)
    #     return(as.numeric(c(length(arr),sum(len_set),length(len_set),sum(len_set)/length(len_set),
    #                         unlist(summary(len_set))[c(2,3,5,6)],unlist(summary(mean_set))[c(2,3,5,6)])))
    #   }
    # })
    # r <- list2df(r,n = c('count','len_fit','times_fit','mean_len','lenQ1','lenQ2','lenQ3','lenQ4','meanQ1','meanQ2','meanQ3','meanQ4','svrid'))
  })

  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(rr)
}

# STA:MAIN ----
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))

# For si
# attr <- 'si'
# thresholds <- c(50,100,150,200,400,800)
# grt <- 0 # 1 for greater than and 0 for less than
# rr <- foreachX(idx,'length_value_type',frac_cores = 0.9)
# llv50 <- do.call(rbind,lapply(rr,'[[',1))
# llv100 <- do.call(rbind,lapply(rr,'[[',2))
# llv150 <- do.call(rbind,lapply(rr,'[[',3))
# llv200 <- do.call(rbind,lapply(rr,'[[',4))
# llv400 <- do.call(rbind,lapply(rr,'[[',5))
# llv800 <- do.call(rbind,lapply(rr,'[[',6))
# save(llv50,llv100,llv150,llv200,llv400,llv800,file = file.path(dir_data,'length_value_type.Rda'))

# For duty cycle
attr <- 'util'
thresholds <- seq(10,90,10)
grt <- 1 
r <- foreachX(idx,'length_value_type',frac_cores = 0.9)
length_last_dutycycle <- lapply(thresholds,function(x)do.call(c,lapply(r,'[[',which(thresholds==x))))
save(length_last_dutycycle,file = file.path(dir_data,'length_last_dutycycle.Rda'))