#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: smart_RSC_RER.R
#
# Description: extract features of RSC and RER
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-12-05 10:40:03
#
# Last   modified: 2017-12-05 10:40:06
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

# F1. extract feature
smart_RSC_RER <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  DT <- subset(smart,time > dayS & time <= dayE,c('sn','time',col_smart[c(1,4)]))
  
  # Mean
  r_mean <- aggregate(DT[,col_smart[c(1,4)]],list(DT$sn),mean)
  
  # For RSC
  DT_RSC <- factorX(subset(DT,sn %in% cng_smart$sn[cng_smart$Reallocated_Sector_Ct_Raw != 0 & !is.na(cng_smart$Reallocated_Sector_Ct_Raw)]))
  DT_RSC <- DT_RSC[order(DT_RSC$time),]
  splitRSC <- split(DT_RSC,DT_RSC$sn)
  r_rsc <- lapply(splitRSC,function(df){
    df$diff <- c(0,diff(df$Reallocated_Sector_Ct_Raw))
    c1 <- length(df$diff[df$diff!=0]) # count in our data
    c2 <- c1/(as.numeric(df$time[nrow(df)]) - as.numeric(df$time[1]))*86400 # mean count for one day
    if(c1>1){
      pointT <- as.numeric(df$time[df$diff != 0])
      c3 <- mean(diff(pointT)/86400) # interval between increment
    }else{
      c3 <- -1
    }
    c4 <- mean(df$diff[df$diff != 0]) # mean increment
    list(c1,c2,c3,c4)
  })
  r_rsc <- list2df(r_rsc,n = c('count','count_day','itv','avg_inc','sn'))
  
  
  # For RER
  DT_RER <- factorX(subset(DT,sn %in% cng_smart$sn[cng_smart$Raw_Read_Error_Rate_Value != 0 & !is.na(cng_smart$Raw_Read_Error_Rate_Value)]))
  DT_RER <- DT_RER[order(DT_RER$time),]
  splitRER <- split(DT_RER,DT_RER$sn)
  r_ber <- lapply(splitRER,function(df){
    df$diff <- c(0,diff(df$Raw_Read_Error_Rate_Value))
    df$tag <- as.numeric(df$diff <= mean(df$diff)-3*sd(df$diff))
    c1 <- length(df$tag[df$tag==1])
    if(c1 > 1){
      pointT <- as.numeric(df$time[df$tag == 1])
      c2 <- mean(diff(pointT)/86400) # interval/periodicity
      c3 <- mean(df$diff[df$tag==1]) # mean reduction
    }else{
      c2 <- -1
      c3 <- -1
    }
    list(c1,c2,c3)
  })
  r_ber <- list2df(r_ber,n = c('count','itv','avg_rdc','sn'))
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(list(r_rsc,r_ber,r_mean))
}

# S1. Load basic data ------------------------------------
dayS <- as.POSIXct('2014-07-10');dayE <- as.POSIXct('2014-07-31')
dir_dataset <- file.path(dir_dataSMT,'smart5k')
fname <- list.files(dir_dataset)
load(file.path(dir_data,'change_SMART.Rda'))

# S2. computation ------
idx <- seq_len(length(fname))
r <- foreachX(idx,'smart_RSC_RER',frac_cores = 0.9)
r_rsc <- roundDF(do.call(rbind,lapply(r,'[[',1)))
r_rer <- roundDF(do.call(rbind,lapply(r,'[[',2)))
r_mean <- do.call(rbind,lapply(r,'[[',3))
names(r_mean)[1] <- 'sn'

save(r_rsc,r_rer,r_mean,file = file.path(dir_data,'smart_RSC_RER.Rda'))

