#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: change_SMART.R
#
# Description: get the change of SMART attributes between the 2014-07-10 to 2014-07-30.
# The last day is 2014-07-30, we generate the change for 2, 5, 10, 15, 20 days
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-27 19:26:45
#
# Last   modified: 2017-11-27 19:26:46
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')


change_SMART <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataSMT,'smart5k',fn))
  
  smart <- format_smart(smart)
  smart$date <- as.Date(smart$time)
  DT <- factorX(subset(smart,date > dayS & date <= dayE,c('sn','time','date',col_smart)))
  DT$sndt <- paste(DT$sn,DT$date,sep='+')
  DT <- DT[!duplicated(DT$sndt),]
  DT <- subset(DT, date %in% seqDay)
  splitDT <- split(DT,DT$sn)
  
  r <- do.call(rbind,lapply(splitDT,function(df){
    if(!(seqDay[length(seqDay)] %in% df$date) | nrow(df)==1)return(NULL)
    df <- replace_value(df,v1=-1,v2=NA)
    rr <- do.call(rbind,lapply(seqDay[-length(seqDay)],function(d){
      if(d %in% df$date){
        return(df[df$date==d,col_smart]-df[df$date==dayE,col_smart])
      }else{
        return(rep(NA,15))
      }
    }))
    rr$sn <- fct2ori(df$sn[1])
    rr$interval <- seq(20,5,-5)
    rr
  }))
  row.names(r) <- NULL
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  
  return(r)
}

###### MAIN ######
dayS <- as.Date('2014-07-10');dayE <- as.Date('2014-07-31')
seqDay <- sort(seq.Date(dayE,dayS,by = -5))
fname <- list.files(file.path(dir_dataSMT,'smart5k'))
idx <- seq_len(length(fname))
r <- foreachX(idx,'change_SMART')
cng_smart <- do.call(rbind,r)
cng_smart <- cng_smart[,c('sn','interval',col_smart)]
cng_smart$sn <- factor(cng_smart$sn)
save(cng_smart,file = file.path(dir_data,'change_SMART.Rda'))
