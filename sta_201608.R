#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_201608.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-02-13 11:04:22
#
#
#
rm(list = ls())
source('head.R')
source('iops_clear.R')
library(plyr)

dir_datafile <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid'
fname <- list.files(dir_datafile)
coln_mapper <- data.frame(a = c('svr_id','date',paste('X',c(902,903,999,36810:36857),sep='')),
                          b = c('svrid','date','rps','wps','util',paste('iopsr',1:24,sep=''),paste('iopsw',1:24,sep='')))

day_aggre <- function(df,aggr){
  df <- subset(df,!is.na(df$value))
  r <- tapply(df$value,df$attrid,function(x)data.frame(mean = mean(x),sd = sd(x),
                                            sum = sum(x),count = sum(!is.na(x))))
  return(data.frame(lapply(r,'[[',aggr)))
}

col_sort <- function(df){
  names(df) <- coln_mapper$b[match(names(df),coln_mapper$a)]
  df[grepl('ps|util|iops',names(df))] <- round(df[grepl('ps|util|iops',names(df))],digits = 4)
  order_col <- setdiff(coln_mapper$b,setdiff(coln_mapper$b,names(df)))
  df <- df[,order_col]
}

require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',output = 'out_sta')
registerDoParallel(ck)

r <- foreach(i = idx,.verbose = T) %dopar% {
  load(file.path(dir_datafile,fname[i]))
  data$svr_id <- factor(data$svr_id)
  data <- iops_melt_clear(data)
  
  cat(sprintf('[%s]%s:r_sta\n',date(),fname[i]))
  r_sta <- ddply(data,.(svr_id,date),function(df)data.frame(Count = length(df$attrid),numAttrid = length(unique(df$attrid)),fn = fname[i]))
  cat(sprintf('[%s]%s:r_mean\n',date(),fname[i]))
  r_mean <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'mean'))
  cat(sprintf('[%s]%s:r_sd\n',date(),fname[i]))
  r_sd <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'sd'))
  cat(sprintf('[%s]%s:r_sum\n',date(),fname[i]))
  r_sum <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'sum'))
  cat(sprintf('[%s]%s:r_count\n',date(),fname[i]))
  r_count <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'count'))
  
  save(list(r_sta,r_mean,r_sd,r_sum,r_count),file.path(dir_data,'sta_201608',fname[i]))
  # return(list(r_sta,r_mean,r_sd,r_sum,r_count))
}
stopCluster(ck)




# sta_day <- do.call(rbind,r)
# sta_svrid <- by(sta_day,sta_day$svr_id,function(df){
#   data.frame(svr_id = fct2ori(df$svr_id[1]),
#              min_date = min(df$date),
#              max_date = max(df$date),
#              count = sum(df$Count),
#              count_day = length(unique(df$date)),
#              fn = fct2ori(df$fn[1]))
# })
# sta_svrid <- do.call(rbind,sta_svrid)
# save(r,file = file.path(dir_data,'sta_201608A.Rda'))

