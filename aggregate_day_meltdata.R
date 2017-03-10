#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: aggregate_day_meltdata.R
#
# Description: statistic data of 201608 to aggreate sum,count,sd,mean for each day
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
library(plyr)

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

del_col <- function(df){
  r <- apply(df,2,function(x)sum(is.na(x)))/nrow(df)
  df[,names(r)[r > 0.999]] <- NULL
  df
}

gen_sta <- function(i){
  load(file.path(dir_datafile,fname[i]))
  data$svr_id <- factor(data$svr_id)
  data <- iops_melt_clear(data)
  
  r <- list()
  cat(sprintf('[%s]%s:r_sta\n',date(),fname[i]))
  r[['sta']] <- ddply(data,.(svr_id,date),function(df)data.frame(Count = length(df$attrid),numAttrid = length(unique(df$attrid)),fn = fname[i]))
  cat(sprintf('[%s]%s:r_mean\n',date(),fname[i]))
  r[['mean']] <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'mean'))
  cat(sprintf('[%s]%s:r_sd\n',date(),fname[i]))
  r[['sd']] <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'sd'))
  cat(sprintf('[%s]%s:r_sum\n',date(),fname[i]))
  r[['sum']] <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'sum'))
  cat(sprintf('[%s]%s:r_count\n',date(),fname[i]))
  r[['count']] <- col_sort(ddply(data,.(svr_id,date),day_aggre,aggr = 'count'))
  r

}

reduce_sta <- function(rs){
  system.time(rs <- lapply(rs,del_col))
  sta_col <- data.frame(numCol = (sapply(rs,ncol)-5)/2,
                        idx = 1:length(rs),
                        fn = fname)
  splitCol <- split(sta_col,sta_col$numCol)
  rs1 <- list()
  x <- lapply(splitCol,function(sc){
    rs1[[paste('d',sc$numCol[1],sep='')]] <<- do.call(rbind,rs[sc$idx]) 
    return(0)
  })
  rs1
}


###### STATISTIC MAIN ######
dir_datafile <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid'
fname <- list.files(dir_datafile)
coln_mapper <- data.frame(a = c('svr_id','date',paste('X',c(902,903,999,36810:36857),sep='')),
                          b = c('svrid','date','rps','wps','util',paste('iopsr',1:24,sep=''),paste('iopsw',1:24,sep='')))
require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_sta')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% gen_sta(i)
stopCluster(ck)

# fname <- list.files(file.path(dir_data,'sta_201608'))
# r <- lapply(fname,function(f){
#   load(file.path(dir_data,'sta_201608',f))
#
#   list(r[['sta']],r[['mean']],r[['sd']],r[['sum']],r[['count']])
# })
r_sta <- do.call(rbind,lapply(r,'[[',1))
r_mean <- reduce_sta(lapply(r,'[[',2))
r_sd <- reduce_sta(lapply(r,'[[',3))
r_sum <- reduce_sta(lapply(r,'[[',4))
r_count <- reduce_sta(lapply(r,'[[',5))
save(r_sta,r_mean,r_sd,r_sum,r_count,file = file.path(dir_data,'perday_201608_melt.Rda'))

