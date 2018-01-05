#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: RER_failure.R
#
# Description: Check the relationship between the RER period and the failure.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-12-07 09:12:50
#
# Last   modified: 2017-12-07 09:12:51
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'smart_RSC_RER.Rda'))
load(file.path(dir_dataSMT,'sta_disk_model.Rda'))
col_char3 <- c('9WK', '9WJ', '9QK', '9SF',
               'Z1P', 'Z1N', 'Z29', 'Z1K','Z1M', 'Z1X')
r_rer <- mchAttr(r_rer,sta_ss,'sn','sn',c('svrid','char3'))
r_rer <- binning_data(r_rer,'itv',attr_max = 7,bins = 20)
r_rer$avg_rdc[r_rer$avg_rdc!=-1] <- -r_rer$avg_rdc[r_rer$avg_rdc!=-1]
r_rer <- binning_data(r_rer,'avg_rdc',attr_max = 20,bins = 20)

# S2. Generate failure records (we index the number of failure and the number of disk by the sn)
f <- subset(f201409,svrid %in% r_rer$svrid)
table_f <- setNames(melt(table(f$svrid)),nm = c('svrid','count'))
failed_svrid <- factorX(subset(r_rer,svrid %in% table_f$svrid))
failed_svrid$fcount <- table_f$count[match(failed_svrid$svrid,table_f$svrid)]
failed_svrid$svrid <- factor(failed_svrid$svrid)
failed_svrid$ftag <- 0

# Add sn for failure records.
add_failed_tag <- function(failed_svrid,all_svrid = F){
  
  if(all_svrid == T){
    failed_svrid$ftag <- 1
    return(failed_svrid)
  }
  
  # if the number of disk is larger than the number of failure, we choose sn randomly and reserve the number of the failures.
  r <- by(failed_svrid,failed_svrid$svrid,function(df){
    arr <- df$fcount
    len_sn <- length(arr)
    len_f <- arr[1]
    idx <- rep(0,len_sn)
    if(len_sn <= len_f){
      idx[sample(seq_len(len_sn),len_f,replace = T)] <- 1
    }else{
      idx[sample(seq_len(len_sn),len_f,replace = F)] <- 1
    }
    df$ftag <- idx
    return(df)
  })
  failed_svrid <- do.call(rbind,r)
  failed_svrid <- subset(failed_svrid,ftag==1)
  return(failed_svrid)
}

failed_svrid_single <- add_failed_tag(failed_svrid,F)
failed_svrid_all <- add_failed_tag(failed_svrid,T)

# S3. failure rate
# interval
data_fr_single <- ioAFR(r_rer,failed_svrid_single,attr = 'itv_level') 
data_fr_all <- ioAFR(r_rer,failed_svrid_all,attr = 'itv_level') 
ggplot(data_fr_single,aes(x=itv_level,y=AFR))+geom_line()+geom_point()

#amplitude
data_fr_single <- ioAFR(r_rer,failed_svrid_single,attr = 'avg_rdc_level') 
data_fr_all <- ioAFR(r_rer,failed_svrid_all,attr = 'avg_rdc_level') 
ggplot(data_fr_single,aes(x=-avg_rdc_level,y=AFR))+geom_line()+geom_point()
