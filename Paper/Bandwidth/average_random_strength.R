#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_random_strength.R
#
# Description: 
# We present the distribution of the strength of request random and correlate it and the failure rate
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 16:32:08
#
# Last   modified: 2017-08-29 21:58:23
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

gen_result_random_strength <- function(i,all=F){
  cat(sprintf('[%s]\t %s SATRT!!!\n',date(),i))
  r <- gen_result_feature(DT_quan,col_name[i],quantile(DT_quan[[col_name[i]]],0.99))
  if(all){
    return(r[1:4])
  }else{
    return(r[[4]])
  }
}

# S1. Load Data ------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

DT_raw <- quan_random
thresholds<- c(10,20,50,100,200)
attr_main <- paste('T',thresholds,sep='')
for(i in seq_len(length(thresholds))){
  df_frac <- get_quan_percentage(DT_raw,thresholds[i])
  DT_raw[[attr_main[i]]] <- 100-df_frac$fraction[match(DT_raw$svrid,df_frac$svrid)]*100
}
col_name <- setdiff(names(DT_raw),c('svrid','count'))
DT_quan <- gen_data(DT_raw,expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# S2. generate result ------
idx <- which(col_name %in% c('mean','sd',attr_main,paste('Q',c(1:3,20,95),sep='')))
idx <- seq_len(length(col_name))
r <- foreachX(idx,'gen_result_random_strength',outname = -1)
r <- lapply(idx,gen_result_random_strength,all=T)
corr <- unlist(r)
plot(corr)

list[data_fr,p_fr,p_count,corr] <- gen_result_feature(DT_quan,'mean',bins = 20)


# S_plot ------
p_rs_fr <- p_fr + xlim(c(1,1000))+xlab('The Sequential Strength') + theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))
p_rs_dist <- p_count+ xlim(c(1,1000))+xlab('The Sequential Strength')

save_fig(p_rs_fr,'rs_fr')
save_fig(p_rs_dist,'rs_dist')
