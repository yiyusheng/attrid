#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: extreme.R
#
# Description: extreme duty cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-15 15:32:43
#
# Last   modified: 2017-09-15 15:32:44
#
#
#

# S1. Load function and data ------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'large_quantile_dutycycle.Rda'))

# S2. Extract the maximum duty cycle
DT_quan <- quantile_dutycycle
for(i in 95:99){
  a <- paste('D',i,sep='')
  DT_quan[[a]] <- DT_quan$Q100 - DT_quan[[paste('Q',i,sep='')]]
}

attr_main <- c('mean',paste('Q',95:100,sep=''),paste('D',95:99,sep=''))
DT_quan <- gen_data(DT_quan[,c('svrid',attr_main)],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

r <- lapply(attr_main[8:12],function(attr)gen_result_feature(DT_quan,attr,100,bins = 20))
data_fr_list <- lapply(r,'[[',1)
p_fr_list <- lapply(r,'[[',2)
p_count_list <- lapply(r,'[[',3)
data_corr <- sapply(r,'[[',4)

# S3. correlation of 98% quantile to 100% quanitle break by 0.1%
# DT_quan <- gen_data(quan_largedc,expand = T)
DT_quan <- gen_data(quantile_dutycycle,expand = T)
gen_corr <- function(i){
  col_attr <- paste('Q',i,sep='')
  list[data_fr,p_fr,p_count] <- gen_result_feature(DT_quan,col_attr,100)
  corr <- cor(data_fr[,1],data_fr$AFR)
  return(data.frame(i,corr))
}
idx <- 0:100
system.time(r <- foreachX(idx,'gen_corr',frac_cores = 0.9))
corr_largedc <- setNames(do.call(rbind,r),nm=c('quantile','corr'))
p_corr <- ggplot(corr_largedc,aes(x=quantile,y=corr)) + geom_point(size=5)+geom_line()+
  xlab('Quantile(%)')+ylab('Correlation Coefficient')+
  guides(fill = guide_legend(title=NULL),color=guide_legend(title='interval')) +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.title = element_text(size=24),legend.position = 'bottom')

# S4. get adc and distribution for each bins
DT_quan$id <- seq_len(nrow(DT_quan))
DT_quan <- binning_data(DT_quan,'Q100',100)
idset <- unlist(tapply(DT_quan$id,DT_quan$Q100_level,function(arr)sample(arr,1000)))
DT_smp <- DT_quan[idset,]
ggplot(DT_smp,aes(x=factor(Q100_level)))+geom_boxplot(aes(y = age),outlier.shape = NA) + geom_jitter(aes(y = age),width = 0.2,alpha = 0.1)

# S.plot ------------------------------------
multiplot(plotlist = p_fr_list,layout = matrix(1:6,nrow=2,byrow = T))
multiplot(plotlist = p_count_list,layout = matrix(1:6,nrow=2,byrow = T))
multiplot(plotlist = p_count_list[6:7],layout = matrix(1:2,nrow=1,byrow = T))
print(p_corr)

