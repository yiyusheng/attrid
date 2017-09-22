#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: fraction.R
#
# Description: fraction of duty cycle larger than a threshold 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-11 16:32:25
#
# Last   modified: 2017-09-11 16:32:26
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1.data prepare ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))

DT = quantile_dutycycle
thresholds = seq(0,100,1)
attr <- paste('T',thresholds,sep='')
for(i in seq_len(length(thresholds))){
  df_frac <- get_quan_percentage(DT,thresholds[i])
  DT[[attr[i]]] <- df_frac$fraction[match(DT$svrid,df_frac$svrid)]*100
}

attr_main <- paste('T',thresholds,sep='')
DT_quan <- gen_data(DT[,c('svrid',attr_main)],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]


# S2. analysis ------------------------------------
system.time(r <- lapply(attr_main,function(a)gen_result_feature(DT_quan,a,100)))
data_fr_list <- lapply(r,'[[',1)
p_fr_list <- lapply(r,'[[',2)
p_count_list <- lapply(r,'[[',3)
data_corr <- data.frame(thred = thresholds[-1],corr=sapply(r,'[[',4)[-1])

# fr_gqe <- function(df){
#   len <- nrow(df)
#   df$AFR_gqe <- sapply(1:len,function(i)sum(df$fCount[i:len])/sum(df$count[i:len]))*600
#   return(df)
# }
# 
# twice_afr <- function(df){
#   df[,1][min(which(df$AFR_gqe>df$AFR_gqe[1]*2),nrow(df))]
#   # df[,1][min(which(df$AFR_gqe>10),nrow(df))]
# }
# 
# data_fr_list <- lapply(data_fr_list,fr_gqe)
# d <- data_fr_list[[which(thresholds==60)]]
# sum(d$fCount[d$T60_level>=0 & d$T60_level<=0.3])/sum(d$count[d$T60_level>=0 & d$T60_level<=0.3])*600
# sum(d$fCount[d$T60_level>0.3])/sum(d$count[d$T60_level>0.3])*600
# 
# thred <- data.frame(t=thresholds,warning_thred = sapply(data_fr_list,twice_afr))
# p_thred <- ggplot(thred,aes(t,warning_thred))+geom_line()+geom_point()

# S3. plot ------------------------------------
# orginal distribution of five factors
list[p_frac_fr50,p_frac_fr60,p_frac_fr70,p_frac_fr80] <- 
  lapply(c(50,60,70,80),function(i){
    p <- p_fr_list[[which(thresholds == i)]]
    p+xlab('The Fraction of Large Duty Cycle(%)')+coord_cartesian(ylim=c(0,35))
  })

p_frac_corr <- ggplot(data_corr,aes(x=thred,y=corr))+geom_line(size=0.5)+geom_point(size=2)+
  xlab('The Duty Cycle Threshold(%)') + ylab("Pearson's Correlation Coefficient")+
  gen_theme()


save_fig(p_frac_fr50,'frac_fr50')
save_fig(p_frac_fr60,'frac_fr60')
save_fig(p_frac_fr70,'frac_fr70')
save_fig(p_frac_fr80,'frac_fr80')
save_fig(p_frac_corr,'frac_corr')

save(p_frac_fr50,p_frac_fr60,p_frac_fr70,p_frac_fr80,p_frac_corr,file=file.path(dir_data,'Paper','Rda','p_frac.Rda'))
