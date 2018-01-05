#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: fraction_large_value.R
#
# Description: fraction of sequancial intensity less than a threshold 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-11 16:32:25
#
# Last   modified: 2017-11-10 09:13:03
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1.data prepare ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

DT <- quan_random
thresholds <- seq(0,400,5)
attr <- paste('T',thresholds,sep='')
# Get the value for 1%-100% quantile
for(i in seq_len(length(thresholds))){
  df_frac <- get_quan_percentage(DT,thresholds[i],leq = F)
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

# S3. plot ------------------------------------
# orginal distribution of five factors
p_frac_si <- lapply(c(50,100,150,200),function(i){
    p <- p_fr_list[[which(thresholds == i)]]
    p+xlab('Fraction of Small Sequencial Intensity (%)')+
      # coord_cartesian(ylim=c(0,35))+
      scale_fill_manual(values=c('grey60','grey20'))+ ylab('Failure Rate (%)')
  })


p_frac_corr <- ggplot(data_corr,aes(x=thred,y=corr))+geom_line(size=0.5)+geom_point(size=2)+
  xlab('Sequencial Intensity Threshold (%)') + ylab("Correlation Coefficient")+gen_theme()

list[p_frac_siA,p_frac_siB,p_frac_siC,p_frac_siD] <- p_frac_si
multiplot(plotlist = p_frac_si,cols=2)

save_fig(p_frac_siA,'si_frac_fr50')
save_fig(p_frac_siB,'si_frac_fr100')
save_fig(p_frac_siC,'si_frac_fr150')
save_fig(p_frac_siD,'si_frac_fr200')
save_fig(p_frac_corr,'si_frac_corr')

save(p_frac_si,p_frac_corr,file=file.path(dir_data,'Paper','Rda','p_si_frac.Rda'))
