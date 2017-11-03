#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: bandwidth.R
#
# Description: Duty Cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-01 11:52:59
#
# Last   modified: 2017-09-01 11:53:00
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
require('reshape')

# MAIN ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_ratio.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

# DT = quantile_dutycycle;title='mean_duty_cycle';attr_main = 'mean';attr_main_max = 100;bin_count = 530*2
# DT = quantile_dutycycle;title='extreme_duty_cycle';attr_main = 'Q100';attr_main_max = 100;bin_count = 5300
# DT = quan_xps;title='mean_bandwidth';attr_main = 'mean';attr_main_max = 9000;bin_count = 5300
# DT = quan_ratio;title='mean_ratio';attr_main = 'mean';attr_main_max = 1;bin_count = 5300
DT = quan_random;title='mean_random_strength';attr_main = 'mean';attr_main_max = 400;bin_count = 5300
# DT = cmdbSMP;title='Disk_Age';attr_main = 'age';attr_main_max = 1500;bin_count = 5300


factor_rsmp <- c('age','adc','abw','mainModel','numD')
DT_quan <- gen_data(DT[,c('svrid',attr_main)],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]
maxy = bin_count/5300

# S2. Failure rate ------------------------------------
para <- expand.grid.df(data.frame(attr=attr_main),
                       data.frame(af=factor_rsmp[1:5],
                                  ma=c(365*5,100,9000,0,0)[1:5]),
                       data.frame(am=attr_main_max),
                       data.frame(bb=c(T,F)),
                       data.frame(rsmp=c(F,T)),
                       data.frame(ob=F),
                       data.frame(bc=bin_count),
                       data.frame(my=maxy))
para <- para[order(para$attr,para$af,para$bb,para$rsmp),]
ind <- seq_len(nrow(para))
r <- foreachX(ind,'gen_result_feature_all_dopa',frac_cores = 0.8,outname =-1)

data_fr_list <- lapply(r,'[[',1)
p_fr_list <- lapply(r,'[[',2)
p_count_list <- lapply(r,'[[',3)
data_corr_list <- lapply(r,'[[',4)
p_factors_list <- lapply(r,'[[',5)
p_factors_list_ff <- lapply(factor_rsmp,function(x)p_factors_list[[which(para$af==x&para$bb==F & para$rsmp==F)]][[x]])
p_factors_list_ft <- lapply(factor_rsmp,function(x)p_factors_list[[which(para$af==x&para$bb==F & para$rsmp==T)]][[x]])
p_factors_list_tf <- lapply(factor_rsmp,function(x)p_factors_list[[which(para$af==x&para$bb==T & para$rsmp==F)]][[x]])
p_factors_list_tt <- lapply(factor_rsmp,function(x)p_factors_list[[which(para$af==x&para$bb==T & para$rsmp==T)]][[x]])
# data_ob_list <- lapply(r,'[[',6)
data_fr <- data_fr_list[[which(para$af=='abw'&para$bb==T&para$rsmp==T)]]

# S_end. plot ------------------------------------
# orginal distribution of five factors
png(filename = file.path(dir_data,'Paper','templete',paste(title,'_dist_ori_bb.jpg',sep='')),width = 1100, height = 500, bg = "white")
multiplot(plotlist = c(list(p_count_list[which(para$bb==T & para$rsmp==F)[1]][[1]]+xlab(title)),
                       p_factors_list_tf),
          layout = matrix(1:6,nrow=2,byrow = F))#
dev.off()

png(filename = file.path(dir_data,'Paper','templete',paste(title,'_dist_ori_ub.jpg',sep='')),width = 1100, height = 500, bg = "white")
multiplot(plotlist = c(list(p_count_list[which(para$bb==F & para$rsmp==F)[1]][[1]]+xlab(title)),
                       p_factors_list_ff),
          layout = matrix(1:6,nrow=2,byrow = F))#
dev.off()

# failure rate when we resample the five factors
png(filename = file.path(dir_data,'Paper','templete',paste(title,'_fr_bb.jpg',sep='')),width = 1100, height = 500, bg = "white")
multiplot(plotlist = c(list(p_fr_list[which(para$bb==T & para$rsmp==F)[1]][[1]]+xlab(title)),
                       p_fr_list[which(para$bb==T & para$rsmp==T)]),
          layout = matrix(1:6,nrow=2,byrow = F))#
dev.off()

png(filename = file.path(dir_data,'Paper','templete',paste(title,'_fr_ub.jpg',sep='')),width = 1100, height = 500, bg = "white")
multiplot(plotlist = c(list(p_fr_list[which(para$bb==F & para$rsmp==F)[1]][[1]]+xlab(title)),
                       p_fr_list[which(para$bb==F & para$rsmp==T)]),
          layout = matrix(1:6,nrow=2,byrow = F))#
dev.off()

# resampled distribution of five factors
png(filename = file.path(dir_data,'Paper','templete',paste(title,'_dist_rsmp_bb.jpg',sep='')),width = 1100, height = 500, bg = "white")
multiplot(plotlist = c(list(p_count_list[which(para$bb==T & para$rsmp==T)[1]][[1]]+xlab(title)),
                       p_factors_list_tt),
          layout = matrix(1:6,nrow=2,byrow = F))#
dev.off()

png(filename = file.path(dir_data,'Paper','templete',paste(title,'_dist_rsmp_ub.jpg',sep='')),width = 1100, height = 500, bg = "white")
multiplot(plotlist = c(list(p_count_list[which(para$bb==F & para$rsmp==T)[1]][[1]]+xlab(title)),
                       p_factors_list_ft),
          layout = matrix(1:6,nrow=2,byrow = F))#
dev.off()

# distribution of five factors when each of them are resampled
png(filename = file.path(dir_data,'Paper','templete',paste(title,'_bb_rsmp_dist_all.jpg',sep='')),width = 1920, height = 1080, bg = "white")
multiplot(plotlist = unlist(p_factors_list[para$bb==T & para$rsmp==T],recursive = F),
          layout = matrix(1:25,nrow=5,byrow = F))
dev.off()

png(filename = file.path(dir_data,'Paper','templete',paste(title,'_ub_rsmp_dist_all.jpg',sep='')),width = 1920, height = 1080, bg = "white")
multiplot(plotlist = unlist(p_factors_list[para$bb==F & para$rsmp==T],recursive = F),
          layout = matrix(1:25,nrow=5,byrow = F))
dev.off()
return(r)


#rs ------
p_rs_adc <- p_factors_list_tf[[2]]
p_rs_abw <- p_factors_list_tf[[3]]

