#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: age.R
#
# Description: age
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-01 11:52:59
#
# Last   modified: 2017-09-03 17:32:19
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
DT_quan <- gen_data(quan_xps,expand=T)

# S2. Generate failure rate ------------------------------------
bb <- T
r_age <- gen_result_feature(DT=DT_quan,attr='age',attr_max=2000,balanced_binning=bb,bins=20,bin_count=5000) #(2500,3000)(1000,5000)
list[data_fr1,p_fr1,p_count1,corr1,data_ob1] <- r_age

# S3. detail of age/model/disk number ------------------------------------
p_amd_ratio <- plot_amd_diff(data_ob1,'age_level',balanced_binning=bb)

# S4. plot ------------------------------------
ifelse(bb,multiplot(plotlist = list(p_fr1,p_count1,p_fr2,p_count2),cols=2),
       multiplot(plotlist = list(p_fr1,p_fr1+xlim(c(0,1800)),p_fr2,p_fr2+xlim(c(0,18000))),cols=2))

multiplot(plotlist = p_amd_ratio[1:4],cols=2)

multiplot(plotlist=list(p_fr1,p_amd_ratio[[1]]),cols=1)

ggplot(data_fr1,aes(x=mean_level))+geom_line(aes(y=AFR),color='red')+geom_point(aes(y=AFR),color='red')+
  geom_line(aes(y=mean_age/365),color='blue')+geom_point(aes(y=mean_age/365),color='blue')
