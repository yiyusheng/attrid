#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: continuous_idle_duty_cycle.R
#
# Description: we generage the average length/mean duty cycle/count of idle and working period of duty cycle.
# Here, we compare these attributes in disk drives to find the difference and correlate them and the disk failure.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 09:08:31
#
# Last   modified: 2017-08-11 09:08:32
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'average_legnth_idle_duty_cycle5.Rda'))

foo <- function(i){
  # S1. Prepare data
  object_data <- r[[i]]
  col_dc <- names(object_data)[2:7]
  for(cn in col_dc){
    cut_point <- unique(seq(floor(min(object_data[[cn]])),ceiling(max(object_data[[cn]])),length.out = 20))
    if(length(cut_point)==1){
      object_data[[paste(cn,'level',sep='_')]] <- rep(cut_point,length(object_data[[cn]]))
    }else{
      object_data[[paste(cn,'level',sep='_')]] <- cut(object_data[[cn]],cut_point,cut_point[-length(cut_point)])
    }
  }
  
  # S2. generate diff and fr
  list[im_diff,im_fr,im_p_diff,im_p_fr,im_object_data,im_fail_data] <- gen_fr(object_data,attr='idle_mean_level',prt=F)
  list[bm_diff,bm_fr,bm_p_diff,bm_p_fr,bm_object_data,bm_fail_data] <- gen_fr(object_data,attr='busy_mean_level',prt=F)
  plist <- list(im_p_fr,bm_p_fr)
  plist[[1]] <- plist[[1]] + xlab(sprintf('ADC(%%)[idle part][%s]',object_data$thred[1]))
  plist[[2]] <- plist[[2]] + xlab(sprintf('ADC(%%)[busy part][%s]',object_data$thred[1]))
  # multiplot(plotlist = plist,layout = matrix(1:2,nrow=1,byrow = T))
  plist
}

plist <- unlist(lapply(c(5,15,25,35,45,55)/5,foo),recursive = F)
multiplot(plotlist = plist,layout = matrix(1:12,nrow=3,byrow = T))

