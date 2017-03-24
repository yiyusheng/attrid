#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: diff_wps_iopsw.R
#
# Description: find the reason why wps is different to iopsw. when wps is zero,iopsw is not zero and when iopsw is zero, wps is not zero.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-20 11:06:35
#
# Last   modified: 2017-03-20 11:06:36
#
#
#

rm(list = ls());source('~/rhead')
load(file.path(dir_datatendcastClear,'b12.Rda'))

dt_dcast <- dt_dcast[,-c(4,5)]

a <- factorX(subset(dt_dcast,iopsw == 0))
b <- apply(a[,attrName],2,quantileX)

a1 <- factorX(subset(dt_dcast,svrid %in% levels(a$svrid)))
a2 <- subset(a1,svrid == '20227' & as.Date(time) == as.Date('2015-05-05'))
splita1 <- split(a1,a1$svrid)

sta_iopsw  <- function(df){
  df_all <- factorX(subset(df,iopsw == 0))
  num_zero_days <- length(unique(as.Date(df_all$time)))
  avg_count_zday <- nrow(df_all)/num_zero_days
  num_days <- length(unique(as.Date(df$time)))
  data.frame(num_days,num_zero_days,avg_count_zday)
}

sta1 <- lapplyX(splita1,sta_iopsw)

