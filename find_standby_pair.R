#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: find_standby_pair.R
#
# Description: [2015] find standby pair of servers such as 202/203 and 198/199
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-10 17:28:21
#
# Last   modified: 2017-04-10 17:28:23
#
#
#

rm(list = ls());source('~/rhead')

find_standby_pair <- function(i){
  
}

###### MAIN STATISTIC ######
load(file.path(dir_data,'sta_dcastClear.Rda'))
load(file.path(dir_data,'sta_cut.Rda'))
fname <- list.files(dir_data15DC)
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)),
                cut_rps = c(0,2^(seq(0,16,1)),1e6),cut_iopsr = c(0,2^(seq(0,16,1)),1e6),cut_sizer = roundX(c(0,2^(seq(-6.5,1.5,0.5)),10)),
                cut_wps = c(0,2^(seq(0,16,1)),1e6),cut_iopsw = c(0,2^(seq(0,16,1)),1e6),cut_sizew = roundX(c(0,2^(seq(-6.5,1.5,0.5)),10)))
idx <- seq_len(length(fname))
r <- foreachX(idx,'find_standby_pair')
save(r,file = file.path(dir_data,'find_standby_pair.Rda'))