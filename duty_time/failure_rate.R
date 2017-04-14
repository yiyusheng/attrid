#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: failure_rate.R
#
# Description: Generate annual failure rate(AFR) for different method to classify server by duty time
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-13 15:03:50
#
# Last   modified: 2017-04-13 15:03:52
#
#
#
rm(list = ls());source('~/rhead')
load(file.path(dir_data,'sta_cut14DC.Rda'))
load(file.path(dir_data,'sta_count14.Rda'))

# A1.classify by util rate
load(file.path(dir_data,'dts_classify_util_rate.Rda'))
dts_classify_util_rate <- subsetX(dts_classify_util_rate,svrid %in% r_sta_svrid$svrid[r_sta_svrid$count > (17280-288*7)])
