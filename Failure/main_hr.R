#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: main_hr.R
#
# Description: generate hazard rate for different taxonomy
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-17 09:23:41
#
# Last   modified: 2017-01-17 09:23:43
#
#
#
rm(list = ls())
source('head.R')
source('harzard_rate.R')
load(file.path(dir_data,'bs_virtdisk.Rda'))

# Taxonomy1. All
vt <- vt_smp
vt$tax <- 'All'
list[p.dev.month,r.dev.month] <- surv_disk(vt,30,'tax')
