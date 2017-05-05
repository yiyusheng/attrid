#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: diskNum_iops_smart.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-02 17:48:55
#
# Last   modified: 2017-05-02 17:48:57
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')
load(file.path(dir_dataSMT,'locate_fail_disk.Rda'))
load(file.path(dir_dataSMT,'find_rules_fail_disks_data.Rda'))
