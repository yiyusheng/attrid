#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: check_iops_failure.R
#
# Description: It's a newer version of find_relation_io_failure.R. In that file, it do not use dcast workload file and the targe is bias.
# In this script, I plan to find the relationship between iops and failure. 
# My target is to find the exact failed disk instead of the failed server by using IOPS. Two method to make it.
# 1.get the id from locate_fail. which parse the event description from ym's failure recrod[201401-201412]
# 2.observe iops trend of all failed server to see is there any anomaly when failure takes place.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-14 11:33:45
#
# Last   modified: 2017-04-14 11:33:48
#
#
#

rm(list = ls());setwd('/home/yiyusheng/Code/R/Disk_Workload_201406-201407/');source('~/rhead')
source('IO_statistic/iopsFunc.R')
dir_dataset <- dir_data15DC
dir_datasource <- '/home/yiyusheng/Data/Load_Data_2014/Config_Failure_2014'
dir_ten <- "/home/yiyusheng/Data/Load_Data_2015"

load(file.path(dir_dataset,'d1.Rda'))
load(file.path(dir_datasource,'load_ftr_attrid.Rda'))
load(file.path(dir_ten,'merge_id_svrid.Rda'))
load(file.path(dir_data,'locate_fail.Rda'))
load(file.path(dir_data,'sta_count15'))

# S1. generate the unified id of disk position.
mis <- merge_id_svrid[,c('svrid','sid','ip','f_time')]
col_tag <- c('sn','logicID','slotID','devID','shapeID','diskNum','partitionID')
table_tag <- apply(ext_disk[,col_tag],2,table)

# S2. plot figure to correlate the failed time and the IOPS in Plot_Figure