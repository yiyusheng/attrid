#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: quantile_duty_cycle.R
#
# Description: compare the quantile of duty cycle for each disk drive and correlate the pattern with disk failure.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:24:27
#
# Last   modified: 2017-08-11 11:24:28
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'average_legnth_idle_duty_cycle.Rda'))
