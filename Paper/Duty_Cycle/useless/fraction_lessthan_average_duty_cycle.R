#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: fraction_lessthan_average_duty_cycle.R
#
# Description: To get the pattern of balance duty cycle or most light duty cycle
# we generate the fraction of items less than the average duty cycle for each disk drive
# the higher the fraction is, the higher the failure rate is. It is interesting .
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-16 19:29:25
#
# Last   modified: 2017-08-16 19:29:26
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))

####################################
io14$ave_duty_cycle <- with(io14,sum_util/count)
itv <- 5
col_quan <- paste('X',seq(0,100,itv),sep='')
col_allquan <- paste('X',seq(0,100,1),sep='')

object_data <- r[,c('svrid',col_quan)]
object_data$ave_duty_cycle <- io14$ave_duty_cycle[match(object_data$svrid,io14$svrid)]
object_data$frac_less_than_adc <- apply(object_data[,c(col_quan,'ave_duty_cycle')],1,function(arr)sum(arr[-length(arr)] < arr[length(arr)])*itv)

list[table_diff,fr,p_diff,p_fr,object_data,fail_data] <- gen_fr(object_data,attr='frac_less_than_adc')
p_diff <- p_diff+xlab('Fraction of Duty Cycle Less Than the ADC(%)')
p_fr <- p_fr+xlab('Fraction of Duty Cycle Less Than the ADC(%)')
multiplot(plotlist = list(p_diff,p_fr))
####################################
# The max duty cycle and the median duty cycle
list[table_diff,fr,p_diff,p_fr] <- gen_fr(r[,c('svrid','X100')],attr='X100')
list[table_diff,fr,p_diff,p_fr] <- gen_fr(r[,c('svrid','X50')],attr='X50')

# adc of idle disk/busy disk
io <- io14
io$average_duty_cycle <- with(io,roundX(sum_util/count))
io$status_duty_cycle <- 'idle'
io$status_duty_cycle[io$svrid %in% r$svrid[r$X95>5]] <- 'busy'
list[table_diff,fr,p_diff,p_fr,object_data,fail_data] <- gen_fr(io,attr='status_duty_cycle')
