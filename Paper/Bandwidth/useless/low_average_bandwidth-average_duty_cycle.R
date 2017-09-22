#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: low_average_bandwidth-average_duty_cycle.R
#
# Description: To verify the reason that xps is weakly related to the failure rate, 
# we observe the distribution and the failure rate of different adc in each xps bar.
# If the distribution is balanced and failure rate is proportional to the duty cycle, the assumption is correct.
# The assumption is that the weak relationship between failure rate and the bandwidth is because similar bandwidth consists of different type of request. 
# If the requests are sequential, then the duty cycle will be low and wear-out of disk drive will be low.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-30 15:36:19
#
# Last   modified: 2017-08-30 15:36:20
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

load(file.path(dir_data,'uniform_data.Rda'))

attr_bw <- data.frame(attr = c('rps','wps','xps'),maxlimit = c(4000,5000,9000))
object_data <- add_average_bandwidth(io14, attr_bw)
object_data <- subset(object_data,average_xps_level!=0)
object_data$average_xps_trunc_low <- trunc_level(object_data,'average_xps_level',1980,1)

object_data$duty_cycle <- with(object_data,sum_util/count)
object_data$duty_cycle_level <- ceiling(object_data$duty_cycle/5)*5

uni_abw <- sort(unique(object_data$average_xps_trunc_low))

gen_result <- function(i){
  r <- gen_fr(subset(object_data,average_xps_trunc_low==uni_abw[i]),'duty_cycle_level',prt = F,countLimit = 100)
  r[[1]]$class <- uni_abw[i]
  r[[2]] <- r[[2]] + xlab(sprintf('duty_cycle(abw=%d)',uni_abw[i]))
  r[[3]] <- r[[3]] + xlab(sprintf('duty_cycle(abw=%d)',uni_abw[i]))
  return(r)
}

idx <- seq_len(length(uni_abw))
r <- foreachX(idx,'gen_result',outname = -1)
data_fr_list <- lapply(r,'[[',1)
p_fr_list <- lapply(r,'[[',2)
p_count_list <- lapply(r,'[[',3)

multiplot(plotlist = p_fr_list[1:22],layout = matrix(1:25,nrow=5,byrow = T))
multiplot(plotlist = p_count_list[1:22],layout = matrix(1:25,nrow=5,byrow = T))
