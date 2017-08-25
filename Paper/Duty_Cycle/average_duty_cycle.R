#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: individual_difference-util.R
#
# Description: 
# first, we prove the individual workload difference of disk drives by showing distribution of duty cycle of all disks
# then, we give the failure rate of each type of duty cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 16:32:08
#
# Last   modified: 2017-07-21 14:47:27
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
object_data <- io14
object_data$average_duty_cycle <- with(object_data,sum_util/count)

# S1.failure rate and Distribution ----
itv <- 1
object_data$average_duty_cycle_level <- ceiling(object_data$average_duty_cycle/itv)*itv
list[data_fr,p_fr,p_count,p_countF] <- gen_fr(object_data,attr='average_duty_cycle_level',prt = F,countLimit = 30)
pearson <- cor(data_fr$average_duty_cycle_level,data_fr$AFR)
trunc_value <- data_fr$percentage[data_fr$average_duty_cycle_level>=1&data_fr$average_duty_cycle_level<=3]

# S2. plot distribution and failure rate
p_fr <- p_fr + xlab('Average Duty Cycle of Disk Drives(%)')
p_count <- p_count+coord_cartesian(ylim=c(0,10)) + 
  annotate("text", x=15, y=9.5, size = 8,label= sprintf("(%.2f%%, %.2f%%, %.2f%%)",trunc_value[1],trunc_value[2],trunc_value[3]))+
  xlab('Average Duty Cycle of Disk Drives(%)')

ggsave(file=file.path(dir_data,'Paper','FailureRate_duty_cycle.eps'), plot=p_fr, width = 8, height = 6, dpi = 100)
ggsave(file=file.path(dir_data,'Paper','Dist_duty_cycle.eps'), plot=p_count, width = 8, height = 6, dpi = 100)

# multiplot(plotlist = list(p_fr,p_count),cols=2)

# S3. Age, model and disk number
# Distribution difference is in useless/average_duty_cycle_dist_age_model_numD.R
itv <- 5
object_data <- io14
object_data$average_duty_cycle <- with(object_data,sum_util/count)
object_data$average_duty_cycle_level <- ceiling(object_data$average_duty_cycle/itv)*itv
list[object_data,fail_data,fr_data] <- gen_data(object_data,'average_duty_cycle_level')
p_amd_adc <- plot_amd_diff(object_data,'average_duty_cycle_level')
# multiplot(plotlist = p_amd_adc[2:4],cols=3)
p_amd_adc[[2]]
ggsave(file=file.path(dir_data,'Paper','duty_cycle-age.eps'), plot=p_amd_adc[[2]], width = 8, height = 6, dpi = 100)
