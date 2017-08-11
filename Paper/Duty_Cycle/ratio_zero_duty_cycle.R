#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: ratio_zero_duty_cycle.R
#
# Description: [No useful conclusion because we focus on the pattern of 0/1 instead of the value. The value is more important than 0/1]
# generate the ratio of zero duty cycle for each drive
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-10 11:18:21
#
# Last   modified: 2017-08-10 11:18:23
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'sta_cut14DC.Rda'))
ss_util <- lapplyX(lapply(r,'[[',2),'[[',1)

# S1. generate ratio of zero for each drive
ss_util <- subsetX(ss_util,svrid %in% io14$svrid)
ss_util <- mchAttr(ss_util,model_svrid,'svrid','svrid',c('numD','mainModel'))
ss_util$age <- cmdbSMP$age[match(ss_util$svrid,cmdbSMP$svrid)]
ss_util <- svrid_expand_disk(ss_util)

ss_util$zero_ratio <- round(ss_util$X0/rowSums(ss_util[,grep('X',names(ss_util))]),digits = 2)*100
ss_util$zero_ratio <- round(ss_util$X0/rowSums(ss_util[,grep('X',names(ss_util))]),digits = 2)*100

table_zero_ratio <- setNames(melt(table(ss_util$zero_ratio)),nm = c('ratio','count'))
table_zero_ratio$countratio <- array_rate(table_zero_ratio$count)
table_zero_ratio$cumratio <- cumsum(table_zero_ratio$countratio)

sum_bimodal <- table_zero_ratio$countratio[table_zero_ratio$ratio==0]+table_zero_ratio$countratio[table_zero_ratio$ratio==100]
sum_5ratio_95ratio <- sum(table_zero_ratio$countratio[table_zero_ratio$ratio >=5 & table_zero_ratio$ratio<=95])

table_zero_ratio$countratio[table_zero_ratio$countratio>0.02] <- 0.02
p_zeroRatio <- ggplot(table_zero_ratio,aes(x=ratio,y=countratio)) + geom_bar(stat='identity') + 
  xlab('Ratio of Zero Duty Cycle(%)') + ylab('Percentage(%)') + 
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.position = 'top')
ggsave(file=file.path(dir_data,'Paper','Ratio_zero_duty_cycle.eps'), plot=p_zeroRatio, width = 8, height = 6, dpi = 100)
print(p_zeroRatio)

# S2. failure rate of different ratio of zero duty cycle
object_ratio <- ss_util
size_bin <- 1
object_ratio$ratio_level <- with(object_ratio,floor(zero_ratio/size_bin)*size_bin)

fail_ratio <- f201409
fail_ratio$ratio_level <- object_ratio$ratio_level[match(fail_ratio$svrid,object_ratio$svrid)]

ratio_fr <- ioAFR(object_ratio,fail_ratio,attr = 'ratio_level')
# fr_all <- with(ratio_fr,sum(fCount)/sum(count))*600
ratio_fr$count_rate <- array_rate(ratio_fr$count)
ratio_fr$level <- 'low'
ratio_fr$level[ratio_fr$AFR > 5] <- 'high'
p_zeroRatio_fr <- ggplot(ratio_fr,aes(x = ratio_level,y = AFR,fill = level)) + geom_bar(stat = 'identity')+
  xlab('The Ratio of Zero Duty Cycle of Disk Drives(%)') + ylab('Failure Rate(%)') + guides(fill = guide_legend(title=NULL)) +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.position = 'bottom')
ggsave(file=file.path(dir_data,'Paper','FailureRate_ratio_zero_duty_cycle.eps'), plot=p_zeroRatio_fr, width = 8, height = 6, dpi = 100)
print(p_zeroRatio_fr)

