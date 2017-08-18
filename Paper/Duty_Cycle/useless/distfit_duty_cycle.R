#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: distfit_duty_cycle.R
#
# Description: [useless] I don't know how to explain the figure
# I fit gamma distribution for each disk drive, I analysis the two parameter of gamma distribution and the failure rate of them.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-16 20:06:28
#
# Last   modified: 2017-08-16 20:06:30
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'distfit_dutycycle.Rda'))

####################################
cut_scale <- roundX(with(para.gamma,seq(min(scale),max(scale),length.out = 20)))
cut_shape <- roundX(with(para.gamma,seq(0,10,length.out = 20)))
para.gamma$scale_level <- fct2num(cut(para.gamma$scale,cut_scale,cut_scale[-length(cut_scale)],include.lowest = T))
para.gamma$shape_level <- fct2num(cut(para.gamma$shape,cut_shape,cut_shape[-length(cut_shape)],include.lowest = T))

object_data <- para.gamma
object_data <- subset(object_data,svrid %in% io14$svrid)
object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
object_data$age <- cmdbSMP$age[match(object_data$svrid,cmdbSMP$svrid)]
object_data <- svrid_expand_disk(object_data)

# Diff
table_para <- setNames(melt_table(object_data$scale_level,object_data$shape_level),c('scale','shape','count'))
table_para$rate <- array_rate(table_para$count)
ggplot(table_para,aes(x=scale,y=shape,fill=rate))+geom_raster()

# Fr
fail_data <- f201409
fail_data <- mchAttr(fail_data,object_data,'svrid','svrid',c('scale_level','shape_level'))
fr <- ioAFR(object_data,fail_data,attr = c('scale_level','shape_level'))
ggplot(subset(fr,count>100),aes(x=scale_level,y=shape_level,fill=AFR))+geom_raster()
