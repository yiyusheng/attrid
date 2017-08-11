#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: util_fr-detail.R
#
# Description: analize model,age,number of disk for each duty cycle groups.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-21 11:52:35
#
# Last   modified: 2017-07-21 11:53:17
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')
load(file.path(dir_data,'uniform_data.Rda'))

# S1. add tags and expand to disks
io_svrid <- io14
io_svrid <- mchAttr(io_svrid,model_svrid,'svrid','svrid',c('numD','mainModel'))
io_svrid$age <- cmdbSMP$age[match(io_svrid$svrid,cmdbSMP$svrid)]
io_svrid$util <- with(io_svrid,sum_util/count)

io_svrid$util_level <- 'L1'
io_svrid$util_level[io_svrid$util>12] <- 'L2'
io_svrid$util_level[io_svrid$util>32] <- 'L3'
io_svrid$util_level[io_svrid$util>50] <- 'L4'
io_svrid$util_level[io_svrid$util>60] <- 'L5'
io_svrid$util_level <- factor(io_svrid$util_level)

expand_io <- svrid_expand_disk(io_svrid)

# S2. stack ratio
expand_io_model <- expand_io
expand_io_model$mainModel <- fct2ori(expand_io_model$mainModel)
expand_io_model$mainModel[expand_io_model$mainModel %in% c('ST3250310NS','ST3500514NS','ST500NM0011')] <- 'LowCapacity'
expand_io_model$mainModel[expand_io_model$mainModel == 'None'] <- 'Other'
expand_io_model$mainModel <- factor(expand_io_model$mainModel)

f_model <- f201409
f_model <- mchAttr(f_model,expand_io_model,'svrid','svrid',c('util_level','mainModel'))

table_all_model <- with(expand_io_model,melt_table(util_level,mainModel)) 
table_f_model <- with(f_model,melt_table(util_level,mainModel))
io_model_util_level <- ioAFR(expand_io_model,f_model,attr = c('util_level','mainModel'))
dcast_model_util_level <- dcast(io_model_util_level,util_level~mainModel,value.var = 'AFR')

ggplot(io_model_util_level,aes(x = util_level,y=AFR)) + 
  geom_bar(aes(fill=mainModel),stat = 'identity',position = 'stack')+
  coord_flip() + theme(legend.position='bottom')
