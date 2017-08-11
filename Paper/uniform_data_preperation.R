#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: uniform_data_preperation.R
#
# Description: unified data preprocessing for cmdb,f2014_06_09 and io14
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-22 10:19:43
#
# Last   modified: 2017-07-22 10:19:44
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_dataCF,'serverInfo.Rda')) #CMDB
load(file.path(dir_dataSMT,'sta_disk_model.Rda')) # DISK 
load(file.path(dir_dataCF,'uwork2014_06_09.Rda')) # Failure
load(file.path(dir_data,'sta_base14.Rda')) # IO

# S1. [cmdb]generate age and a simple version
cmdb$age <- as.numeric(with(cmdb,difftime(as.p('2014-06-01'),use_time,units = 'days')))
cmdbSMP <- cmdb[,c('svr_asset_id','dev_class_id','bs1','raid','use_time','age')]
names(cmdbSMP)[1] <- 'svrid'

# S2. [failure record]Filter and deduplicate data[2014_06-09]
f201409 <- subset(f2014_06_09,grepl('硬盘故障',ftype_d1) | grepl('硬盘故障',ftype_d2))
table_f201409 <- melt(table(f201409$ftype))
table_f201409$rate <- array_rate(table_f201409$value)
f201409 <- subset(f201409,ftype %in% table_f201409$Var1[table_f201409$rate > 0.1])
dayDup <- 30
f201409 <- dedupTime(f201409,dayDup,'svrid','ftime')
f201409 <- factorX(f201409)

# S3. [Disk number]set disk number to 1/12
sta_model$numD <- 0
sta_model$numD[sta_model$numDisk <= 2] <- 1
sta_model$numD[sta_model$numDisk >= 8 & sta_model$numDisk <= 16] <- 12
sta_model$mainModel <- factor(sta_model$mainModel)
model_svrid <- subset(sta_model,numD > 0 & !is.na(numD) & !is.na(mainModel),
                      c('svrid','firstPerc','secondPerc','numDisk','mainModel','numD'))

# S4. [io]
io14 <- replace_value(sta_svrid)
io14$fn <- factor(io14$fn)
io14 <- subset(io14, count > 5000)


# S5. Intersect
its <- intersect(cmdbSMP$svrid,intersect(model_svrid$svrid,io14$svrid))
cmdbSMP <- factorX(subset(cmdbSMP,svrid %in% its))
io14 <- factorX(subset(io14,svrid %in% its))
model_svrid <- factorX(subset(model_svrid,svrid %in% its))
f201409 <- factorX(subset(f201409,svrid %in% its))

save(cmdbSMP,io14,model_svrid,f201409,file = file.path(dir_data,'uniform_data.Rda'))
