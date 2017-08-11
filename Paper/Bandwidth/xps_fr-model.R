#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: xps_fr-model.R
#
# Description: Calculate failure rate and bit error rate of all disks/each models
# to test how strong the relationship between the workload and error
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 16:32:08
#
# Last   modified: 2017-07-18 09:34:48
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source(file.path('Bandwidth','xps_fr-modelFunc.R'))
load(file.path(dir_dataCF,'serverInfo.Rda'))
load(file.path(dir_dataSMT,'sta_disk_model.Rda'))
load(file.path(dir_dataCF,'uwork2014_06_09.Rda'))
load(file.path(dir_data,'sta_base14.Rda'))

# S1. Filter and deduplicate data[2014_06-09]
f201409 <- subset(f2014_06_09,grepl('硬盘故障',ftype_d1) | grepl('硬盘故障',ftype_d2))
table_f201409 <- melt(table(f201409$ftype))
table_f201409$rate <- array_rate(table_f201409$value)
f201409 <- subset(f201409,ftype %in% table_f201409$Var1[table_f201409$rate > 0.1])

dayDup <- 30
f201409 <- dedupTime(f201409,dayDup,'svrid','ftime')
f201409 <- factorX(f201409)

# S2. Get I/O workload for each svrid
sta_model$numD <- 0
sta_model$numD[sta_model$numDisk <= 2] <- 1
sta_model$numD[sta_model$numDisk >= 8 & sta_model$numDisk <= 16] <- 12

r_sta_svrid <- replace_value(r_sta_svrid)
r_sta_svrid <- mchAttr(r_sta_svrid,sta_model,'svrid','svrid','numD')
r_sta_svrid <- subset(r_sta_svrid, count > 15000 & sum_xps > 10000 & numD > 0)
r_sta_svrid$sum_xps <- r_sta_svrid$sum_rps + r_sta_svrid$sum_wps
r_sta_svrid$mean_xps <- r_sta_svrid$sum_xps/r_sta_svrid$numD

seq_xps_log2 <- 2^with(r_sta_svrid,seq(min(log2(mean_xps)),max(log2(mean_xps)),(max(log2(mean_xps))-min(log2(mean_xps)))/15))
cut_method <- seq_xps_log2
# r_sta_svrid$mean_xps_cut <- fct2num(cut(r_sta_svrid$mean_xps,cut_method,cut_method[-length(cut_method)]))
r_sta_svrid$mean_xps_cut <- cutX(arr=r_sta_svrid$mean_xps,seqArr=seq_xps_log2[4:12],highpoint=seq_xps_log2[13])
r_sta_svrid <- subset(r_sta_svrid,!is.na(mean_xps_cut))

f201409$mean_xps_cut <- r_sta_svrid$mean_xps_cut[match(f201409$svrid,r_sta_svrid$svrid)]
f201409 <- subset(f201409,svrid %in% r_sta_svrid$svrid)

# S4. generate failure rate based on model
cmdbSMP <- cmdb[cmdb$dev_class_id %in% c('C1','TS3','TS4','TS5','TS6'),c('svr_asset_id','dev_class_id','bs1','use_time','raid')]
names(cmdbSMP)[1] <- 'svrid'

# add xps info and model info from sta_model and r_sta_svrid
addCol <- c('count','sum_rps','sum_wps','mean_xps','mean_xps_cut','numD')
cmdbSMP <- mchAttr(cmdbSMP,r_sta_svrid,'svrid','svrid',addCol)
cmdbSMP <- mchAttr(cmdbSMP,sta_model,'svrid','svrid',c('mainModel'))
fSMP <- mchAttr(f201409,r_sta_svrid,'svrid','svrid',addCol)
fSMP <- mchAttr(fSMP,sta_model,'svrid','svrid',c('mainModel'))

# clear and add for f
cmdbSMP <- factorX(subset(cmdbSMP,!is.na(count) & !mainModel == 'None'))
fSMP <- factorX(subset(fSMP,svrid %in% cmdbSMP$svrid))

# Failure rate and Bit error rate
FR_model <- ioAFR(io = cmdbSMP,f = fSMP,attr = c('mainModel','mean_xps_cut'),attr_numDisk = 'numD')
FR_all <- ioAFR(io = cmdbSMP,f = fSMP,attr ='mean_xps_cut')

# BER_all <- BER_gen(io = cmdbSMP,f = fSMP,attr = 'mean_xps_cut',bitattr = 'sum_xps',diskCount = 1)
# ber_all <- nrow(f201409)/sum(r_sta_svrid$sum_xps)
# pber <- ggplot(BER_all,aes(x = log2(mean_xps_cut))) + geom_bar(aes(y = log2(BER)),stat = 'identity')

pmodel <- lapply(unique(cmdbSMP$mainModel),
                 function(m)ggplot(subset(FR_model,mainModel == m),aes(x = log2(mean_xps_cut))) + geom_bar(aes(y = AFR),stat = 'identity') + 
                   ggtitle(m) + xlab('xps(2^x KB)') + ylab('Failure Rate(%)'))
pall <- ggplot(FR_all,aes(x = log2(mean_xps_cut))) + geom_bar(aes(y = AFR),stat = 'identity')
pmodel_line <- ggplot(FR_model,aes(x = log2(mean_xps_cut))) + geom_line(aes(y = AFR,group=mainModel,linetype=mainModel,color=mainModel))

multiplot(plotlist = c(pmodel,list(pall)),cols = 3)


#[***table: display the number of different disk model***]
table_unimodel <- setNames(melt(table(sta_ss$unified_model)),c('model','count'))
table_unimodel$rate <- array_rate(table_unimodel$count)
sum(table_unimodel$rate[table_unimodel$count>8000 & table_unimodel$model!= 'None'])
