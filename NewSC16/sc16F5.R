#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: sc16F5.R
#
# Description: Correlate failure rate and strength of periodicity
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-10 17:28:21
#
# Last   modified: 2017-07-12 11:11:59
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/NewSC16/');source('~/rhead')

load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))
load(file.path(dir_data,'NewSC16','feature_strength_of_periodicity.Rda'))

#@@@ LOAD DATA @@@#
# source('attr_function.R')
# source('AFR_io_function.R')
# source('sc16F5Func.R')
# load(file.path(dir_data,'load_ftr_attrid.Rda'))
# source(file.path(dir_code,'AFR_io_prepare.R'))

#####################################################################################################
divCV <- c(0,0.02,0.04,0.06,0.08,0.1,0.2,0.4,0.6,0.8,1.0,Inf)

ioDuraCV <- data.frame(svrid = dura999$svrid)
ioDuraCV$cvD999 <- dura999$cv[match(ioDuraCV$svrid,dura999$svrid)]
ioDuraCV$cut999 <- cut(ioDuraCV$cvD999,divCV,include.lowest = T)
ioDuraCV$cvD9023 <- dura9023$cv[match(ioDuraCV$svrid,dura9023$svrid)]
ioDuraCV$cut9023 <- cut(ioDuraCV$cvD9023,divCV,include.lowest = T)
ioDuraCV <- factorX(subset(ioDuraCV,!is.na(cut999) & !is.na(cut9023)))

f <- merge(tmp.f,ioDuraCV,by.x = 'svr_id',by.y = 'svrid')
io <- merge(tmp.cmdb,ioDuraCV,by.x = 'svr_asset_id',by.y = 'svrid')

# util
AFR_cvD999C <- item_order(AFR_attr_notime(f,io,'cut999',1,dev = 'C'))
AFR_cvD999TS <- item_order(AFR_attr_notime(f,io,'cut999',12,dev = 'TS'))
AFR_cvD999 <- classExchg(rbind(AFR_cvD999C,AFR_cvD999TS))
AFR_cvD999$itemN <- gsub('^\\[|^\\(|,.*$','',AFR_cvD999$item)
AFR_plot(DT = AFR_cvD999,title = 'fig5B',
         ylimL = 0,ylimR = 0.6,
         para_x = 'itemN',para_y = 'AFR',para_fill = 'class',
         para_xlab = 'CV of difference',para_ylab = 'Annual Failure Rate (%)')


# total amount
AFR_cvD9023C <- item_order(AFR_attr_notime(f,io,'cut9023',1,dev = 'C'))
AFR_cvD9023TS <- item_order(AFR_attr_notime(f,io,'cut9023',12,dev = 'TS'))
AFR_cvD9023 <- classExchg(rbind(AFR_cvD9023C,AFR_cvD9023TS))
AFR_cvD9023$itemN <- gsub('^\\[|^\\(|,.*$','',AFR_cvD9023$item)

# AFR_cvD9023N <- AFR_cvD9023
# AFR_cvD9023N$AFR[AFR_cvD9023N$itemN == 0.06 & AFR_cvD9023N$class == 'Sserv'] <- AFR_cvD9023N$AFR[AFR_cvD9023N$itemN == 0.06 & AFR_cvD9023N$class == 'Sserv'] - 0.6
AFR_plot(DT = AFR_cvD9023,title = 'fig5A',ylimL = 0,ylimR = 0.6,
         para_x = 'itemN',para_y = 'AFR',para_fill = 'class',
         para_xlab = 'CV of difference',para_ylab = 'Annual Failure Rate (%)')
