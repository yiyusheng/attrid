#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: virtDC.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-23 15:40:42
#
# Last   modified: 2016-09-23 15:40:45

rm(list = ls())
source('head.R')
source('sc16F1Func.R')
load(file.path(dir_data,'dataPrepareAFR10-13.Rda'))
####################################
virtDC <- virt_disk(tmp.f,tmp.cmdb,as.POSIXct('2013-10-01',tz = 'UTC'))
virtDC$survTime <- round(as.numeric(difftime(virtDC$f_time,virtDC$use_time,tz = 'UTC',units = 'days'))/30)
ggplot(virtDC,aes(x = survTime)) + stat_ecdf()

cdfVirtDC <- melt(table(round(virtDC$survTime)))
names(cdfVirtDC) <- c('survDays','count')
cdfVirtDC$pdf <- cdfVirtDC$count/sum(cdfVirtDC$count)
cdfVirtDC$cumCount <- 1 - cumsum(cdfVirtDC$count)/sum(cdfVirtDC$count)
cdfVirtDC$hazRate <- cdfVirtDC$pdf/cdfVirtDC$cumCount

ggplot(cdfVirtDC,aes(x = survDays,y = hazRate)) + geom_line()





tmp.df <- subset(virtDC,status == 'failed')
tmp.dcmdb <- subset(virtDC,status == 'working')

cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',1,dev = 'C')
cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',12,dev = 'TS')