#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: virtDC.R
#
# Description: Useless. I generate hazard rate myself in this file.
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

####################################
hazRate <- function(fn){
  load(file.path(dir_data,fn))
  virtDC$survTime <- as.numeric(difftime(virtDC$f_time,virtDC$use_time,tz = 'UTC',units = 'days'))
  virtDC$survTime <- round(virtDC$survTime*2/365)/2
  virtDC <- subset(virtDC,status == 'failed')
  
  cdfVirtDC <- melt(table(virtDC$survTime))
  names(cdfVirtDC) <- c('survDays','count')
  cdfVirtDC$pdf <- cdfVirtDC$count/sum(cdfVirtDC$count)
  cdfVirtDC$cumCount <- 1 - cumsum(cdfVirtDC$count)/sum(cdfVirtDC$count)
  cdfVirtDC$hazRate <- cdfVirtDC$pdf/cdfVirtDC$cumCount
  
  p <- ggplot(subset(cdfVirtDC,survDays <= 5),aes(x = survDays,y = hazRate)) + geom_bar(stat = 'identity') +
    xlab('time(years)') + ylab('Hazard Rate') + ggtitle(gsub('dataPrepare|\\.Rda','',fn))
  p
}

p13 <- hazRate('dataPrepareAFR13.Rda')
p14 <- hazRate('dataPrepareAFR14.Rda')

p13A <- hazRate('dataPrepareAFR13A.Rda')
p13B <- hazRate('dataPrepareAFR13B.Rda')
p14A <- hazRate('dataPrepareAFR14A.Rda')
p14B <- hazRate('dataPrepareAFR14B.Rda')

p10_15 <- hazRate('dataPrepareAFR10-15.Rda')

multiplot(p13,p14,p13A,p14A,p13B,p14B,p10_15,cols = 4)



# tmp.df <- subset(virtDC,status == 'failed')
# tmp.dcmdb <- subset(virtDC,status == 'working')
# 
# cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',1,dev = 'C')
# cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',12,dev = 'TS')