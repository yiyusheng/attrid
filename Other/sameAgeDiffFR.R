#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sameAgeDiffFR.R
#
# Description: Observe the phenomenon that servers with same age 
# have different failure rate trend in different window of failure records.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-23 15:58:16
#
# Last   modified: 2016-09-23 15:58:20

# S1. plot correlation between disk age and AFR
list[tmp.fC13,tmp.cmdbC13,tmp.fTS13,tmp.cmdbTS13] <- dataLoadF1('dataPrepareAFR1306_1307.Rda')
list[tmp.fC14,tmp.cmdbC14,tmp.fTS14,tmp.cmdbTS14] <- dataLoadF1('dataPrepareAFR1406_1407.Rda')
# list[tmp.fC,tmp.cmdbC,tmp.fTS,tmp.cmdbTS] <- list(tmp.fC14,tmp.cmdbC14,tmp.fTS14,tmp.cmdbTS14)

# S1.5 statistic 13 and 14
lenF <- c(nrow(tmp.fC13),nrow(tmp.fC14),nrow(tmp.fTS13),nrow(tmp.fTS14))
lenCMDB <- c(nrow(tmp.cmdbC13),nrow(tmp.cmdbC14),nrow(tmp.cmdbTS13),nrow(tmp.cmdbTS14))
cm13C <- ioAFR(tmp.cmdbC13,tmp.fC13,c('dev_class_id','shTimeQu'));cm13C$class <- 'Nserv13'
cm13TS <- ioAFR(tmp.cmdbTS13,tmp.fTS13,c('dev_class_id','shTimeQu'));cm13TS$class <- 'Sserv13'
cm14C <- ioAFR(tmp.cmdbC14,tmp.fC14,c('dev_class_id','shTimeQu'));cm14C$class <- 'Nserv14'
cm14TS <- ioAFR(tmp.cmdbTS14,tmp.fTS14,c('dev_class_id','shTimeQu'));cm14TS$class <- 'Sserv14'

tmp <- cm14TS
tmp$shTimeQu <- tmp$shTimeQu - 1
x <- merge(cm13TS[,c('dev_class_id','shTimeQu','count','fCount')],
           tmp[,c('dev_class_id','shTimeQu','count','fCount')],
           by = c('dev_class_id','shTimeQu','count'))

cmNserv <- rbind(cm13TS,tmp)
ggplot(cmNserv,aes(x = shTimeQu,group = class,color = class)) + 
  geom_line(aes(y = fCount))