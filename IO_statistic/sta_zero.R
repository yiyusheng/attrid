#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_zero.R
#
# Description: statistic percentage of idle for each attributes
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-15 17:26:49
#
# Last   modified: 2017-03-15 17:26:51
#
#
#

rm(list = ls());source('~/rhead');source('sta_zeroFunc.R')

###### MAIN: GENERATE FRACTION OF ZERO WHEN SOME ATTRS ARE ZERO ######
fname <- list.files(dir_datatendcastClear)
fname <- fname[!grepl('a\\d.*',fname)]
load(file.path(dir_data,'sta_dcastClear_result.Rda'))
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_eachzero')

attrName <- c('util','rps','iopsr','wps','iopsw')
sta_ez <- do.call(rbind,r)
sta_ez_valid <- subset(sta_ez, count > 0)
sta_ez_aggre <- aggregate(sta_ez_valid[,-c(1:6)],sta_ez_valid[,1:5],sum)

sta_ez_rate <- sta_ez_aggre
sta_ez_rate[,paste(attrName,'z',sep='')] <- roundX(sta_ez_rate[,paste(attrName,'z',sep='')]/sta_ez_rate$count)
sta_ez_rate$countPersid <- sta_ez_rate$count/sta_ez_rate$countsid
sta_ez_rate$class <- rowSums(sta_ez_rate[,1:5])
save(sta_ez,sta_ez_aggre,sta_ez_rate,file = file.path(dir_data,'sta_zero.Rda'))

###### ANALYSIS ######
load(file.path(dir_data,'sta_zero.Rda'))
splitSER <- split(sta_ez_rate,sta_ez_rate$class)
list[a1,a2,a3,a4,a5] <- splitSER
b <- get_lines(c(0,0,0,0,1),df = sta_ez)
b[,paste(attrName,'z',sep='')] <- roundX(b[,paste(attrName,'z',sep='')]/b$count)
b1 <- subset(b,gsub('\\d.*','',id) %in% c('b','c','e'))
ggplot(b,aes(count,countsid,color = gsub('\\d.*','',id))) + geom_point(size = 10)
