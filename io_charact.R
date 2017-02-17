#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: io_charact.R
#
# Description: research for io characteristics
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-02-13 11:04:22
#
#
#
rm(list = ls())
source('head.R')
dir_dataten <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid/'
dir_datatendcast <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcast/'

# origin data charactoristics
# load(file.path(dir_datatendcast,'d1test.Rda'))
# dd <- dt_dcast[,1:4]
# dd$iopsr <- rowSums(dt_dcast[grepl('iopsr',names(dt_dcast))])
# dd$iopsw <- rowSums(dt_dcast[grepl('iopsw',names(dt_dcast))])
# dd$rater <- dd$rps/dd$iopsr
# dd$ratew <- dd$wps/dd$iopsw
# dd$rater[dd$rater < 0] <- 0
# dd$ratew[dd$ratew < 0] <- 0
# dd$date <- as.Date(dd$time)
# 
# 
# dt_dcast$date <- as.Date(dt_dcast$time)
# dd_day <- by(dt_dcast,list(dt_dcast$svrid,dt_dcast$date),function(x){
#   tmp <- x[1,]
#   tmp[grepl('iopr',names(tmp))] <- rowSums(x[grepl('iopr',names(x))],na.rm = T)
# })
# # F1. single disk bear most of I/O request
# dt_dcast[dt_dcast == -1] <- NA
# splitDD <- split(dt_dcast,dt_dcast$svrid)
# f1r <- data.frame(t(sapply(splitDD,function(x){
#   tmp <- colSums(x[,grepl('iopsr',names(dt_dcast))],na.rm = T)
#   round(tmp/sum(tmp)*100,digits = 4)})))
# f1w <- data.frame(t(sapply(splitDD,function(x){
#   tmp <- colSums(x[,grepl('iopsw',names(dt_dcast))],na.rm = T)
#   round(tmp/sum(tmp)*100,digits = 4)})))
# 
# # F2.
# r <- aggregate(dd$rater,by = list(dd$svrid,dd$date),mean)
# r <- subset(r,!is.na(r$x))
# names(r) <- c('svrid','date','mean')
# r1 <- t(dcast(svrid~date,data = r,value.var = 'mean'))
# 
# splitdd <- split(dd,dd$svrid)
# a <- subset(splitdd[[1]],time > as.p('2014-07-01') & time <= as.p('2014-07-02'))
# ggplot(a,aes(x = time,y = rater)) + geom_line()

# day/week/month data charactoristics
load(file.path(dir_data,'io_timescale.Rda'))
load(file.path(dir_data,'sta_201608.Rda'))
load(file.path(dir_dataten,'..','failRecord_1407-1506.Rda'))
dd_day <- lapply(r,'[[','d1')
dd_week <- lapply(r,'[[','d2')
dd_month <- lapply(r,'[[','d3')

sta_col <- data.frame(numCol = (sapply(dd_day,ncol)-5)/2,
                      idx = 1:length(dd_day))
splitCol <- split(sta_col,sta_col$numCol)
x <- lapply(splitCol,function(sc){
  eval(parse(text = sprintf('dd_day%d <<- do.call(rbind,dd_day[sc$idx])',sc$numCol[1])))
  eval(parse(text = sprintf('dd_week%d <<- do.call(rbind,dd_week[sc$idx])',sc$numCol[1])))
  eval(parse(text = sprintf('dd_month%d <<- do.call(rbind,dd_month[sc$idx])',sc$numCol[1])))
  return(0)
})


str <- '117';day <- subset(sta_day,svr_id == str);sid <- subset(sta_svrid,svr_id == str);f <- subset(failRecord,svr_id == str);
day$date <- as.POSIXct(as.character(day$date),tz = 'UTC',format = '%Y%m%d');
ggplot(day,aes(x = date,y = numAttrid,group = 1)) + geom_line() + geom_point()
