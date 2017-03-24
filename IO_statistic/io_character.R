#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plot_util_xps_iops.R
#
# Description: plot util-xps and util-iops to anylize
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-02-13 11:04:22

rm(list = ls())
source('head.R')
dir_dataten <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid/'
dir_datatendcast <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcast/'

# origin data character
load(file.path(dir_datatendcast,'d1test.Rda'))
dd <- dt_dcast[,1:4]
dd$iopsr <- rowSums(dt_dcast[grepl('iopsr',names(dt_dcast))])
dd$iopsw <- rowSums(dt_dcast[grepl('iopsw',names(dt_dcast))])
dd$rater <- dd$rps/dd$iopsr
dd$ratew <- dd$wps/dd$iopsw
dd$rater[dd$rater < 0] <- 0
dd$ratew[dd$ratew < 0] <- 0
dd$date <- as.Date(dd$time)

dt_dcast$date <- as.Date(dt_dcast$time)
dd_day <- by(dt_dcast,list(dt_dcast$svrid,dt_dcast$date),function(x){
  tmp <- x[1,]
  tmp[grepl('iopr',names(tmp))] <- rowSums(x[grepl('iopr',names(x))],na.rm = T)
})

# F1. single disk bear most of I/O request
dt_dcast[dt_dcast == -1] <- NA
splitDD <- split(dt_dcast,dt_dcast$svrid)
f1r <- data.frame(t(sapply(splitDD,function(x){
  tmp <- colSums(x[,grepl('iopsr',names(dt_dcast))],na.rm = T)
  round(tmp/sum(tmp)*100,digits = 4)})))
f1w <- data.frame(t(sapply(splitDD,function(x){
  tmp <- colSums(x[,grepl('iopsw',names(dt_dcast))],na.rm = T)
  round(tmp/sum(tmp)*100,digits = 4)})))

# F2.
r <- aggregate(dd$rater,by = list(dd$svrid,dd$date),mean)
r <- subset(r,!is.na(r$x))
names(r) <- c('svrid','date','mean')
r1 <- t(dcast(svrid~date,data = r,value.var = 'mean'))

splitdd <- split(dd,dd$svrid)
a <- subset(splitdd[[1]],time > as.p('2014-07-01') & time <= as.p('2014-07-02'))
ggplot(a,aes(x = time,y = rater)) + geom_line()

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
dd_month2$sizer <- round(dd_month2$rps/(dd_month2$iopsr_1 + dd_month2$iopsr_2),digits = 4)
dd_month2$sizew <- round(dd_month2$wps/(dd_month2$iopsw_1 + dd_month2$iopsw_2),digits = 4)

# Analysis1
sta_need <- factorX(subset(sta_svrid,grepl('[b|c|d|e].*\\.Rda',fn) & count_day > 300))

d2 <- factorX(subset(dd_month2,svrid %in% sta_need$svr_id))
d2$sizer[is.infinite(d2$sizer) | d2$sizer > 20] <- NA
d2$sizew[is.infinite(d2$sizew) | d2$sizew > 20] <- NA

# 3D plot
d2_ur <- data.frame(table(cut(d2$util,20),cut(d2$sizer,20)))
ggplot(d2_ur,aes(Var1,Var2)) + geom_raster(aes(fill = log2(Freq)))

# 2D plotAR
cut_util2 <- c(0,1.5,10,100)
cut_sizer2 <- c(seq(0,2,0.04),20)
d2$util_level <- fct2num(cut(d2$util,breaks = cut_util2,labels = cut_util2[-length(cut_util2)],include.lowest = F))
d2$sizer_level <- fct2num(cut(d2$sizer,breaks = cut_sizer2,labels = cut_sizer2[-length(cut_sizer2)],include.lowest = F))

d2r <- tapply(d2$sizer_level,d2$util_level,function(x){r1 <- melt(table(x));r1$value <- r1$value/sum(r1$value);r1})
d2r[[1]]$class <- 'x0';d2r[[2]]$class <- 'x1.5';d2r[[3]]$class <- 'x10'
d2r <- do.call(rbind,d2r)
names(d2r) <- c('sizer_level','count','util_level')
ggplot(d2r,aes(x = sizer_level,y = count,group = util_level,color = util_level)) + geom_line() + geom_point()

# 2D plotAW
cut_util2 <- c(0,1.5,10,100)
cut_sizew2 <- c(seq(0,2,0.04),20)
d2$util_level <- fct2num(cut(d2$util,breaks = cut_util2,labels = cut_util2[-length(cut_util2)],include.lowest = F))
d2$sizew_level <- fct2num(cut(d2$sizew,breaks = cut_sizew2,labels = cut_sizew2[-length(cut_sizew2)],include.lowest = F))

d2r <- tapply(d2$sizew_level,d2$util_level,function(x){r1 <- melt(table(x));r1$value <- r1$value/sum(r1$value);r1})
d2r[[1]]$class <- 'x0';d2r[[2]]$class <- 'x1.5';d2r[[3]]$class <- 'x10'
d2r <- do.call(rbind,d2r)
names(d2r) <- c('sizew_level','count','util_level')
ggplot(d2r,aes(x = sizew_level,y = count,group = util_level,color = util_level)) + geom_line() + geom_point()

# 2D plotB
cut_util2 <- exp(seq(-15,5,1))
cut_sizer2 <- c(0,1,2,20)
d2$util_level <- fct2num(cut(d2$util,breaks = cut_util2,labels = cut_util2[-length(cut_util2)],include.lowest = F))
d2$sizer_level <- fct2num(cut(d2$sizer,breaks = cut_sizer2,labels = cut_sizer2[-length(cut_sizer2)],include.lowest = F))

d2r <- tapply(d2$util_level,d2$sizer_level,function(x){r1 <- melt(table(x));r1$value <- r1$value/sum(r1$value);r1})
d2r[[1]]$class <- 'x0';d2r[[2]]$class <- 'x1';d2r[[3]]$class <- 'x2'
d2r <- do.call(rbind,d2r)
names(d2r) <- c('util_level','count','sizer_level')
ggplot(d2r,aes(x = log(util_level),y = count,group = sizer_level,color = sizer_level)) + geom_line() + geom_point()

# Analysis2
str <- '122';day <- subset(sta_day,svr_id == str);sid <- subset(sta_svrid,svr_id == str);f <- subset(failRecord,svr_id == str);
day$date <- as.POSIXct(as.character(day$date),tz = 'UTC',format = '%Y%m%d');
ggplot(day,aes(x = date,y = numAttrid,group = 1)) + geom_line() + geom_point()