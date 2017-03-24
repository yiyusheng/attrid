#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: tagDisk_analize.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-20 17:56:07
#
# Last   modified: 2016-09-20 17:56:10
rm(list = ls())
source('head.R')
library(ggplot2)
load(file.path(dir_data,'dataPrepareAFR13.Rda'))

####################################
# S1. statistic number of servers using in different time for all model
splitTime <- split(tmp.cmdb,tmp.cmdb$tagDisk)

staTime <- lapply(splitTime,function(x){
  df <- melt(table(cut.POSIXt(x$use_time,breaks = 'month')))
  df$class <- x$tagDisk[1]
  df})

staTime <- do.call(rbind,staTime)
names(staTime) <- c('date','numServer','tagDisk')
staTime$date <- as.Date(staTime$date)
staTime$numServer[grepl('B',staTime$tagDisk)] <- staTime$numServer[grepl('B',staTime$tagDisk)]*12

# plot for the two type
pA <- ggplot(subset(staTime,grepl('A',tagDisk)),aes(x = date,y = numServer,fill = tagDisk)) + 
               geom_bar(stat = 'identity')
print(pA)
pB <- ggplot(subset(staTime,grepl('B',tagDisk)),aes(x = date,y = numServer,fill = tagDisk)) + 
  geom_bar(stat = 'identity')
print(pB)

# select tag for same-age comparison
tagA <- 'A-ST3500514NS';tagB <- 'B-ST31000524NS'

# S2. some-age comparison
load(file.path(dir_data,'dataPrepareAFR1306_1307.Rda'))
c13 <- subset(tmp.cmdb,tagDisk %in% c(tagA,tagB))
f13 <- subset(tmp.f,tagDisk %in% c(tagA,tagB))
load(file.path(dir_data,'dataPrepareAFR1302_1303.Rda'))
c14 <- subset(tmp.cmdb,tagDisk %in% c(tagA,tagB))
f14 <- subset(tmp.f,tagDisk %in% c(tagA,tagB))

source(file.path(dir_code,'sc16F1Func.R'))
fr13 <- ioAFR(c13,f13,c('tagDisk','shTime'))
fr13$class <- 'y13'
fr14 <- ioAFR(c14,f14,c('tagDisk','shTime'))
fr14$class <- 'y14'
frAge <- rbind(fr13,fr14)
frAge$AFR <- frAge$AFR*6
frAge <- frAge[order(frAge$tagDisk,frAge$shTime,frAge$class),]
ggplot(subset(frAge,grepl('A',tagDisk)),aes(x = shTime,y = AFR,color = class)) + geom_line()


