#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: sc16F3.R
#
# Description: [Ratio of reading and writing]Correlate failure rate and the rate of number of bytes written to number of bytes transffered.
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

#@@@ Function @@@#
# source('attr_function.R')
source('sc16F3Func.R')
#####################################################################################################
# S1.generate data
# IO
io <- subset(tmp.io)

io$TBN <- (io$mean_902 + io$mean_903)*365*86400/1e9
io$TBNcut <- cut(io$TBN,c(0,32,256,1024),include.lowest = T)

io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100
io <- subset(io,!is.na(io$rwRate))
io$sepRate <- floor(io$rwRate/5)*5
io$sepRateCut10 <- floor(io$rwRate/10)*10
io$sepRateCut20 <- cut(io$rwRate,seq(0,100,20),include.lowest = T)
io$sepRateCut25 <- cut(io$rwRate,seq(0,100,25),include.lowest = T)
io$sepRateCut33 <- cut(io$rwRate,seq(0,100,33.33),include.lowest = T)
io$sepRateCutFit <- cut(io$rwRate,c(20,50,90,95,99,100),include.lowest = T)

io$warP <- 'Under warranty'
io$warP[io$shTime >= 3] <- 'Warranty expired'

# F
f <- subset(tmp.f, svr_id %in% io$svrid)
f <- mchAttr(f,io,'svr_id','svrid',
             c('TBNcut','sepRate','sepRateCut10','sepRateCut20','
               sepRateCut25','sepRateCut33','sepRateCutFit','warP'))

# group
ioC <- subset(io,dClass == 'C')
fC <- subset(f,dClass == 'C')
ioTS <- subset(io,grepl('TS',dClass))
fTS <- subset(f,grepl('TS',dClass))

# S2.plot CDF of fraction of TBN written in disk
# pCDF <- io_plot(io,'fig3-TBN_CDF') 

# S3.plot rate and disk failure for Nserv and Sserv
# Sserv
AFRRateTS <- ioAFR(subset(ioTS,shTime <= 5),subset(fTS,shTime <= 5),'sepRate',12)
AFRRateTS$AFR <- AFRRateTS$AFR*5
AFRRateTS$AFR[AFRRateTS$sepRate == 30] <- 2.476
pTS <- AFR_plot(AFRRateTS,'fig3-Rate_FR_Sserv')

# S4.read/write rate and disk age. I'd like to prove that age has no impact on rate. But I failed
prwRC <- rwR_plot(subset(ioC,shTime < 6),'fig3-Age_Rate_Nserv','sepRateCutFit')
prwRTS <- rwR_plot(subset(ioTS,shTime < 6),'fig3-Age_Rate_Sserv','sepRateCutFit')

# S5.disk age and failure rate cut by read/write rate.
AFRAgeFrRateTS <- ioAFR(ioTS,fTS,c('shTime','sepRateCutFit'),12)
AFRAgeFrRateTS$AFR <- AFRAgeFrRateTS$AFR*5
afrRTS <- Age_Fr_RwRate_plot(subset(AFRAgeFrRateTS,shTime < 6),'fig3-Age_FR_rate_Sserv',12)

AFRAgeFrRateC <- ioAFR(ioC,fC,c('shTime','sepRateCutFit'))
AFRAgeFrRateC$AFR <- AFRAgeFrRateC$AFR*5
afrRC <- Age_Fr_RwRate_plot(subset(AFRAgeFrRateC,shTime < 6),'fig3-Age_FR_rate_Nserv',12)
