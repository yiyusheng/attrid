#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: sc16F4.R
#
# Description: [vibration]Correlate failure rate and vibration of io in each disk
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
load(file.path(dir_data,'NewSC16','ioFluc9023Simp.Rda'))

#@@@ Function @@@#
# source('attr_function.R')
# source('AFR_io_function.R')
# source('sc16F4Func.R')
#source('AFR_io_prepareOld.R')
# load(file.path(dir_data,'load_ftr_attridOld.Rda'))

#####################################################################################################
# S1. For cmdbio
cmdbio <- tmp.io #new line
cmdbio$dClassN <- 'Sserv'
cmdbio$dClassN[grepl('C',cmdbio$dClass)] <- 'Nserv'
cmdbio$warP <- 'Under warranty'
cmdbio$warP[cmdbio$shTime >= 3] <- 'Warranty expired'

# S2. For f
f <- tmp.f
f <- mchAttr(f,cmdbio,'svr_id','svrid',c('dClassN','warP'))

# For vibration of IO
ioFluc <- ioFluc9023
ioFluc$cv9023[is.na(ioFluc$cv9023)] <- 0
ioFluc$cv9023 <- abs(ioFluc$cv902)
ioFluc <- subset(ioFluc,svrid %in% cmdbio$svrid)
ioFluc <-factorX(ioFluc)

# S3. Statistic of vibration
meCV <- tapply(ioFluc$cv9023,ioFluc$svrid,mean)
maxCV <- tapply(ioFluc$cv9023,ioFluc$svrid,max)
topmaxCV <- tapply(ioFluc$cv9023,ioFluc$svrid,function(x){
  x <- sort(x,decreasing = T)
  mean(x[1:10])
})

disp <- data.frame(svrid = names(meCV),
                   meanCV = as.numeric(meCV),
                   maxCV = as.numeric(maxCV),
                   topmaxCV = as.numeric(topmaxCV))

# S4. Cut maxCV
divCV <- c(seq(0,5,0.5),max(disp$maxCV,na.rm = T))
disp$maxCVd <- fct2num(cut(disp$maxCV,divCV,divCV[-1],include.lowest = T))

# S5. get attributes from disp
io <- mchAttr(cmdbio,disp,'svrid','svrid',c('meanCV','maxCV','topmaxCV','maxCVd'))
f <- mchAttr(f,disp,'svr_id','svrid',c('meanCV','maxCV','topmaxCV','maxCVd'))
io$classA <- paste(io$dClassN,'(',io$warP,')',sep='')

# S6. group
ioC <- subset(io,dClassN == 'Nserv')
ioTS <- subset(io,dClassN == 'Sserv')
fC <- subset(f,dClassN == 'Nserv')
fTS <- subset(f,dClassN == 'Sserv')

# S7. plot CDF of maxCV
# p1 <- ggplot(io,aes(x = maxCV,color = classA)) + stat_ecdf(size = 1) + xlim(c(0,17))
# print(p1)
# ggsave(file=file.path(dir_data,'sc16','fig4A.eps'), plot=p1, width = 8, height = 6, dpi = 100)

# S8. Plot AFR
AFRflucTS <- ioAFR(ioTS,fTS,c('maxCVd'),12)
AFRflucC <- ioAFR(ioC,fC,c('maxCVd'))
AFRflucC$class <- 'Nserv'
AFRflucTS$class <- 'Sserv'
AFR_plot(DT = rbind(AFRflucC,AFRflucTS),title = 'fig4D')

AFRfluc = subset(rbind(AFRflucC,AFRflucTS),!is.infinite(maxCVd))
AFR_plot(DT = AFRfluc,title = 'fig4D',ylimL = 0,ylimR = 6,
         para_x = 'maxCVd',para_y = 'AFR',para_fill = 'class',
         para_xlab = 'Coefficient of Variable',para_ylab = 'Annual Failure Rate (%)')
# pC <- AFR_plot(AFRflucC,'fig4B')
# pTS <- AFR_plot(AFRflucTS,'fig4C')
