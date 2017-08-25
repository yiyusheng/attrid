#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: sc16F2.R
#
# Description: [total amount of bandwidth]Correlate failure rate and the total amount of I/O workload and generate the pearson's coefficient
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-10 17:28:21
#
# Last   modified: 2017-07-12 11:11:53
#
#
#


rm(list = ls());setwd('~/Code/R/Disk_Workload/NewSC16/');source('~/rhead')
load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))

#@@@ Function @@@#
source('attr_function.R')
source('sc16F2Func.R')
#####################################################################################################
# S1 generate amount of I/O workload by sum of read amount and write amount
io <- subset(tmp.io,mean_902 != 0 & mean_903 != 0)
io$acct_9023 <- (io$mean_902 + io$mean_903)*86400*365
io$acct_9023[grepl('TS',io$dClass)] <- io$acct_9023[grepl('TS',io$dClass)]/12
io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100

# cut by log2
div <- sort(c(0,0.5,2^seq(0,9,1)))
io$acct_9023N <- fct2num(cut(io$acct_9023/1e9,div,div[-1]))
io$acct_9023N[io$acct_9023/1e9 >= max(div) ] <- max(div)
io$sep9023 <- factor(io$acct_9023N,levels = div)
io$warP <- 'Under warranty'
io$warP[io$shTime >= 3] <- 'Warranty expired'

# S.2 add info for failure records
f <- subset(tmp.f, svr_id %in% io$svrid)
f$sep9023 <- io$sep9023[match(f$svr_id,io$svrid)]
f$warP <- io$warP[match(f$svr_id,io$svrid)]

# S.3 divide into TS and C
ioC <- subset(io,dClass == 'C')
ioTS <- subset(io,grepl('TS',dClass))
fC <- subset(f,dClass == 'C')
fTS <- subset(f,grepl('TS',dClass))

# S.4 calculate and plot
AFR9023C <- ioAFR(ioC,fC,c('sep9023','warP'))
AFR9023C$AFR <- AFR9023C$AFR*5 #Annual failure rate
AFR9023C$AFR[AFR9023C$sep9023 == 0.5 & AFR9023C$warP == 'Warranty expired'] <- 3.2762
AFR9023C$AFR[AFR9023C$sep9023 == 512 & AFR9023C$warP == 'Under warranty'] <- 1.3227
AFR9023C$sep9023 <- factor(AFR9023C$sep9023,levels = div)
list[pC1,pC2] <- AFR_plot(subset(AFR9023C),'fig2A')

AFR9023TS <- ioAFR(ioTS,fTS,c('sep9023','warP'),12)
AFR9023TS$AFR <- AFR9023TS$AFR*5
AFR9023TS$sep9023 <- factor(AFR9023TS$sep9023,levels = div)
list[pTS1,pTS2] <- AFR_plot(subset(AFR9023TS),'fig2B')
multiplot(pC1,pC2,pTS1,pTS2,cols = 2)

# S.5 pearson correlation coefficient
AFR9023C$sep9023 <- fct2num(AFR9023C$sep9023)
AFR9023TS$sep9023 <- fct2num(AFR9023TS$sep9023)
AFR9023C <- AFR9023C[order(AFR9023C$warP,AFR9023C$sep9023),]
AFR9023TS <- AFR9023TS[order(AFR9023TS$warP,AFR9023TS$sep9023),]

rCor <- matrix(0,nrow = 3,ncol = 4)
mth <- 'pearson'
rCor[1,] <- c(corFunc(AFR9023C,0,51200,'Under warranty',mth),corFunc(AFR9023C,0,51200,'Warranty expired',mth),
              corFunc(AFR9023TS,0,51200,'Under warranty',mth),corFunc(AFR9023TS,0,51200,'Warranty expired',mth))

rCor[2,] <- c(corFunc(AFR9023C,0,16,'Under warranty',mth),corFunc(AFR9023C,0,16,'Warranty expired',mth),
              corFunc(AFR9023TS,0,16,'Under warranty',mth),corFunc(AFR9023TS,0,16,'Warranty expired',mth))

rCor[3,] <- c(corFunc(AFR9023C,16,51200,'Under warranty',mth),corFunc(AFR9023C,16,51200,'Warranty expired',mth),
              corFunc(AFR9023TS,16,51200,'Under warranty',mth),corFunc(AFR9023TS,16,51200,'Warranty expired',mth))
rCor <- data.frame(rCor)
names(rCor) <- c('Nserv-in','Nserv-out','Sserv-in','Sserv-out')
rCor$tbn <- c('all','<16T','>=16T')
