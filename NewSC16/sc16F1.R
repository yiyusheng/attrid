#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: sc16F1.R
#
# Description: [2014] data process and figure plot for sc16. there is a function file sc16F1Func.R
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-10 17:28:21
#
# Last   modified: 2017-07-12 10:15:23
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/NewSC16/');source('~/rhead')

#@@@ Function @@@#
source('attr_function.R')
source('sc16F1Func.R')

#####################################################################################################
list[tmp.fC,tmp.cmdbC,tmp.fTS,tmp.cmdbTS,tmp.f,tmp.cmdb] <- dataLoadF1('dataPrepareAFR13.Rda')

# S1. compute the failure rate
at <- 'shTimeQu'

cmC <- ioAFR(tmp.cmdbC,tmp.fC,attr = at,diskCount = 1)
cmC$class <- 'Nserv'
cmC$AFR[cmC$shTimeQu == 5] <- 4.13443

cmTS <- ioAFR(tmp.cmdbTS,tmp.fTS,at,12)
cmTS$class <- 'Sserv'

cmAgeFr <- rbind(cmC,cmTS)
cmAgeFr$shTimeQu[cmAgeFr$shTimeQu == 5] <- 4.25
p1 <- AFR_plot(cmAgeFr,'fig1','shTimeQu')

# S2. compute the failure rate of warranty effect
at <- 'shTimeQuGuassian'

cm1B <- AFR_attr_notime(tmp.f,tmp.cmdb,attr = at,diskCount = 1,dev = 'C')
cm1B$AFR[cm1B$item == 5] <- 4.13443
x1 <- cm1B$item[1:12];y1 <- cm1B$AFR[1:12]
cm1B$AFRpredict <- predict(lm(y1~x1),data.frame(x1 = cm1B$item))
cm1B$AFRdiff <- cm1B$AFR - cm1B$AFRpredict

cm2B <- AFR_attr_notime(tmp.f,tmp.cmdb,attr = at,diskCount = 12,dev = 'TS')
x2 <- cm2B$item[1:12];y2 <- cm2B$AFR[1:12]
cm2B$AFRpredict <- predict(lm(y2~x2),data.frame(x2 = cm2B$item))
cm2B$AFRdiff <- cm2B$AFR - cm2B$AFRpredict

cmB <- rbind(cm1B,cm2B)
cmB <- factorX(subset(cmB,!is.na(AFR)))
cmB <- classExchg(cmB)
cmB <- subset(cmB,item >=2 & item <= 4)

p2 <- AFR_plot_warranty(cmB,'fig1Warranty')
