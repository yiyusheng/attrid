#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: feature_strength_of_periodicity.R
#
# Description: We divide a day into idle period and busy period . 
# sumarize attributes in each period and genarate their difference DIF.
# Then, we use the cv of DIF to evalute the strength of periodicity. Large cv means large vibration of difference of idle and busy period
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-10 17:28:21
#
# Last   modified: 2017-07-12 15:55:03
#
#
#


rm(list = ls());setwd('~/Code/R/Disk_Workload/NewSC16/');source('~/rhead')
source('attr_function.R')
source('AFR_io_function.R')

load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))

#@@@ LOAD DATA @@@#
# load(file.path(dir_data,'load_ftr_attrid.Rda'))
# source(file.path(dir_code,'AFR_io_prepare.R'))
# load(file.path(dir_data,'freqFieldWatch200.Rda'))

#########################################################################################################
# S1. Load diff of dividing. three different divides point are used.
load(file.path(dir_data,'NewSC16','ioDura1.Rda'));ioDura1 <- ioDura
load(file.path(dir_data,'NewSC16','ioDura2.Rda'));ioDura2 <- ioDura
load(file.path(dir_data,'NewSC16','ioDura3.Rda'));ioDura3 <- ioDura

names(ioDura1)[12:15] <- c('diff902A','diff903A','diff999A','diff9023A')
names(ioDura2)[12:15] <- c('diff902B','diff903B','diff999B','diff9023B')
names(ioDura3)[12:15] <- c('diff902C','diff903C','diff999C','diff9023C')
ioDura <- cbind(ioDura1[,c('svrid','time','count')],ioDura1[,12:15],ioDura2[,12:15],ioDura3[,12:15])
rm(ioDura1);rm(ioDura2);rm(ioDura3)

# S2. data merge and preparation
cmdbio <- tmp.io
ioDura <- factorX(subset(ioDura,count == 288 &
                           !is.na(diff902A) & !is.na(diff903A) & !is.na(diff9023A) & !is.na(diff999A) &
                           svrid %in% cmdbio$svrid))
# log them before divide
ioDura$diff902Al <- log4neg(ioDura$diff902A)
ioDura$diff903Al <- log4neg(ioDura$diff903A)
ioDura$diff9023Al <- log4neg(ioDura$diff9023A)
ioDura$diff999Al <- log4neg(ioDura$diff999A)

ioDura$diff902Bl <- log4neg(ioDura$diff902B)
ioDura$diff903Bl <- log4neg(ioDura$diff903B)
ioDura$diff9023Bl <- log4neg(ioDura$diff9023B)
ioDura$diff999Bl <- log4neg(ioDura$diff999B)

ioDura$diff902Cl <- log4neg(ioDura$diff902C)
ioDura$diff903Cl <- log4neg(ioDura$diff903C)
ioDura$diff9023Cl <- log4neg(ioDura$diff9023C)
ioDura$diff999Cl <- log4neg(ioDura$diff999C)

# S3. genertate the CV of diff for util
cvD999A <- tapply(ioDura$diff999Al,ioDura$svrid,function(x)sd(x)/mean(x))
cvD999B <- tapply(ioDura$diff999Bl,ioDura$svrid,function(x)sd(x)/mean(x))
cvD999C <- tapply(ioDura$diff999Cl,ioDura$svrid,function(x)sd(x)/mean(x))
dura999 <- data.frame(svrid = names(cvD999A),
                      cvDA = as.numeric(cvD999A),
                      cvDB = as.numeric(cvD999B),
                      cvDC = as.numeric(cvD999C))
dura999$cv <- (abs(dura999$cvDA) + abs(dura999$cvDB) + abs(dura999$cvDC))/3

# S4. genertate the CV of diff for xps
cvD9023A <- tapply(ioDura$diff9023Al,ioDura$svrid,function(x)sd(x)/mean(x))
cvD9023B <- tapply(ioDura$diff9023Bl,ioDura$svrid,function(x)sd(x)/mean(x))
cvD9023C <- tapply(ioDura$diff9023Cl,ioDura$svrid,function(x)sd(x)/mean(x))
dura9023 <- data.frame(svrid = names(cvD9023A),
                      cvDA = as.numeric(cvD9023A),
                      cvDB = as.numeric(cvD9023B),
                      cvDC = as.numeric(cvD9023C))
dura9023$cv <- (abs(dura9023$cvDA) + abs(dura9023$cvDB) + abs(dura9023$cvDC))/3

# S5. save
save(dura999,dura9023,file = file.path(dir_data,'NewSC16','feature_strength_of_periodicity.Rda'))
