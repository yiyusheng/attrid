#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: checkWindow.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-21 10:52:38
#
# Last   modified: 2016-09-21 10:52:40

rm(list = ls())
source('head.R')
source(file.path(dir_code,'sc16F1Func.R'))
library(ggplot2)
library(plyr)

####################################
# S1. plot age - AFR for different window of failure record.
winFile <- list.files(dir_data)
tFlag <- 3

if(tFlag == 1){
  # For one year
  winFile <- c('dataPrepareAFR13.Rda','dataPrepareAFR14.Rda')
}else if(tFlag ==2){
  # For two month
  winFile <- winFile[grepl('dataPrepare.*_.*Rda',winFile)]
  winFile <- winFile[!grepl('dataPrepareAFR1406_1407',winFile)]
}else if(tFlag == 3){
  # For half year
  winFile <- winFile[grepl('[A|B]\\.Rda',winFile)]
}

# tag
winBound <- gsub('dataPrepareAFR|\\.Rda','',winFile)

frCalculate <- function(wf){
  wfBound <- gsub('dataPrepareAFR|\\.Rda','',wf)
  list[tmp.fC,tmp.cmdbC,tmp.fTS,tmp.cmdbTS] <- dataLoadF1(wf)
  
  staC <- ioAFR(tmp.cmdbC,tmp.fC,'shTimeQu')
  staC$class <- paste('C',wfBound,sep='')
  
  staTS <- ioAFR(tmp.cmdbTS,tmp.fTS,'shTimeQu',12)
  staTS$class <- paste('TS',wfBound,sep='')
  
  rbind(staC,staTS)
}

r <- lapply(winFile,frCalculate)
r <- do.call(rbind,r)
r$group <- gsub('[0-9]|_','',r$class)
rC <- r[grepl('C',r$group) & r$count > 100,]
rTS <- r[grepl('TS',r$group)& r$count > 10,]

ggplot(rC,aes(x = shTimeQu,y = AFR,color = class)) + geom_line()
ggplot(rTS,aes(x = shTimeQu,y = AFR,color = class)) + geom_line()

# S2. We find 177 + 476 + 109 TS servers. They have high failure rate in each windows.
# We extract them and summary their features.
list[tmp.fC,tmp.cmdbC,tmp.fTS,tmp.cmdbTS] <- dataLoadF1('dataPrepareAFR13A.Rda')

sel.cmdbTS <- subset(tmp.cmdbTS,shTimeQu >= 3.0 & shTimeQu <= 3.5)
nosel.cmdbTS <- subset(tmp.cmdbTS,shTimeQu < 3.0 | shTimeQu > 3.5)
sel.fTS <- subset(tmp.fTS,shTimeQu >= 3.0 & shTimeQu <= 3.5)
nosel.fTS <- subset(tmp.fTS,shTimeQu < 3.0 | shTimeQu > 3.5)

sel.cmdbC <- subset(tmp.cmdbC,shTimeQu >= 3.0 & shTimeQu <= 3.75)
nosel.cmdbC <- subset(tmp.cmdbC,shTimeQu < 3.0 | shTimeQu > 3.75)
sel.fC <- subset(tmp.fC,shTimeQu >= 3.0 & shTimeQu <= 3.75)
nosel.fC <- subset(tmp.fC,shTimeQu < 3.0 | shTimeQu > 3.75)

frAttr <- function(attr){
  sel.frC <- ioAFR(sel.cmdbC,sel.fC,attr);sel.frC$class <- 'Csel'
  nosel.frC <- ioAFR(nosel.cmdbC,nosel.fC,attr);nosel.frC$class <- 'Cnosel'
  frC <- rbind(sel.frC,nosel.frC)
  sel.frTS <- ioAFR(sel.cmdbTS,sel.fTS,attr,12);sel.frTS$class <- 'TSsel'
  nosel.frTS <- ioAFR(nosel.cmdbTS,nosel.fTS,attr,12);nosel.frTS$class <- 'TSnosel'
  frTS <- rbind(sel.frTS,nosel.frTS)
  rbind(frC,frTS)
}

frModel <- frAttr('mainModel')
frnumModel <- frAttr('numModel')
frtagDisk <- frAttr('tagDisk')

####################################
# S3. select 6 mainmodel and calculate failure rate of them in different age
load(file.path(dir_dataSource,'load_ftr_attrid.Rda'))

# S3.1 Load data(filter data whose numDisk is not 1,11,12,13 and whose mainmodel is not in our 6 mainmodel)
cmdbModel <- mchAttr(cmdb,disk_ip,'svr_asset_id','svr_id',
                     c('numDisk','mainModel','numModel'))

cmdbModel <- subset(cmdbModel,numDisk %in% c(1,11,12,13))
cmdbModel$numDiskA <- cmdbModel$numDisk;
cmdbModel$numDiskA[cmdbModel$numDiskA != 1] <- 12

cmdbModel$year <- format(cmdbModel$use_time,format='%Y')
modelNeed <- c('ST3500514NS','ST31000524NS','ST32000645NS',
               'ST500NM0011','ST1000NM0011','ST2000NM0011')
cmdbModel <- subset(cmdbModel,mainModel %in% modelNeed)

# S3.2 Add attr for flist and filter some
data.flistModel <- mchAttr(data.flist,cmdbModel,'svr_id','svr_asset_id',
                           c('numDiskA','mainModel','numModel','year'))
data.flistModel <- subset(data.flistModel,svr_id %in% cmdbModel$svr_asset_id)

# S3.3 calculate failure rate for each mainModel + numDisk
frModelNumD <- ioAFR(cmdbModel,data.flistModel,c('mainModel','numDiskA','year'))
frModelNumD <- frModelNumD[order(frModelNumD$mainModel,frModelNumD$numDiskA,frModelNumD$year),]
frModelNumD$AFR <- frModelNumD$AFR/frModelNumD$numDiskA
