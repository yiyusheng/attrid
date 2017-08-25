#!/usr/bin/env python2                                                                      
# -*- coding: utf-8 -*-
# Filename: sc16F6.R
#
# Description: [Duty cycle]Correlate failure rate and utilization in each disk
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

#@@@ Function @@@#
source('attr_function.R')
source('sc16F6Func.R')
load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))
load(file.path(dir_data,'NewSC16','ioFeature.Rda')) #speed + util + Dense特征
load(file.path(dir_data,'NewSC16','ioUtilMax.Rda')) #100%util持续时间

# #@@@ LOAD DATA @@@#
# load(file.path(dir_data,'load_ftr_attridOld.Rda'))
# source(file.path(dir_code,'AFR_io_prepareOld.R'))

#####################################################################################################
# S1. The CDF of max util of server over time
attr <- 'q100Util'
ioF <- factorX(subset(ioFtr,svrid %in% tmp.cmdb$svr_asset_id,c('svrid',attr)))
names(ioF) <- c('svrid','attr')
DT_ftr <- data.frame(svrid = levels(ioF$svrid),maxoU = as.numeric(tapply(ioF$attr,ioF$svrid,max)))

DT_ftr$dClass <- cmdb$dClass[match(DT_ftr$svrid,cmdb$svr_asset_id)]
DT_ftr$dClass[grepl('TS',DT_ftr$dClass)] <- 'Sserv'
DT_ftr$dClass[grepl('C',DT_ftr$dClass)] <- 'Nserv'
DT_ftr$fClass <- 'Normal'
DT_ftr$fClass[DT_ftr$svrid %in% tmp.f$svr_id] <- 'Failed'
DT_ftr$class <- paste(DT_ftr$fClass,DT_ftr$dClass,sep=' ')

p1 <- plot_feature_CDF(DT_ftr);print(p1)
ggsave(file=file.path(dir_data,'sc16','fig6A.eps'), plot=p1, width = 8, height = 6, dpi = 100)
######################################################################################################
# The length of period of 100 util and failure rate
ioU <- factorX(subset(ioUtilMax,svrid %in% tmp.cmdb$svr_asset_id & maxCount > 0))
meanMLC <- tapply(ioU$maxLastCount,ioU$svrid,mean)
maxMLC <- tapply(ioU$maxLastCount,ioU$svrid,max)
maxtopMLC <- tapply(ioU$maxLastCount,ioU$svrid,function(x){x <- sort(x,decreasing = T);mean(x[1:10])}) # mean of top 10

# generate feature
divU1 <- c(-1,0,1,2,3,6,12,60,120,288)
staU1 <- data.frame(svrid = names(meanMLC),
                   meanoMLC = as.numeric(meanMLC),
                   maxoMLC = as.numeric(maxMLC),
                   maxtopoMLC = as.numeric(maxtopMLC),
                   meanMLC = as.numeric(cut(as.numeric(meanMLC),divU1,divU1[-1],include.lowest = T)),
                   maxMLC = as.numeric(cut(as.numeric(maxMLC),divU1,divU1[-1],include.lowest = T)),
                   maxtopMLC = as.numeric(cut(as.numeric(maxtopMLC),divU1,divU1[-1],include.lowest = T)))

# assignment
tf <- tmp.f
tf <- mchAttr(tf,staU1,'svr_id','svrid',c('maxMLC','meanMLC','maxtopMLC'))
tf[,c('maxMLC','meanMLC','maxtopMLC')] <- replace_value(tf[,c('maxMLC','meanMLC','maxtopMLC')])
tcmdb <- tmp.cmdb
tcmdb <- mchAttr(tcmdb,staU1,'svr_asset_id','svrid',c('maxMLC','meanMLC','maxtopMLC'))
tcmdb[,c('maxMLC','meanMLC','maxtopMLC')] <- replace_value(tcmdb[,c('maxMLC','meanMLC','maxtopMLC')])

# group
tfC <- subset(tf,grepl('C',dClass))
tfTS <- subset(tf,grepl('TS',dClass))
tcmdbC <- subset(tcmdb,grepl('C',dClass))
tcmdbTS <- subset(tcmdb,grepl('TS',dClass))

# plot
attrNeed <- 'maxMLC'
pC <- ioAFR(tcmdbC,tfC,attrNeed)
pC$class <- 'Nserv'
pTS <- ioAFR(tcmdbTS,tfTS,attrNeed,diskCount = 12)
pTS$class <- 'Sserv'

pC$AFR <- pC$AFR/2.26*1.48
pTS$AFR <- pTS$AFR/1.96*1.61

Dura <- rbind(item_order(pC,attrNeed),item_order(pTS,attrNeed))
Dura[[attrNeed]] <- fct2num(Dura[[attrNeed]])*5
AFR_plot(DT = Dura,title = 'fig6B',
         ylimL = 0,ylimR = 30,
         para_x = attrNeed,para_y = 'AFR',para_fill = 'class',
         para_xlab = 'Duration of full workload (minutes)',
         para_ylab = 'Annual Failure Rate (%)')
