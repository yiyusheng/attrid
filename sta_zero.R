#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_zero.R
#
# Description: statistic percentage of idle for each attributes
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-15 17:26:49
#
# Last   modified: 2017-03-15 17:26:51
#
#
#

rm(list = ls());source('head.R');source('sta_zeroFunc.R')

###### MAIN: GENERATE FRACTION OF ZERO WHEN SOME ATTRS ARE ZERO ######
fname <- list.files(dir_datatendcastClear)
fname <- fname[!grepl('a\\d.*',fname)]
require(doParallel)
idx <- seq_len(length(fname))
outname <- 'out_sz';if(file.exists(outname))file.remove('out_sz')
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_sz')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% tryCatch(sta_eachzero(i),error = function(e){cat(sprintf("ERROR:%s\t%s\n",fname[i],e))})
stopCluster(ck)

attrName <- c('util','rps','iopsr','wps','iopsw')
sta_ez <- do.call(rbind,r)
sta_ez_valid <- subset(sta_ez, count > 0)
sta_ez_aggre <- aggregate(sta_ez_valid[,-c(1:6)],sta_ez_valid[,1:5],sum)
sta_ez_rate <- sta_ez_aggre
sta_ez_rate[,paste(attrName,'z',sep='')] <- roundX(sta_ez_rate[,paste(attrName,'z',sep='')]/sta_ez_rate$count)
sta_ez_rate$countPersid <- sta_ez_rate$count/sta_ez_rate$countsid
sta_ez_rate$class <- rowSums(sta_ez_rate[,1:5])
save(sta_ez,sta_ez_aggre,sta_ez_rate,file = file.path(dir_data,'sta_zero.Rda'))

###### ANALYSIS ######
# sta_nz_melt <- melt(sta_nz,id.vars = 'svrid')
# names(sta_nz_melt) <- c('svrid','attrid','nonzero_frac')
# ggplot(sta_nz_melt,aes(x = attrid,y = nonzero_frac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2,alpha = 0.01)
# ggplot(sta_nz,aes(x = rps,y = wps)) + geom_point(alpha = 0.1)
# 
# dd <- dt_dcast[,c('svrid','time','util','rps','iopsr','wps','iopsw')]
# dd1 <- factorX(subset(dd,util == 0 &wps == 0))
# dd1$rqsizew <- dd1$wps/dd1$iopsw
# splitDD <- split(dt_dcast,dt_dcast$svrid)
