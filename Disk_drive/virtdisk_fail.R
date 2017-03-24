#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_virtdisk_ym.R
#
# Description: Generate virt disk for ym's IO data including IOPS
# 1.extract pairs of (sid,did) from IO data
# 2.add use_time for each disk
# 3.add failed time for each disk according to locate_fail and add replaced disk info
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-13 15:07:12
#
# Last   modified: 2017-01-17 09:09:41
#
#
#

rm(list = ls())
source('head.R')
source('iopsFunc.R')
dir_iops <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid'
dir_ten <- '/home/yiyusheng/Data/tencProcess'
fname <- list.files(dir_iops)
# fname <- 'd1.Rda'
# load(file.path(dir_data,'iops.Rda'))

# S1.extrac sid and did based on iops.(We focus on iops. if the disk do not have iops, we ignore it.)
# nbrpsid <- 36810:36833
# nbwpsid <- 36834:36857
# 
# extract_id <- function(fn){
#   load(file.path(dir_iops,fn))
#   sdSet <- by(data[,c('svr_id','attrid')],data$svr_id,function(df){
#     x <- df$attrid
#     ioid <- setdiff(sort(unique(x)),c(902,903,999))
#     rid <- ioid[ioid %in% nbrpsid] - 36810 + 1
#     wid <- ioid[ioid %in% nbwpsid] - 36834 + 1
#     diskid <- union(rid,wid)
#     return(data.frame(sid = df$svr_id[1],did = diskid))
#   })
#   sdSet <- do.call(rbind,sdSet)
# }
# 
# require(doParallel)
# idx <- seq_len(length(fname))
# numCore <- floor(detectCores()*0.9)
# ck <- makeCluster(min(numCore,length(fname)),outfile = 'sds',type = 'FORK')
# registerDoParallel(ck)
# r <- foreach(i = idx,.verbose = F) %dopar% extract_id(fname[i])
# stopCluster(ck)
# sdSet <- do.call(rbind,r)
# row.names(sdSet) <- NULL
# save(sdSet,file = file.path(dir_data,'sdSet.Rda'))
load(file.path(dir_data,'sdSet.Rda'))
sdSet$sid <- factor(as.numeric(gsub('s','',sdSet$sid)))
sdSet$did <- as.numeric(gsub('d','',sdSet$did))
sdSet$did[is.na(sdSet$did)] <- 0

# S2. add use_time for each disk
load(file.path(dir_ten,'diskInfo0902.Rda'))
sdSet$use_time <- svrInfo0902$use_time[match(sdSet$sid,svrInfo0902$svr_id)]
sdSet <- subset(sdSet,!is.na(use_time))
sdSet$status <- 'working'
sdSet$f_time <- as.p('2015-07-01')
sdSet <- factorX(sdSet)

# S3.add failed time. similar to sc16F1Func.R::virt_disk
load(file.path(dir_ten,'failRecord_1407-1506.Rda'))
fr <- failRecord[,c('svr_id','f_time')]
names(fr) <- c('svrid','f_time')
fr <- factorX(fr)

frSplit <- split(fr,fr$svrid)
sdSplit <- split(sdSet,sdSet$sid)
interSplit <- intersect(names(frSplit),names(sdSplit))
frSplit <- frSplit[names(frSplit) %in% interSplit]
sdSplit <- sdSplit[names(sdSplit) %in% interSplit]

r <- lapply(names(frSplit),function(s){
  f <- frSplit[[s]]
  c <- sdSplit[[s]]
  
  len.f <- nrow(f)
  len.c <- nrow(c)
  cat(sprintf('%s\n',s))

  # append new item after the last disk id
  # copy len.f items of the last item of c[len.c]
  # use_time means the start use time after inserting a new disk, then it equals to the time fails
  tmp <- c[rep(len.c,len.f),]
  tmp$did <- tmp$did + 1:len.f
  tmp$use_time <- f$f_time
  c <- rbind(c,tmp)
  
  # modify status to failed for disks with low diskid
  c$status[1:len.f] <- 'failed'
  c$f_time[1:len.f] <- f$f_time
  
  return(c)
})
virtDisk <- do.call(rbind,r)

virtDisk <- virtDisk[order(virtDisk$sid,virtDisk$did),]
row.names(virtDisk) <- NULL
save(virtDisk,file = file.path(dir_data,'virtDisk.Rda'))
