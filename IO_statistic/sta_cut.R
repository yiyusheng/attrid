#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_cut.R
#
# Description: I observed that wps is aggragate in 16-64 kB/s indicating a possible workload of logging. 
# I wanna know whether that's true for all svrid or just for a range of svrid.
# Because 16 kB/s means 1.38GB/day, it is a large consume of disk capacity.
# Generally, I statistic all five attributes with the same method.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-23 10:48:05
#
# Last   modified: 2017-03-23 11:43:44
#
#
#

rm(list = ls());source('~/rhead')

sta_attrid <- function(dd,aid,fnm){
  level_aid <- paste(aid,'_level',sep='')
  cut_aid <- cutList[[paste('cut_',aid,sep='')]]
  dd[[level_aid]] <- cut_level(dd[[aid]],cut_aid,F)
  
  sta_aid_svrid <- as.data.frame(do.call(rbind,tapply(dd[[level_aid]],dd$svrid,table)))
  sta_aid_svrid <- cbind(levels(dd$svrid),sta_aid_svrid)
  names(sta_aid_svrid) <- c('svrid',paste('X',cut_aid[-length(cut_aid)],sep=''))
  sta_aid_svrid$fn <- fnm
  
  # dd$svridtime <- factor(paste(fct2ori(dd$svrid),dd$time,sep = '#'))
  # sta_aid_day <- as.data.frame(do.call(rbind,tapply(dd[[level_aid]],dd$svridtime,table)))
  # sta_aid_day <- cbind(levels(dd$svridtime),sta_aid_day)
  # names(sta_aid_day) <- c('svriddate',paste('X',cut_aid[-length(cut_aid)],sep=''))
  # 
  # tmp <- strsplit(fct2ori(sta_aid_day$svriddate),split = '#')
  # sta_aid_day$svrid <- factor(sapply(tmp,'[[',1))
  # sta_aid_day$time <- as.Date(sapply(tmp,'[[',2))
  # sta_aid_day <- sta_aid_day[,c('svrid','time',paste('X',cut_aid[-length(cut_aid)],sep=''))]
  # row.names(sta_aid_day) <- NULL
  # sta_aid_day$fn <- fnm
  sta_aid_day <- 0
  
  list(sta_aid_svrid,sta_aid_day)
}

sta_cut <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  dd <- dt_dcast
  # dd <- subsetX(dd,svrid %in% levels(dd$svrid)[1:10])
  dd <- filter_badiops_NA(dd,attrName,fn)
  dd$time <- as.Date(dd$time)
  dd$sizer <- dd$rps/dd$iopsr;dd$sizer[is.na(dd$sizer) | is.infinite(dd$sizer)] <- 0
  dd$sizew <- dd$wps/dd$iopsw;dd$sizew[is.na(dd$sizew) | is.infinite(dd$sizew)] <- 0
  
  
  list[sta_util_svrid,sta_util_day] <- sta_attrid(dd,'util',fn)
  list[sta_rps_svrid,sta_rps_day] <- sta_attrid(dd,'rps',fn)
  list[sta_iopsr_svrid,sta_iopsr_day] <- sta_attrid(dd,'iopsr',fn)
  list[sta_wps_svrid,sta_wps_day] <- sta_attrid(dd,'wps',fn)
  list[sta_iopsw_svrid,sta_iopsw_day] <- sta_attrid(dd,'iopsw',fn)
  
  list[sta_sizer_svrid,sta_sizer_day] <- sta_attrid(dd,'sizer',fn)
  list[sta_sizew_svrid,sta_sizew_day] <- sta_attrid(dd,'sizew',fn)
  
  sta_day <- list(sta_util_day,sta_rps_day,sta_iopsr_day,sta_wps_day,sta_iopsw_day,sta_sizer_day,sta_sizew_day)
  sta_svrid <- list(sta_util_svrid,sta_rps_svrid,sta_iopsr_svrid,sta_wps_svrid,sta_iopsw_svrid,sta_sizer_svrid,sta_sizew_svrid)
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  list(sta_day,sta_svrid)
}

###### MAIN STATISTIC ######
load(file.path(dir_data,'sta_dcastClear.Rda'))
dir_dataset <- dir_data15DC
fname <- list.files(dir_dataset)
# fname <- fname[!grepl('a\\d.*',fname)]
attrName <- attrNameAll
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)),
                cut_rps = c(0,2^(seq(0,16,1)),1e6),cut_iopsr = c(0,2^(seq(0,16,1)),1e6),cut_sizer = roundX(c(0,2^(seq(-6.5,1.5,0.5)),10)),
                cut_wps = c(0,2^(seq(0,16,1)),1e6),cut_iopsw = c(0,2^(seq(0,16,1)),1e6),cut_sizew = roundX(c(0,2^(seq(-6.5,1.5,0.5)),10)))
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_cut')
save(cutList,r,file = file.path(dir_data,'sta_cut.Rda'))

