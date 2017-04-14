#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: duty_time_group.R
#
# Description: we use util to represent duty time and tag each util as real (real workload) or basic (basic workload).
# then we statistic count of util and sum of util for real and basic for each servers.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-12 10:44:32
#
# Last   modified: 2017-04-12 10:44:34
#
#
#

rm(list = ls());source('~/rhead')

gen_util_sta <- function(df){
  tmp_count <- tapply(df$util,df$tag,length);tmp_count[is.na(tmp_count)] <- 0;names(tmp_count) <- paste('count',1:4,sep='')
  tmp_sum <- tapply(df$util,df$tag,sum);tmp_sum[is.na(tmp_sum)] <- 0;names(tmp_sum) <- paste('sum',1:4,sep='')
  r <- data.frame(svrid = df$svrid[1],
             util_count = nrow(df),t(tmp_count),
             util_sum = sum(df$util)/100,t(tmp_sum)/100)
}

duty_time_group <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  dd <- DT
  dd$time <- as.Date(dd$time)
  
  tagSet <- c('idle','busy1','busy2','busy3')
  dd$tag <- tagSet[1]
  dd$tag[dd$rps > 0 & dd$wps <= 64] <- tagSet[2]
  dd$tag[dd$rps == 0 & dd$wps > 64] <- tagSet[3]
  dd$tag[dd$rps > 0 & dd$wps > 64] <- tagSet[4]
  dd$tag <- factor(dd$tag,levels = tagSet)
  splitDD <- split(dd,dd$svrid)
  r <- lapplyX(splitDD,gen_util_sta)
  row.names(r) <- NULL
  r
  
  # sta_svrid <- list2df(tapply(dd$tag,dd$svrid,table),n = tagSet)
  # sta_day <- lapplyX(splitDD,function(df){
  #   r <- list2df(tapply(df$tag,df$time,table),n = c(tagSet,'time'))
  #   r$svrid <- df$svrid[1]
  #   r})
  # row.names(sta_day) <- NULL
  # sta_day <- sta_day[,c('svrid','time',tagSet)]

  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  r
}

###### MAIN STATISTIC ######
dir_dataset <- dir_data14C
fname <- list.files(dir_dataset)
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)))
idx <- seq_len(length(fname))
r <- foreachX(idx,'duty_time_group')
duty_time_svrid <- do.call(rbind,r)
save(duty_time_svrid,file = file.path(dir_data,'duty_time_group.Rda'))

