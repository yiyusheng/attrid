#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: iopsFunc.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-19 16:16:23
#
# Last   modified: 2016-12-19 16:16:27
#
#
#

get_attr <- function(sid,aid){
  if(is.numeric(sid)){
    subset(data,svr_id == as.character(sid) & attrid == aid)
  }else{
    subset(data,svrid == sid & attrid == aid)
  }
}

plot_attr <- function(sid,aid){
  df <- get_attr(sid,aid)
  df$time <- as.POSIXct(as.character(df$date),format = '%Y%m%d',tz = 'UTC') + df$timepoint*300
  p <- ggplot(df,aes(x = time,y = value)) + ylab(aid) + geom_path() + geom_point();print(p)
  p
}

class_max_fraction_iops <- function(table_attrid,col_id,numClass){
  max_fraction <- apply(table_attrid[,names(table_attrid) %in% col_id],1,
                        function(x)max(0,round(max(x/sum(x)),digits = 4)))
  ceiling(max_fraction*numClass)
}
