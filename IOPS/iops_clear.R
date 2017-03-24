#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: iops_clear.R
#
# Description: [useless. merged in to base.R]clear dcast-format io data including iops
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-03-20 11:55:31
#
#
#

iops_melt_clear <- function(data){
  attrid_ps <- 902:903
  attrid_iops <- 36810:36857
  
  data$value[data$value < 0] <- NA
  data$value[data$attrid %in% attrid_ps & data$value > 1e7] <- NA
  data$value[data$attrid %in% attrid_iops & data$value > 1e5] <- NA
  data$value[data$attrid == 999 & data$value > 100] <- 100
  data
}

iops_dcast_clear <- function(dt_dcast){
  col.na <- which(is.na(names(dt_dcast)))
  if(length(col.na) != 0){
    sid <- factor(unique(dt_dcast$svrid[dt_dcast[,col.na]!=0]))
    dt_dcast <- subset(dt_dcast,!(svrid %in% sid))
    dt_dcast[,col.na] <- NULL
  }
  
  dt_dcast[grepl('ps|util|iops',names(dt_dcast))][dt_dcast[grepl('ps|util|iops',names(dt_dcast))] < 0] <- NA
  dt_dcast$rps[dt_dcast$rps > 1e7] <- NA
  dt_dcast$wps[dt_dcast$wps > 1e7] <- NA
  dt_dcast$util[dt_dcast$util > 100] <- 100
  if(any(grepl('iops',names(dt_dcast)))){
    dt_dcast[grepl('iops',names(dt_dcast))][dt_dcast[grepl('iops',names(dt_dcast))] > 1e5] <- NA
  }
  dt_dcast
}
