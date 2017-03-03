#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: aggregate_svrid_iops.R
#
# Description: statistic data of 201608 to aggreate sum,count,sd,mean for each day
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-02-13 11:04:22
#
#
#

rm(list = ls())

aggregate_date <- function(rx,func){
  f <- get(func)
  d <- lapply(rx,function(x){
    aggregate(x[,!grepl('svrid|date',names(x))],by = list(x$svrid),f,na.rm = T)
  })
}

aggregate_iops <- function(rx,func){
  f <- get(func)
  d0 <- rx[['d0']]
  d1 <- lapply(rx[-1],function(x){
    x1 <- cbind(x[,!grepl('iops',names(x))],
                apply(x[,grepl('iopsr',names(x))],1,f,na.rm = T),
                apply(x[,grepl('iopsw',names(x))],1,f,na.rm = T))
    names(x1) <- c(names(x)[!grepl('iops',names(x))],'iopsr','iopsw')
    x1
  })
  list(d0,do.call(rbind,d1))
}

get_d <- function(str){
  list[me,sd,su,co] <- list(r_mean[[str]],r_sd[[str]],r_sum[[str]],r_count[[str]])
}

get_svrid <- function(dx,id){
  list(subset(r_mean[[dx]],svrid == id),
       subset(r_sd[[dx]],svrid == id),
       subset(r_sum[[dx]],svrid == id),
       subset(r_count[[dx]],svrid == id),
       subset(r_sta,svr_id == id))
}

###### ANALYSIS MAIN ######
source('head.R')
source('iops_clear.R')
library(plyr)
load(file.path(dir_data,'perday_201608.Rda'))

# aggregate iops
r_count_ai <- aggregate_iops(r_count,'sum')
r_mean_ai <- aggregate_iops(r_mean,'mean')
r_sd_ai <- aggregate_iops(r_sd,'sum')
r_sum_ai <- aggregate_iops(r_sum,'sum')

# aggreate date without aggregated iops
r_count_ad <- aggregate_date(r_count,'sum')
r_mean_ad <- aggregate_date(r_mean,'mean')
r_sd_ad <- aggregate_date(r_sd,'sum')
r_sum_ad <- aggregate_date(r_sum,'sum')

# aggreate date with aggregated iops
r_count_aid <- aggregate_iops(r_count_ad,'sum')
r_mean_aid <- aggregate_iops(r_mean_ad,'sum')
r_sd_aid <- aggregate_iops(r_sd_ad,'sum')
r_sum_aid <- aggregate_iops(r_sum_ad,'sum')

save(r_count_ai,r_mean_ai,r_sd_ai,r_sum_ai,
     r_count_ad,r_mean_ad,r_sd_ad,r_sum_ad,
     r_count_aid,r_mean_aid,r_sd_aid,r_sum_aid,r_sta,
     file = file.path(dir_data,'aggregate_svrid_iops.Rda'))
# list[me_day,sd_day,su_day,co_day,sid_info] <- get_sta('d2',4820)
# 
# me_cast <- melt(me1[,-c(2,3,4)],id.vars = 'Group.1')
# ggplot(me_cast,aes(x = value,group = variable,color = variable)) + stat_ecdf()
