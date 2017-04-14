#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: find_relation_io_failure.R
#
# Description: Find the reason why failure have no influence on IO workload
# 1.observe single disk servers
# 2.observe intersect disks from yk's failure record and ym's failure record
#
# [2017/04/14]After I scan this script, I find it do not find the relationship between io and failure. 
# But find the number of servers in each day. If data of the servers is missing, the counter add 1.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-21 20:17:33
#
# Last   modified: 2016-12-21 20:17:35
#
#
#

rm(list = ls());setwd('/home/yiyusheng/Code/R/Disk_Workload_201406-201407/');source('~/rhead')
source('IO_statistic/iopsFunc.R')
dir_iops <- dir_data15
dir_datasource <- '/home/yiyusheng/Data/Load_Data_2014/Config_Failure_2014'
dir_ten <- "/home/yiyusheng/Data/Load_Data_2015"

load(file.path(dir_iops,'d3.Rda'))
load(file.path(dir_datasource,'load_ftr_attrid.Rda'))
load(file.path(dir_ten,'merge_id_svrid.Rda'))
load(file.path(dir_data,'locate_fail.Rda'))

# S2. generate intersect disks from two failure records
# dt <- data
# names(dt)[names(dt) == 'svr_id'] <- 'sid'
# aid <- c(902,903,999,36810:36857)
# dt$svrid <- factor(merge_id_svrid$svrid[match(dt$sid,merge_id_svrid$sid)])
# yk_disk <- pos_disk
# yk_disk$dist_f_time <- as.numeric(abs(difftime(yk_disk$failed_time,yk_disk$f_time_new,units = 'days')))
# yk_disk_same_time <- subset(yk_disk,dist_f_time <= 5 & svrid %in% dt$svrid)
# yk_disk_same_time$dev_class_id <- cmdb$dev_class_id[match(yk_disk_same_time$svrid,cmdb$svr_asset_id)]
# yk_disk_inter <- factorX(subset(yk_disk_same_time))
# yk_disk_inter <- merge(yk_disk_inter,ext_disk[,c('svrid','eventdescription')],by = 'svrid')
# 
# ydi <- yk_disk_inter[1,]
# smpIO <- subset(dt,sid == ydi$sid)
# smpIO$time <- as.POSIXct(as.character(smpIO$date),tz = 'UTC',format = '%Y%m%d') + smpIO$timepoint*300
# smpIO$attrid <- paste('X',smpIO$attrid,sep='')
# smpIO_dcast <- dcast(smpIO[,c('svrid','time','attrid','value')],svrid + time ~ attrid,value.var = 'value',fill = 0)
# smpIO_dcast <- subset(smpIO_dcast,abs(difftime(time,ydi$failed_time,units = 'days')) < 5)
# 
# ggplot(smpIO_dcast,aes(x = time,y = X902)) + geom_line()
# ggplot(smpIO_dcast,aes(x = time,y = X903)) + geom_line()
# ggplot(smpIO_dcast,aes(x = time,y = X999)) + geom_line()
# ggplot(smpIO_dcast,aes(x = time,y = X36810)) + geom_line()
# ggplot(smpIO_dcast,aes(x = time,y = X36811)) + geom_line()
# ggplot(smpIO_dcast,aes(x = time,y = X36834)) + geom_line()
# ggplot(smpIO_dcast,aes(x = time,y = X36835)) + geom_line()

# S3. extract time without data
missing_svr <- function(dt,serverid){
  cat(sprintf('sid: %s\n',serverid))
  smpIO <- subset(dt,svrid == serverid)
  max_tp <- max(smpIO$time)
  min_tp <- min(smpIO$time)
  seq_tp <- seq.POSIXt(min_tp,max_tp,300)
  seq_tp <- seq_tp[as.Date(seq_tp) != as.Date('2014-10-27') & as.Date(seq_tp) != as.Date('2015-05-07')]
  uni_tp <- unique(smpIO$time)
  if(length(seq_tp) == length(uni_tp)){
    return(NULL)
  }else{
    svr_iocheck <- data.frame(timepoint = seq_tp,check = F)
    svr_iocheck$check[svr_iocheck$timepoint %in% uni_tp] <- T
    svr_iocheck_noexist <- subset(svr_iocheck,check == F)
    return(c(unique(as.Date(svr_iocheck_noexist$timepoint))))
  }
}

missing_date <- function(i){
  load(file.path(dir_iops,fname[i]))
  dt <- data
  names(dt)[names(dt) == 'svr_id'] <- 'sid'
  dt$time <- as.POSIXct(as.character(dt$date),tz = 'UTC',format = '%Y%m%d') + dt$timepoint*300
  dt$svrid <- factor(merge_id_svrid$svrid[match(dt$sid,merge_id_svrid$sid)])
  uni_sid <- levels(dt$svrid)
  missing_svr <- sapply(uni_sid,function(x)missing_svr(dt,x))
  
}

fname <- list.files(dir_iops)
require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_frif')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% {
  missing_date(i)
}
stopCluster(ck)
save(r,file = file.path(dir_data,'missing_date.Rda'))

sta_r <- melt(table(as.Date(unlist(unlist(r)),origin = '1970-01-01')))
names(sta_r) <- c('date','missing_svr')
ggplot(sta_r,aes(x = date,y = missing_svr)) + geom_bar(stat = 'identity')

# The plot indicates number of missing data is burst up than 800 in some days. 
# It implys that their storage system does not receive data for these days.
# There is something wrong with these burst days
# For those days less than 800 days, it indicates there are about 400-600 failure happened in each day?
# I don't think so, I believe that most of them are all zero.