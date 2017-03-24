#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_coefficientVar.R
#
# Description: generate coefficient variable for io (util, xps, iops, iopsr, iopsw) and analize their relationship
# We split value of io(5 attributes) for each week and generate their distribution based on 20 quantiles of all values.
# Then the cv of each range limited by quantiles for all weeks are generated. We present the mean cv, count of cv and number of cv large than 1.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-03 11:07:50
#
# Last   modified: 2017-03-03 11:07:51
#
#
#

rm(list = ls());source('head.R')

gen_cut <- function(arr,numCut){
  cut_arr <- quantile(arr,seq(0,1,1/numCut),na.rm = T)
  dup_arr <- duplicated(cut_arr)
  if(any(dup_arr) == T){
    idx_upper <- min(which(dup_arr == T))
    new_cut <- round(gen_cut(arr[arr > cut_arr[idx_upper]],numCut - idx_upper + 1))
    return(c(cut_arr[1:(idx_upper-1)],new_cut))
  }else{
    return(cut_arr)
  }
  # cut_util <- gen_cut(df$util,numCut)
  # idx <- !is.na(cut_util)
  # cut_util <- cut_util[idx]
  # names(cut_util) <- names(quantile(1,seq(0,1,1/numCut)))[idx]
  # if(length(cut_util) <= 3)return(data.frame(sum = -1,mean = -1))
  # cut_x <- c(0:4,seq(5,15,5),20,50,80,seq(85,95,5),96:101)
  # cut_x <- sort(unique(round(quantile(df[[attr]],seq(0,1,0.05),na.rm = T))))
  # if(length(cut_x) <= 3)return(data.frame(num_vibrate = -1,mean_cv = -1, count = -1))
}

sta_svrid_coefficientVar <- function(df,fn,attr,cut_attr){
  cat(sprintf('file:%s\tid:%s\n',fn,df$svrid[1]))


  cut_x <- cut_attr

  df$x_cut <- fct2num(cut(df$util,breaks = cut_x,labels = cut_x[-length(cut_x)],right = F))
  df$time_cut <- cut.POSIXt(df$time,seq.POSIXt(min(df$time),max(df$time),by = 'weeks'))

  r <- do.call(rbind,tapply(df$x_cut,df$time_cut,function(x){
    table_x <- melt(table(x))
    if(nrow(table_x) == (length(cut_x)-1)){
      return(t(table_x$value))
    }else{
      table_x <- rbind(table_x,data.frame(x = setdiff(cut_x[-length(cut_x)],table_x$x),value = 0))
      table_x <- table_x[order(table_x$x),]
      return(t(table_x$value))
    }
  }))
  eva <- apply(r,2,function(x)sd(x)/mean(x))
  eva[is.na(eva)] <- 0
  return(data.frame(vibrate_range = sum(eva > 1), valid_range = length(unique(df$x_cut)),mean_cv = mean(eva)))
}

sta_coefficientVar <- function(i){
  fn <- fname[i]
  load(file.path(dir_datatendcast,fn))
  dt_dcast <- iops_dcast_clear(dt_dcast)
  dt_dcast <- factorX(subset(dt_dcast,svrid %in% r_sta_svrid$svrid[r_sta_svrid$numDate > 200]))
  dt_dcast <- iops_aggragate(dt_dcast)
  splitDD <- split(dt_dcast[,c('svrid','time','util','xps','iops','iopsr','iopsw')],factor(dt_dcast$svrid))
  
  util_cv <- lapply(splitDD,sta_svrid_coefficientVar,fn,'util',cut_util)
  xps_cv <- lapply(splitDD,sta_svrid_coefficientVar,fn,'xps',cut_xps)
  iops_cv <- lapply(splitDD,sta_svrid_coefficientVar,fn,'iops',cut_iops)
  iopsr_cv <- lapply(splitDD,sta_svrid_coefficientVar,fn,'iopsr',cut_iops)
  iopsw_cv <- lapply(splitDD,sta_svrid_coefficientVar,fn,'iopsw',cut_iops)
  
  r <- cbind(svrid = names(util_cv),do.call(rbind,util_cv),do.call(rbind,xps_cv),do.call(rbind,iops_cv),do.call(rbind,iopsr_cv),do.call(rbind,iopsw_cv))
}

###### MAIN ######
fname <- list.files(dir_datatendcast)
load(file.path(dir_data,'r_sta.Rda'))
load(file.path(dir_data,'aggregate_aid.Rda'))
r_mean_svrid <- rbind(r_mean_aid[[1]],r_mean_aid[[2]][,1:4]);names(r_mean_svrid)[1] <- 'svrid'
r_sta_svrid <- melt(tapply(r_sta$date,r_sta$svr_id,length));names(r_sta_svrid) <- c('svrid','numDate')
cut_util <- c(seq(0,100,1),101)
cut_xps <- c(seq(0,1e4,1e2),1e6)
cut_iops <- cut_xps

require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_sc')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% tryCatch(sta_coefficientVar(i),error = function(e)cat(sprintf('ERROR:%s\n',fname[i])))
stopCluster(ck)
sta_util_cv <- do.call(rbind,r)


attr_name <- c('util','xps','iops','iopsr','iopsw')
names(sta_util_cv)[grepl('mean_cv',names(sta_util_cv))] <- paste(attr_name,'mcv',sep='_')
names(sta_util_cv)[grepl('vibrate_range',names(sta_util_cv))] <- paste(attr_name,'vir',sep='_')
names(sta_util_cv)[grepl('valid_range',names(sta_util_cv))] <- paste(attr_name,'var',sep='_')

save(sta_util_cv,file = file.path(dir_data,'sta_cv_std.Rda'))


###### ANALYSIS ######
# sta_utilcv_io <- merge(sta_util_cv,r_mean_svrid,by = 'svrid')
# sta_utilcv_io$svrid <- fct2num(sta_utilcv_io$svrid)
# p1 <- ggplot(sta_utilcv_io,aes(x = factor(round(mean_cv/0.5)*0.5))) + geom_boxplot(aes(y = mean_cv),outlier.shape = NA) + geom_jitter(aes(y = mean_cv),width = 0.2,alpha = 0.01)
# p2 <- ggplot(sta_utilcv_io,aes(x = factor(round(mean_cv/0.5)*0.5))) + geom_boxplot(aes(y = rps),outlier.shape = NA) + geom_jitter(aes(y = rps),width = 0.2,alpha = 0.01)
# p3 <- ggplot(sta_utilcv_io,aes(x = factor(round(mean_cv/0.5)*0.5))) + geom_boxplot(aes(y = wps),outlier.shape = NA) + geom_jitter(aes(y = wps),width = 0.2,alpha = 0.01)
# p4 <- ggplot(sta_utilcv_io,aes(x = factor(round(mean_cv/0.5)*0.5))) + geom_boxplot(aes(y = util),outlier.shape = NA) + geom_jitter(aes(y = util),width = 0.2,alpha = 0.01)
# multiplot(p1,p2,p3,p4,cols = 2)
