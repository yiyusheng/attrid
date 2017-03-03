#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_util_coefficientVar.R
#
# Description: 
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
}

sta_util_coefficientVar_svrid <- function(df,thred = 1,numCut = 5,fn){
  cat(sprintf('file:%s\tid:%s\n',fn,df$svrid[1]))
  
  # cut_util <- gen_cut(df$util,numCut)
  # idx <- !is.na(cut_util)
  # cut_util <- cut_util[idx]
  # names(cut_util) <- names(quantile(1,seq(0,1,1/numCut)))[idx]
  # if(length(cut_util) <= 3)return(data.frame(sum = -1,mean = -1))
  cut_util <- c(0:4,seq(5,15,5),20,50,80,seq(85,95,5),96:101)
  
  df$util_cut <- fct2num(cut(df$util,breaks = cut_util,labels = cut_util[-length(cut_util)],right = F))
  df$time_cut <- cut.POSIXt(df$time,seq.POSIXt(min(df$time),max(df$time),by = 'weeks'))
  r <- do.call(rbind,tapply(df$util_cut,df$time_cut,function(x){
    table_x <- melt(table(x))
    if(nrow(table_x) == (length(cut_util)-1))return(t(table_x$value))
    else{
      table_x <- rbind(table_x,data.frame(x = setdiff(cut_util[-length(cut_util)],table_x$x),value = 0))
      table_x <- table_x[order(table_x$x),]
      return(t(table_x$value))
    }
  }))
  eva <- apply(r,2,function(x)sd(x)/mean(x))
  eva[is.na(eva)] <- 0
  return(data.frame(num_vibrate = sum(eva > thred),mean_cv = mean(eva)))
}

sta_util_coefficientVar <- function(i){
  fn <- fname[i]
  load(file.path(dir_datatendcast,fn))
  dt_dcast <- iops_dcast_clear(dt_dcast)
  dt_dcast <- factorX(subset(dt_dcast,svrid %in% r_sta_svrid$svrid[r_sta_svrid$numDate > 200]))
  splitDD <- split(dt_dcast[,c('svrid','time','util')],factor(dt_dcast$svrid))
  util_cv <- lapply(splitDD,sta_util_coefficientVar_svrid,thred = 1,numCut = 5,fn)
  a <- cbind(do.call(rbind,util_cv),svrid = names(util_cv))
}

fname <- list.files(dir_datatendcast)
load(file.path(dir_data,'r_sta.Rda'))
r_sta_svrid <- melt(tapply(r_sta$date,r_sta$svr_id,length));names(r_sta_svrid) <- c('svrid','numDate')

require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_suc')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% tryCatch(sta_util_coefficientVar(i),error = function(e)cat(sprintf('ERROR:%s\n',fname[i])))
stopCluster(ck)
sta_util_cv <- do.call(rbind,r)
save(sta_util_cv,file = file.path(dir_data,'sta_util_cv.Rda'))