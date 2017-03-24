#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_trend_length.R
#
# Description: generate length of increasing/decreasing for each svrid + attrid on each week.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-06 21:22:22
#
# Last   modified: 2017-03-06 21:22:23
#
#
#

rm(list = ls());source('head.R')

get_array_trend <- function(arr){
  arr <- arr[!is.na(arr)]
  len <- length(arr)
  if(length(unique(arr)) == 1)return(data.frame(start = 0,end = 0,len = 0,amp = 0))
  p <- 1;q <- p + 1
  trend_tag <- NA
  arr_trend <- list()
  
  while(q <= len){
    cur_tag <- sign(arr[q] - arr[q-1])
    if(is.na(trend_tag) & cur_tag == 0){
      p <- p + 1;q <- p + 1
    }else if(is.na(trend_tag) & cur_tag != 0){
      trend_tag <- cur_tag;q <- q + 1
    }else if(trend_tag == cur_tag & q != len){
      q <- q + 1
    }else if(trend_tag != cur_tag){
      target_arr <- diff(arr[p:(q-1)])
      arr_trend[[length(arr_trend) + 1]] <- data.frame(start = p,end = q - 1,len = q - p,amp = arr[q - 1] - arr[p],
                                                       median = mean(quantile(target_arr,.25),quantile(target_arr,.75)))
      p <- q ;q <- q + 1
      trend_tag <- NA
    }else{
      q <- q + 1
    }
  }
  
  if(q - p > 1){
    target_arr <- diff(arr[p:(q-1)])
    arr_trend[[length(arr_trend) + 1]] <- data.frame(start = p,end = q - 1,len = q - p,amp = arr[q - 1] - arr[p],
                                                     median = mean(quantile(target_arr,.25),quantile(target_arr,.75)))
  }
  
  do.call(rbind,arr_trend)
}

sta_svrid_lenTrend <- function(df,fn,attr){
  cat(sprintf('file:%s\tid:%s\tattr:%s\n',fn,df$svrid[1],attr))
  # df <- splitDD[names(splitDD) == '1589'][[1]]
  df$time_cut <- cut.POSIXt(df$time,seq.POSIXt(min(df$time),max(df$time),by = 'month'))
  
  df_unit <- list2df(tapply(df[[attr]],df$time_cut,mean,na.rm = T),n = c('value','units'))
  # ggplot(df_unit,aes(x = units,y = value,group = 1)) + geom_line()
  len <- dim(df_unit)[1]
  sta_trend <- get_array_trend(df_unit$value)
  sta_trend <- subset(sta_trend,len > 1)
  if(nrow(sta_trend) == 0){
    r_ssl <- data.frame(amp_ap = NA,start_ap = NA,len_ap = NA,mdn_ap = NA,amp_lp = NA,start_lp = NA,len_lp = NA,mdn_lp = NA)
    names(r_ssl) <- paste(attr,names(r_ssl),sep='_')
    return(r_ssl)
  }
  
  idx_pa <- which(sta_trend$amp == max(sta_trend$amp[sta_trend$amp > 0]))[1]
  idx_na <- which(sta_trend$amp == min(sta_trend$amp[sta_trend$amp < 0]))[1]
  idx_pl <- which(sta_trend$amp > 0);idx_pl <- idx_pl[which(sta_trend$len[idx_pl] == max(sta_trend$len[idx_pl]))][1]
  idx_nl <- which(sta_trend$amp < 0);idx_nl <- idx_nl[which(sta_trend$len[idx_nl] == max(sta_trend$len[idx_nl]))][1]

  r_ssl <- data.frame(amp_ap = sta_trend$amp[idx_pa],start_ap = sta_trend$start[idx_pa],len_ap = sta_trend$len[idx_pa],mdn_ap = sta_trend$median[idx_pa],
             amp_lp = sta_trend$amp[idx_pl],start_lp = sta_trend$start[idx_pl],len_lp = sta_trend$len[idx_pl],mdn_lp = sta_trend$median[idx_pl])
             # amp_an = sta_trend$amp[idx_na],start_an = sta_trend$start[idx_na],len_an = sta_trend$len[idx_na],
             # amp_ln = sta_trend$amp[idx_nl],start_ln = sta_trend$start[idx_nl],len_ln = sta_trend$len[idx_nl])
  names(r_ssl) <- paste(attr,names(r_ssl),sep='_')
  r_ssl
}

sta_lenTrend <- function(i){
  fn <- fname[i]
  load(file.path(dir_datatendcast,fn))
  dt_dcast <- iops_dcast_clear(dt_dcast)
  dt_dcast <- factorX(subset(dt_dcast,svrid %in% r_sta_svrid$svrid[r_sta_svrid$numDate > 200]))
  dt_dcast <- iops_aggragate(dt_dcast)
  splitDD <- split(dt_dcast[,c('svrid','time','util','xps','iops','iopsr','iopsw')],factor(dt_dcast$svrid))
  
  util_cv <- lapply(splitDD,sta_svrid_lenTrend,fn,'util')
  xps_cv <- lapply(splitDD,sta_svrid_lenTrend,fn,'xps')
  iops_cv <- lapply(splitDD,sta_svrid_lenTrend,fn,'iops')
  iopsr_cv <- lapply(splitDD,sta_svrid_lenTrend,fn,'iopsr')
  iopsw_cv <- lapply(splitDD,sta_svrid_lenTrend,fn,'iopsw')
  
  r <- cbind(svrid = names(util_cv),do.call(rbind,util_cv),do.call(rbind,xps_cv),do.call(rbind,iops_cv),do.call(rbind,iopsr_cv),do.call(rbind,iopsw_cv))
}

###### MAIN ######
fname <- list.files(dir_datatendcast)
load(file.path(dir_data,'r_sta.Rda'))
r_sta_svrid <- melt(tapply(r_sta$date,r_sta$svr_id,length));names(r_sta_svrid) <- c('svrid','numDate');r_sta_svrid$fn <- get_fname(r_sta_svrid$svrid)

require(doParallel)
idx <- seq_len(length(fname))
file.remove('out_stl')
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_stl')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% tryCatch(sta_lenTrend(i),error = function(e)cat(sprintf('ERROR:%s\t%s\n',fname[i],e)))
stopCluster(ck)
sta_trend_len <- do.call(rbind,r)
save(sta_trend_len,file = file.path(dir_data,'sta_trend_len_month.Rda'))

###### ANALYSIS ######
sta_trend_len[,-1] <- round(sta_trend_len[,-1],digits = 4)

sta_trend_util <- sta_trend_len[,grepl('svrid|util',names(sta_trend_len))]
# sta_trend_util <- sta_trend_util[,grepl('svrid|amp_ap|len_lp|amp_an|len_ln|mdn',names(sta_trend_util))]
# ggplot(sta_trend_util,aes(x = util_amp_ap,y = util_len_lp)) + geom_point(alpha = 0.1)

sta_trend_xps <- sta_trend_len[,grepl('svrid|xps',names(sta_trend_len))]
sta_trend_iops <- sta_trend_len[,grepl('svrid|iops_',names(sta_trend_len))]

# df_unit$units <- as.p(df_unit$units)
# df_unit$diff <- c(diff(df_unit$value),0)
# df_unit$trendTag <- c(0,df_unit$diff[-c(1,2)]*df_unit$diff[-c(1,len)],0)
# 
# inflex_point <- which(df_unit$trendTag < 0)
# if(length(inflex_point) == 0)return(data.frame(rep(0,12)))
# 
# sta_trend <- data.frame(start = c(1,inflex_point),
#                         end = c(inflex_point - 1,nrow(df_unit)))
# sta_trend$len <- sta_trend$end - sta_trend$start + 1
# sta_trend$amp <- df_unit$value[sta_trend$end] - df_unit$value[sta_trend$start]