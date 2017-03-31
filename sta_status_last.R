#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_status_last.R
#
# Description: divide rps and wps into three level and statistic time stay in each level and number of transfer from levels
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-28 11:51:38
#
# Last   modified: 2017-03-28 11:51:39
#
#
#
rm(list = ls());source('~/rhead')

status_last <- function(arr){
  arr_diff <- c(0,diff(fct2num(arr)))
  idx_start <- c(1,which(arr_diff != 0))
  group_len <- c(diff(idx_start),length(arr) - idx_start[length(idx_start)] + 1)
  group_ele <- arr[idx_start]
  
  # recover <- do.call(c,mapply(function(x,y)rep(x,y),fct2num(group_ele),group_len))
  # if(all.equal(fct2num(arr),recover)){
  #   return(data.frame(ele = group_ele,len = group_len))
  # }else{
  #   return(data.frame(ele = -1,len = -1))
  # }
  return(data.frame(ele = group_ele,len = group_len))
}


trim_status_last <- function(df){
  stat_last <- tapply(df$len,df$ele,summary)
  stat_last <- lapplyX(stat_last,function(x){
    if(!is.null(x))return(x)
    return(summary(-1))
  })
  stat_last <- data.frame(stat_last[,1:6])
  names(stat_last) <- c('min','Q1','Q2','mean','Q3','max')
  row.names(stat_last) <- c('low','median','high')
  stat_last$level <- row.names(stat_last)
  as.numeric(unlist(c(stat_last[stat_last$level == 'low',1:6],stat_last[stat_last$level == 'median',1:6],stat_last[stat_last$level == 'high',1:6])))
}

slx_svrid <- function(df,attrid){
  cname_level <- paste(attrid, '_level',sep = '')
  count_level <- melt(table(df[[cname_level]]))
  
  ssl <- status_last(df[[cname_level]])
  count_change <- nrow(ssl)
  stat_last <- trim_status_last(ssl)
  data.frame(df$svrid[1],attrid,t(c(nrow(df),as.numeric(count_level$value),count_change,t(stat_last))))
}

sta_status_last <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  if(!grepl('a\\d*',fn)){dd <- filter_badiops_NA(dt_dcast,attrName)}
  else{dd <- remove_line_byvalue(df[,c('svrid','time',attrName[c(1,2,4)])])}
  # dd <- subsetX(dd,svrid %in% levels(dd$svrid)[1:50])
  
  dd$rps_level <- cut(dd$rps,levelList$cut_rps,levelList$cut_rps[-length(levelList$cut_rps)],right = F)
  dd$wps_level <- cut(dd$wps,levelList$cut_wps,levelList$cut_wps[-length(levelList$cut_wps)],right = F)
  splitDD <- split(dd,dd$svrid)
  
  level_name <- c('low','median','high')
  summary_name <- c('min','Q1','Q2','mean','Q3','max')
  col_name <- c('svrid','attrid','count',paste('count',level_name,sep='_'),'count_change',
                paste(rep(level_name,each = length(summary_name)),rep(summary_name,length(level_name)),sep = '_'))
  
  status_last_rps <- lapplyX(splitDD,slx_svrid,'rps')
  names(status_last_rps) <- col_name
  
  status_last_wps <- lapplyX(splitDD,slx_svrid,'wps')
  names(status_last_wps) <- col_name

  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  list(status_last_rps,status_last_wps)
}

###### MAIN ######
load(file.path(dir_data,'sta_dcastClear.Rda'))
fname <- list.files(dir_datatendcastClear)
attrName <- c('util','rps','iopsr','wps','iopsw')
levelList <- list(cut_rps = c(0,1,1024,1e6),cut_wps = c(0,64,4096,1e6))
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_status_last')

status_last_rps <- lapplyX(r,'[[',1)
status_last_wps <- lapplyX(r,'[[',2)
save(levelList,r,file = file.path(dir_data,'sta_status_last.Rda'))

###### ANALYSIS ######
slw <- subsetX(status_last_wps,count > 8e4)
slw$cl_round <- roundX(slw$count_low/slw$count)
slw$cm_round <- roundX(slw$count_median/slw$count)
ggplot(slw,aes(x = factor(round(cl_round,digits = 1)),y = cm_round)) + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.05,width = 0.2)
ggplot(slw,aes(cl_round,cm_round)) + geom_point(alpha = 0.1)
