#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_level.R
#
# Description: inherited from sta_zero. we set levels of zero, low, median, high for each attributes and generate distribution of other attributes.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-17 11:25:03
#
# Last   modified: 2017-03-17 11:25:05
#
#
#
rm(list = ls());source('head.R');source('sta_zeroFunc.R')

dataframe_fill <- function(df,colname,default = 0){
  missing_col <- setdiff(colname,names(df))
  if(length(missing_col) == 0)return(df)
  
  tmp <- data.frame(matrix(default,length(missing_col)*nrow(df),nrow = nrow(df)))
  names(tmp) <- missing_col
  df1 <- cbind(df,tmp)
  return(df1[,as.character(colname)])
}

sta_level <- function(i,cutList){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  dd <- remove_line_byvalue(dt_dcast[,c('svrid','time',attrName)])
  
  dd$util_level <- fct2num(cut(dd$util,cutList$cut_util,cutList$cut_util[-length(cutList$cut_util)],right = F))
  dd$rps_level <- fct2num(cut(dd$rps,cutList$cut_rps,cutList$cut_rps[-length(cutList$cut_rps)],right = F))
  dd$iopsr_level <- fct2num(cut(dd$iopsr,cutList$cut_iopsr,cutList$cut_iopsr[-length(cutList$cut_iopsr)],right = F))
  dd$wps_level <- fct2num(cut(dd$wps,cutList$cut_wps,cutList$cut_wps[-length(cutList$cut_wps)],right = F))
  dd$iopsw_level <- fct2num(cut(dd$iopsw,cutList$cut_iopsw,cutList$cut_iopsw[-length(cutList$cut_iopsw)],right = F))
  
  attr_pair <- data.frame(attr_value = rep(attrName,each = length(attrName)),attr_dist = rep(attrName,length(attrName)))
  attr_pair <- subset(attr_pair, attr_value != attr_dist)
  
  r <- mapply(function(attrv,attrd){
    # cat(sprintf('%s\t%s\n',attrv,attrd))
    attrv_level <- paste(attrv,'_level',sep='')
    attrd_level <- paste(attrd,'_level',sep='')
    tmp <- dataframe_fill(as.data.frame.matrix(table(dd[[attrv_level]],dd[[attrd_level]])),
                          as.character(cutList[[paste('cut_',attrd,sep = '')]]))
    names(tmp) <- c('zero','low','median','high')
    data.frame(a1 = attrv,a2 = attrd,a1_value = as.numeric(row.names(tmp)),tmp)
  },attr_pair$attr_value,attr_pair$attr_dist,SIMPLIFY = F)
  r1 <- do.call(rbind,r);r1$fn <- fn;row.names(r1) <- NULL
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  r1
}

###### MAIN ######
fname <- list.files(dir_datatendcastClear)
fname <- fname[!grepl('a\\d.*',fname)]
attrName <- c('util','rps','iopsr','wps','iopsw')
cutList <- list(cut_util = c(0,1,3,25,101),
                cut_rps = c(0,1,500,1e4,1e6),
                cut_iopsr = c(0,1,150,3e3,1e6),
                cut_wps = c(0,1,100,8e3,1e6),
                cut_iopsw = c(0,1,400,3e3,1e6))

require(doParallel)
idx <- seq_len(length(fname))
outname <- 'out_sl';if(file.exists(outname))file.remove(outname)
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = outname)
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% tryCatch(sta_level(i,cutList),error = function(e){cat(sprintf("[%s]\tERROR:%s\n%s\n",date(),fname[i],e))})
stopCluster(ck)