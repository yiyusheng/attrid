#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plot_util_xps_iops.R
#
# Description: plot util-xps and util-iops for analysis
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
rm(list = ls());source('head.R')

# Reduce sample for geom_jitter
reduce_sample <- function(smp){
  smp$idx <- seq_len(nrow(smp))
  idx_need <- do.call(c,tapply(smp$idx,smp$util,function(x)sample(x,min(length(x),200),replace = T)))
  smp[-idx_need,c('xps','iops','rsize','wsize')] <- NA
  smp
}

# plot util with xps, iops, sizer and sizew
plot_util_xps_iops <- function(smp,fn){
  fn <- gsub('\\.Rda','',fn)
  fn_path <- file.path(dir_data,'util_xps_iops',paste(fn,'-',smp$svrid[1],'.png',sep=''))
  if(file.exists(fn_path))return(0)
  cat(sprintf('%s\t%s\n',fn,smp$svrid[1]))
  smp <- reduce_sample(smp)
  
  p1 <- ggplot(smp,aes(x = util))  + 
    geom_bar(fill = colorSet[1]) + 
    geom_boxplot(aes(y = xps),fill = colorSet[5],outlier.shape = NA) + 
    geom_jitter(aes(y = xps),alpha = 0.2,width = 0.2,color = colorSet[6]) + 
    xlab('util_level(%)') + ylab('xps(KB/s)') + ggtitle(paste('[util+xps][',smp$svrid[1],'-',fn,']',sep=''))
  
  p2 <- ggplot(smp,aes(x = util))  + 
    geom_bar(fill = colorSet[1]) + 
    geom_boxplot(aes(y = iops),fill = colorSet[5],outlier.shape = NA) + 
    geom_jitter(aes(y = iops),alpha = 0.2,width = 0.2,color = colorSet[6]) + 
    xlab('util_level(%)') + ylab('iops') + ggtitle(paste('[util+iops][',smp$svrid[1],'-',fn,']',sep=''))
  
  p3 <- ggplot(smp,aes(x = util))  + 
    geom_boxplot(aes(y = rsize),fill = colorSet[5],outlier.shape = NA) + 
    geom_jitter(aes(y = rsize),alpha = 0.2,width = 0.2,color = colorSet[6]) + 
    xlab('util_level(%)') + ylab('rsize(kB)') + ggtitle(paste('[util+rsize][',smp$svrid[1],'-',fn,']',sep=''))
  
  p4 <- ggplot(smp,aes(x = util))  + 
    geom_boxplot(aes(y = wsize),fill = colorSet[5],outlier.shape = NA) + 
    geom_jitter(aes(y = wsize),alpha = 0.2,width = 0.2,color = colorSet[6]) + 
    xlab('util_level(%)') + ylab('wsize(kB)') + ggtitle(paste('[util+wsize][',smp$svrid[1],'-',fn,']',sep=''))
     
  
  png(filename = file.path(dir_data,'util_xps_iops',paste(fn,'-',smp$svrid[1],'.png',sep='')),                                                                
      width = 1920, height = 1080, bg = "white")
  if(grepl('a\\d',fn)){
    print(p1)
    }else{
    multiplot(p1,p2,p3,p4,cols = 2)
    }
  dev.off()
  return(0)
}

# plot for each file
gen_plot_util_xps_iops <- function(i){
  fn <- fname[i]
  load(file.path(dir_datatendcast,fn))
  df <- iops_dcast_clear(dt_dcast)
  df <- iops_aggragate(df)
  df <- request_size(df)
  df$util <- factor(df$util)
  splitdf <- split(df,factor(df$svrid))
  smp <- splitdf[[100]]
  # smp <- splitdf[[which(names(splitdf) == '3463')]]
  plotset <- lapply(splitdf,plot_util_xps_iops,fn = fn)
}

###### MAIN ######
fname <- list.files(dir_datatendcast)
require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_ioc')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% {
  tryCatch(gen_plot_util_xps_iops(i),error = function(e){cat(sprintf('ERROR:%s\n',fname[i]))})
}
stopCluster(ck)


