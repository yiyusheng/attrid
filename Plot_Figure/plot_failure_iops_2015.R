#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plot_failure_iops_2015.R
#
# Description: plot iops of each disk
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-14 16:36:05
#
# Last   modified: 2017-04-14 16:36:06
#
#
#

rm(list = ls());source('~/rhead')

display_svrid <- function(df,fn){
  df.fr <- factorX(subset(failRecord,svrid == fct2ori(df$svrid[1])))
  
  p1 <- ggplot(df,aes(x = time,y = util)) + geom_line() + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3) + ggtitle(df$svrid[1])
  p2 <- ggplot(df,aes(x = time,y = rps)) + geom_line() + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3)
  p3 <- ggplot(df,aes(x = time,y = wps)) + geom_line() + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3)
  p4 <- ggplot(df,aes(x = time,y = iopsr)) + geom_line()  + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3)
  p5 <- ggplot(df,aes(x = time,y = iopsw)) + geom_line()  + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3)
  p6 <- ggplot(df,aes(x = time,y = sizer)) + geom_line()  + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3)
  p7 <- ggplot(df,aes(x = time,y = sizew)) + geom_line()  + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'red',linetype = 3)
  
  png(filename = file.path(dir_figure,paste(gsub('\\.Rda','',fn),'-',df$svrid[1],'.png',sep='')),                                                                
      width = 1920, height = 1080, bg = "white")
  multiplot(p1,p2,p3,p4,p5,p6,p7,cols = 1)
  dev.off()
  
}

plot_failure_iops <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]file %s START!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  dd <- filter_badiops_NA(dt_dcast,attrName,fn)
  dd$sizer <- dd$rps/dd$iopsr;dd$sizer[is.na(dd$sizer) | is.infinite(dd$sizer)] <- 0
  dd$sizew <- dd$wps/dd$iopsw;dd$sizew[is.na(dd$sizew) | is.infinite(dd$sizew)] <- 0
  dd <- factorX(dd)
  splitDD <- split(dd,dd$svrid)
  lapply(splitDD,display_svrid,fn)
  cat(sprintf('[%s]file %s END!!!\n',date(),fn))
}

###### MAIN ######
dir_dataset <- dir_data15D
attrName <- attrNameExtend
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
dir_figure <- file.path(dir_data,'failure_iops_2015');check_dir(dir_figure)
r <- foreachX(idx,'plot_failure_iops')


