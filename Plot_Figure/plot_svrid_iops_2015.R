#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plot_svrid_iops.R
#
# Description: To detect failed disk precisely. We correlate the iops of each disk and failure for each svrid in order to find features to locate failed disk.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-17 11:31:56
#
# Last   modified: 2017-04-17 11:31:57
#
#
#
rm(list = ls());source('~/rhead')

display_svrid <- function(df,fn){
  df.fr <- factorX(subset(failRecord,svrid == fct2ori(df$svrid[1])))
  col_iopsr <- names(df)[grepl('iopsr',names(df))]
  col_iopsw <- names(df)[grepl('iopsw',names(df))]
  
  p_util <- ggplot(df,aes(x = time,y = util)) + geom_line(color = cbPalette[[4]]) + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'yellow') + ggtitle(df$svrid[1])
  p_rps <- ggplot(df,aes(x = time,y = rps)) + geom_line(color = cbPalette[[4]]) + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'yellow')
  p_wps <- ggplot(df,aes(x = time,y = wps)) + geom_line(color = cbPalette[[4]]) + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'yellow')
  
  p_iopsr <- lapply(col_iopsr,function(cn){
    df$tmp <- df[[cn]]
    ggplot(df,aes(x = time,y = tmp)) + geom_line(color = cbPalette[[4]]) + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'yellow') + ylab(cn)
  })
  
  p_iopsw <- lapply(col_iopsw,function(cn){
    df$tmp <- df[[cn]]
    ggplot(df,aes(x = time,y = tmp)) + geom_line(color = cbPalette[[4]]) + geom_vline(xintercept = as.numeric(df.fr$f_time),size = 1,color = 'yellow') + ylab(cn)
  })
  
  p <- c(list(p_util),list(p_rps),p_iopsr,list(p_util),list(p_wps),p_iopsw)
  
  png(filename = file.path(dir_figure,paste(gsub('\\.Rda','',fn),'-',df$svrid[1],'.png',sep='')),                                                                
      width = 1920*2, height = 1080*2, bg = "white")
  multiplot(plotlist = p,cols = 2)
  dev.off()
  cat(sprintf('[%s]svrid %s from %s with %d failures DONE!!!\n',date(),df$svrid[1],fn,nrow(df.fr)))
  return(0)
}

plot_svrid_iops_2015 <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]file %s START!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  dd <- factorX(DT)
  # dd <- subset(dd,svrid %in% levels(dd$svrid)[1:10])
  splitDD <- split(dd,dd$svrid)
  lapply(splitDD,display_svrid,fn)
  cat(sprintf('[%s]file %s END!!!\n',date(),fn))
}

###### MAIN ######
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
dir_dataset <- dir_data15ADC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
dir_figure <- file.path(dir_data,'plot_svrid_iops_2015')
check_dir(dir_figure)
r <- foreachX(idx,'plot_svrid_iops_2015')



