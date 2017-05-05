#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plot_failure_iops_1507A.R
#
# Description: To detect failed disk precisely. We correlate the iops of each disk and failure for each svrid in order to find features to locate failed disk.
# NOTE: inhereted from plot_svrid_iops_2015.R 
# Here we plot iops of each disk of failed servers with replacing disks
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-17 11:31:56
#
# Last   modified: 2017-05-02 10:42:14
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')

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

plot_failureS_iops_1407 <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]file %s START!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  dd <- factorX(subset(DT,svrid %in% SD_replacing_fail_svrid$svrnum & time > dayS & time < dayE))
  # dd <- subset(dd,svrid %in% levels(dd$svrid)[1:10])
  splitDD <- split(dd,dd$svrid)
  x <- lapply(splitDD,display_svrid,fn)
  cat(sprintf('[%s]file %s END!!!\n',date(),fn))
}

###### MAIN ######
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
load(file.path(dir_dataCF,'uwork2014.Rda'))
load(file.path(dir_dataCF,'merge_svrnum_svrid_1406_1409.Rda'))
load(file.path(dir_dataSMT,'SD_replacing_fail_svrid.Rda'))

dir_dataset <- dir_data15ADC
dir_figure <- file.path(dir_data,'plot_failureS_iops_1407')
check_dir(dir_figure);clear_dir(dir_figure)

dayS <- as.p('2014-07-10');dayE <- as.p('2014-08-01')
SD_replacing_fail_svrid$svrnum <- FR15M$svrnum[match(SD_replacing_fail_svrid$svrid,FR15M$svrid)]
SD_replacing_fail_svrid <- subset(SD_replacing_fail_svrid,!is.na(svrnum))

fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'plot_failureS_iops_1407')



