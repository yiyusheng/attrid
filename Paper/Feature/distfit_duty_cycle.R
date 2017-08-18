#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: distfit_duty_cycle.R
#
# Description: Fit Distribution of duty cycle data of each disk drive
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-16 15:25:39
#
# Last   modified: 2017-08-16 15:25:40
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/IO_statistic_2014/');source('~/rhead')
library(fitdistrplus)
library(logspline)

distfit_dutycycle<- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  splitDT <- split(DT,DT$svrid)
  # r <- lapplyX(seq_len(length(splitDT)),function(i){
  r <- lapply(sample(1:length(splitDT),200),function(i){
    itv <- 1;if(i%%itv==0)cat(sprintf('[%s]\t %d Done!!!\n',fn,i))
    df <- splitDT[[i]]
    x <- as.numeric(df$util)
    x_scale <- (x-min(x))/(max(x)-min(x))
    tryCatch({
      fit.weibull <- fitdist(x,distr='weibull',lower=c(0,0),start=list(scale=1,shape=1))
      return(list(fct2ori(df$svrid[1]),fit.weibull$estimate[1],fit.weibull$estimate[2],fit.weibull$aic))},
      error=function(e){
        return(list(fct2ori(df$svrid[1]),0,0,0))
    })
  })
  r <- setNames(data.frame(matrix(unlist(r),nrow=length(r),byrow=T)),nm = c('svrid','scale','shape','aic'))
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'distfit_dutycycle',frac_cores = 0.9)
para.gamma <- lapply(r,function(a)data.frame(matrix(unlist(a),nrow = nrow(a)/4)))
para.gamma <- do.call(rbind,para.gamma)
names(para.gamma) <- c('svrid','scale','shape','aic')
para.gamma$scale <- fct2num(para.gamma$scale)
para.gamma$shape <- fct2num(para.gamma$shape)
para.gamma$aic <- fct2num(para.gamma$aic)
save(para.gamma,file = file.path(dir_data,'distfit_dutycycle.Rda'))
