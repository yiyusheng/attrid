#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: valuecount_bandwidth.R
#
# Description: count duty cycle value for each disk 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-21 22:07:12
#
# Last   modified: 2017-08-30 20:08:58
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

complete_column <- function(df,full_names){
  lack_names <- setdiff(full_names,names(df))
  if(length(lack_names)!=0){
    for(i in 1:length(lack_names)){
      df[[lack_names[i]]] <- 0
      
    }
  }
  df <- df[,c('svrid',full_names)]
  return(df)
}

count_bandwidth_value<- function(i,vt,bins){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT)
  DT$rps_level <- ceiling(DT$rps_trunc/(vt[1]/bins))*(vt[1]/bins)
  DT$wps_level <- ceiling(DT$wps_trunc/(vt[2]/bins))*(vt[2]/bins)
  DT$xps_level <- ceiling(DT$xps_trunc/(vt[3]/bins))*(vt[3]/bins)
  
  # splitDT <- split(DT,DT$svrid)
  # DT <- subset(DT,svrid %in% levels(DT$svrid)[1:100])
  
  r1 <- table(DT$svrid,DT$rps_level)
  r1 <- cbind(row.names(r1),as.data.frame.matrix(r1))
  names(r1)[1] <- 'svrid'
  
  r2 <- table(DT$svrid,DT$wps_level)
  r2 <- cbind(row.names(r2),as.data.frame.matrix(r2))
  names(r2)[1] <- 'svrid'
  
  r3 <- table(DT$svrid,DT$xps_level)
  r3 <- cbind(row.names(r3),as.data.frame.matrix(r3))
  names(r3)[1] <- 'svrid'
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(list(r1,r2,r3))
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc

dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'count_bandwidth_value',frac_cores = 0.9,vt=value_trunc,bins=100)

rr <- lapply(1:3,function(i){
  rrr <- lapply(r,'[[',i)
  rrr <- lapply(rrr,function(df)complete_column(df,as.character(seq(0,value_trunc[i],value_trunc[i]/100))))
  rrr <- do.call(rbind,rrr)
  names(rrr)[2:ncol(rrr)] <- paste('V',names(rrr)[2:ncol(rrr)],sep='')
  return(rrr)
})

rps_value_count <- rr[[1]]
wps_value_count <- rr[[2]]
xps_value_count <- rr[[3]]
save(rps_value_count,wps_value_count,xps_value_count,file = file.path(dir_data,sprintf('count_bandwidth_value_trunc_%iadc.Rda',time_trunc)))
