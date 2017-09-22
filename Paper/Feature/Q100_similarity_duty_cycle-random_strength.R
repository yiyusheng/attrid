#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: Q100_similarity_dutycycle_xps.R
#
# Description: compare the similarity of Q100(duty_cycle) and Q100(random strength) because they both have strongly relationship with the failure rate.
# To confirm they are not the same, we extract the max value of duty cycle and the rs.
# Because the number of value equals to the Q100 may be more than 1. so we compute the ratio of intersecter set number to the small set
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-31 15:20:48
#
# Last   modified: 2017-08-31 15:20:50
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

Q100_similarity <- function(i,vt){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  
  DT <- format_bandwidth(DT,vt,bins = 100)
  DT <- factorX(subset(DT,wps_trunc!=0))
  DT$rs <- DT$util/DT$xps_trunc
  
  splitDT <- split(DT,DT$svrid)
  
  r <- lapplyX(splitDT,function(df){
    ind_dc <- which(df$util==max(df$util))
    ind_rs <- which(df$rs==max(df$rs))
    ind_inter <- intersect(ind_dc,ind_rs)
    
    len_idc <- length(ind_dc)
    len_irs <- length(ind_rs)
    len_iit <- length(ind_inter)
    c(len_iit/min(len_idc,len_irs),len_idc,len_irs,len_iit)
  })
  r <- cbind(row.names(r),data.frame(r))
  names(r)[1] <- 'svrid'
  r$svrid <- fct2ori(r$svrid)
  
  cat(sprintf('[%s]\t END!!!\n',fn))
  return(r)
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
value_trunc <- c(4000,5000,9000)*time_trunc
dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'Q100_similarity',frac_cores = 0.9,vt=value_trunc)
Q100_similarity <- setNames(do.call(rbind,r),nm = c('svrid','similarity','len_dc','len_rs','len_inter'))
save(Q100_similarity,file = file.path(dir_data,'Q100_similarity.Rda'))

# S1. confirm the similarity
list[ob] <- gen_data(Q100_similarity,'similarity')
print(sum(ob$similarity==0)/nrow(ob))
