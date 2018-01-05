#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: cng_smart_info.R
#
# Description: information about the changing of SMART
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-30 09:32:52
#
# Last   modified: 2017-11-30 09:32:53
#
#
#

# S1. Load function and data ------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'change_SMART.Rda'))
load(file.path(dir_dataSMT,'sta_disk_model.Rda'))
cng_smart_list <- split(cng_smart,cng_smart$interval)

# I1. ratio of disk drives with changed attributes
sum_change_time <- lapply(col_smart,function(s){
  tapply(cng_smart[[s]],cng_smart$sn,function(arr)min(4,sum(arr!=0 & !is.na(arr))))
})
sum_change_time <- setNames(data.frame(do.call(cbind,sum_change_time)),nm = col_smart)
sum_change_time$sn <- row.names(sum_change_time)
sum_change_time$svrid <- sta_ss$svrid[match(sum_change_time$sn,sta_ss$sn)]
table_numD <- melt(table(sum_change_time$svrid))
sum_change_time$numD <- table_numD$value[match(sum_change_time$svrid,table_numD$Var1)]

ratio_change_time <- melt(round(apply(sum_change_time[,col_smart],2,function(arr)sum(arr>0))/nrow(sum_change_time),digits = 6))
ratio_change_time$attr <- row.names(ratio_change_time)
ratio_change_time$attr <- factor(ratio_change_time$attr,levels=col_smart)
ggplot(ratio_change_time,aes(x=attr,y=value))+geom_bar(stat='identity',position = 'dodge')+
  theme(axis.text.x =  element_text(angle=-30,size=8))

# I2. failure rate for disks with/without smart changing
fr_change <- lapply(col_smart,function(s){
  df <- sum_change_time[,c('svrid','sn',s)]
  
  # 1. select number of sn from a server to be failed randomly as the real failed times
  table_fail <- melt(table(f201409$svrid))
  idx_fail <- table_fail$value[match(df$svrid,table_fail$Var1)]
  idx_fail[is.na(idx_fail)] <- 0
  idx_fail <- tapply(idx_fail,df$svrid,function(arr){
    if(arr[1] == 0)return(arr)
    if(arr[1]<length(arr)){
      idx_one <- sample(seq_len(length(arr)),arr[1])
      arr[idx_one] <- 1
      arr[-idx_one] <- 0
      return(arr)
    }else{
      arr[1==1] <- 1
      return(arr)
    }
  })
  df$fail <- unlist(idx_fail)
  
  # 2. all sn is labeled as fail when its server failed
  # df$fail <- 0
  # df$fail[df$svrid %in% f201409$svrid] <- 1
  
  
  df$change <- as.numeric(df[[s]]>0)
  c(nrow(df[df$fail==1 & df$change == 1,])/nrow(df[df$change==1,]),
    nrow(df[df$fail==1 & df$change == 0,])/nrow(df[df$change==0,]))
})
fr_change <- setNames(data.frame(do.call(rbind,fr_change)),nm=c('changed','unchanged'))
fr_change$attr <- factor(col_smart,levels=col_smart)
ggplot(melt(fr_change,id.vars = 'attr'),aes(x=attr,y=value,fill=variable))+geom_bar(stat='identity',position = 'dodge')+
  theme(axis.text.x =  element_text(angle=-30,size=8))
  
# I3. amplitude of smart attributes and failure rate
cng_smart_itv20 <- subset(cng_smart,interval==20)
cng_smart_itv20$svrid <- sta_ss$svrid[match(cng_smart_itv20$sn,sta_ss$sn)]
fr_amp <- lapply(col_smart,function(s){
  df <- cng_smart_itv20[,c('svrid','sn',s)]
  df[[s]][is.na(df[[s]])] <- 0
  df$level <- 0.01
  df$level[df[[s]]!=0] <- trunc_level(df[df[[s]]!=0,],s,min_value = -20,max_value = 20)
  df$level[df$level==0] <- 1
  df$level[df$level==0.01] <- 0
  
  df$fail <- 0
  df$fail[df$svrid %in% f201409$svrid] <- 1
  r <- tapply(df$fail,df$level,function(arr)roundX(sum(arr)/length(arr)))
})


# I2. change of disk in the same server[unfinish]
sct12 <- subset(sum_change_time,numD==12)