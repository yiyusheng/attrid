#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_util_level.R
#
# Description: statistic value of util into percentege of 10%
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-01 15:48:02
#
# Last   modified: 2017-03-01 15:48:03
#
#
#

rm(list = ls());source('head.R')

sta_util_level <- function(i){
  load(file.path(dir_datatendcast,fname[i]))
  dt_dcast <- iops_dcast_clear(dt_dcast)
  r_sul <- tapply(dt_dcast$util,dt_dcast$svrid,function(x){
    # melt(table(cut(x,breaks = c(seq(-1,9,1),seq(10,90,10),seq(91,100,1)),
                   # labels = c(seq(0,9,1),seq(10,90,10),seq(91,100,1)))))
    melt(table(cut(x,breaks = -1:100,labels = 0:100)))
  })
  r_sul1 <- do.call(rbind,r_sul)
  r_sul1$svrid <- gsub('\\..*','',row.names(r_sul1))
  r_sul2 <- dcast(svrid~Var1,data = r_sul1,value.var = 'value')
  r_sul2$svrid <- factor(r_sul2$svrid)
  r_sul2
}

###### MAIN STA ######
# fname <- list.files(dir_datatendcast)
# require(doParallel)
# idx <- seq_len(length(fname))
# ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_sul')
# registerDoParallel(ck)
# r <- foreach(i = idx,.verbose = F) %dopar% sta_util_level(i)
# stopCluster(ck)
# 
# sta_util_level <- do.call(rbind,r)
# sta_util_level$svrid <- sort_level(sta_util_level$svrid)
# sta_util_level <- sta_util_level[order(sta_util_level$svrid),]
# save(sta_util_level,file = file.path(dir_data,'sta_util_level.Rda'))

###### MAIN ANALYSIS ######
load(file.path(dir_data,'sta_util_level.Rda'))
load(file.path(dir_data,'r_sta.Rda'))

r_sta_svrid <- melt(tapply(r_sta$date,r_sta$svr_id,length));names(r_sta_svrid) <- c('svrid','numDate')
names(sta_util_level)[-1] <- paste('X',names(sta_util_level)[-1],sep='')
sul <- subset(sta_util_level,svrid %in% r_sta_svrid$svrid[r_sta_svrid$numDate > 300])
              
conv_row_percent <- function(df){
  apply(df,1,function(x)round(x/sum(x)*100,digits = 4))
}

util_mean_zero <- data.frame(svrid = sul$svrid,
                             mean_util = round(apply(sul[,-1],1,function(x)sum(x*(0:100))/sum(x)),digits = 2),
                             num_zero = sul$X0,
                             count = apply(sul[,-1],1,sum))
util_mean_zero$rate_zero <- util_mean_zero$num_zero/util_mean_zero$count
util_mean_zero$rz_cut <- cut(util_mean_zero$rate_zero,c(0,0.2,0.8,1),c(0,0.2,0.8),include.lowest = T)

ggplot(util_mean_zero,aes(x = num_zero,y = mean_util)) + geom_point(alpha = 0.01)
ggplot(subset(util_mean_zero,rate_zero > .01),aes(x = rate_zero)) + geom_histogram(binwidth = 0.001)
ggplot(subset(util_mean_zero,mean_util > 1),aes(x = mean_util)) + geom_histogram(binwidth = 0.1)
ggplot(util_mean_zero,aes(x = rz_cut)) + geom_boxplot(aes(y = mean_util),outlier.shape = NA) + geom_jitter(aes(y = mean_util),width = 0.2,alpha = 0.01)
