#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: individual_difference-util.R
#
# Description: 
# first, we prove the individual workload difference of disk drives by showing distribution of duty cycle of all disks
# then, we give the failure rate of each type of duty cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 16:32:08
#
# Last   modified: 2017-07-21 14:47:27
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))

plot_util_bs <- function(DT,ind,upbound_util = 10){
  dist_util <- replace_value(DT)
  names(dist_util)[names(dist_util)=='svrnum'] <- 'svrid'
  table_bs <- melt(table(cmdbSMP$bs1))
  table_bs <- table_bs[order(table_bs$value,decreasing = T),]
  
  dist_bs <- sapply(ind,function(i){
    if(i > 0){
      bs <- fct2ori(table_bs$Var1[i])
      bs <- factorX(subset(dist_util,svrid %in% cmdbSMP$svrid[cmdbSMP$bs1 == bs]))      
    }else{
      bs <- factorX(subset(dist_util,svrid %in% cmdbSMP$svrid))   
    }
    
    bs$util <- with(bs,sum_util/count)
    # bs <- subset(bs,util <= upbound_util)
    cutseq <- seq(0,50,0.02)
    bs$cut <- fct2num(cut(bs$util,c(cutseq,100),cutseq,include.lowest = T))
    bsDisk <- svrid_expand_disk(bs)
    
    table_util <- setNames(melt(table(bsDisk$cut)),c('util','count'))
    table_util$fraction <- array_rate(table_util$count)
    p <- ggplot(table_util,aes(x = util)) + geom_bar(aes(y = fraction),stat = 'identity') + scale_x_continuous(breaks=seq(0,10,1))
    return(list(p,bsDisk))
  })
  return(dist_bs)
}


# S1. [FILTER FOR AGE MODEL and NUMD] 
io_svrid <- io14
io_svrid <- mchAttr(io_svrid,model_svrid,'svrid','svrid',c('numD','mainModel'))
io_svrid$age <- cmdbSMP$age[match(io_svrid$svrid,cmdbSMP$svrid)]
list[p_svrid,bsDisk_svrid] <- plot_util_bs(io_svrid,-1)

#[***data: to prove the difference of workload***]
rate_svrid <- setNames(melt(table(round(bsDisk_svrid$cut))),c('util','count'))
rate_svrid$rate <- array_rate(rate_svrid$count)
rate_svrid$standrate <- with(rate_svrid,rate/rate[2])
rate_svrid$count_rate <- array_rate(rate_svrid$count)
rate_svrid$count_rate_cumsum <- cumsum(rate_svrid$count_rate)

paper1 <- ggplot(rate_svrid,aes(x = util,y = count_rate)) + geom_bar(stat = 'identity') + 
  xlab('Mean Duty Cycle of disk drives(%)') + ylab('Percentage(%)') +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.position = 'top')
ggsave(file=file.path(dir_data,'Paper','Dist_duty_cycle.eps'), plot=paper1, width = 8, height = 6, dpi = 100)
#[******]

bsDisk_svrid$level <- 'low'
bsDisk_svrid$level[bsDisk_svrid$util > 5] <- 'median'
bsDisk_svrid$level[bsDisk_svrid$util > 10] <- 'high'
level_mean_util <- melt(tapply(bsDisk_svrid$util,bsDisk_svrid$level,mean))
level_count <- melt(tapply(bsDisk_svrid$util,bsDisk_svrid$level,length))
level_svrid <- setNames(merge(level_mean_util,level_count,by='Var1'),c('level','util','count'))
level_svrid$rate <- array_rate(level_svrid$count)

# S2. failure rate of difference utilization.
util_fr_cmdb <- bsDisk_svrid
util_fr_cmdb$util_round <- round(util_fr_cmdb$util)
util_fr_cmdb$util_round <- fct2num(cut(util_fr_cmdb$util,seq(0,100,2),seq(0,98,2),include.lowest = T))
util_fr_cmdb$util_round[util_fr_cmdb$util_round>60] <- 60

util_fr_f <- f201409
util_fr_f$util_round <- util_fr_cmdb$util_round[match(util_fr_f$svrid,util_fr_cmdb$svrid)]

#[***Present the failure rate of different utilization***]
util_fr <- ioAFR(util_fr_cmdb,util_fr_f,attr = 'util_round')
util_fr$count_rate <- array_rate(util_fr$count)
util_fr$level <- 'low'
util_fr$level[util_fr$AFR > 4.5] <- 'high'
paper2 <- ggplot(util_fr,aes(x = util_round,y = AFR,fill = level)) + geom_bar(stat = 'identity') + 
  xlab('Mean Duty Cycle of disk drives(%)') + ylab('Failure Rate(%)') + guides(fill = guide_legend(title=NULL)) +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.position = 'bottom')
ggsave(file=file.path(dir_data,'Paper','FailureRate_duty_cycle.eps'), plot=paper2, width = 8, height = 6, dpi = 100)
#[******]

multiplot(paper1,paper2,cols =2)

# S2. [useless]plot the trend of util,rps and wps
# sta_model$numD <- 0
# sta_model$numD[sta_model$numDisk <= 2] <- 1
# sta_model$numD[sta_model$numDisk >= 8 & sta_model$numDisk <= 16] <- 12
# 
# io_svrid <- replace_value(sta_day)
# names(io_svrid)[names(io_svrid) == 'svrnum'] <- 'svrid'
# # io_svrid <- mchAttr(io_svrid,sta_model,'svrid','svrid','numD')
# # io_svrid <- subset(io_svrid, count >= 250 & numD > 0)
# io_svrid <- subset(io_svrid, count >= 250)
# io_svrid$mean_util <- io_svrid$sum_util/io_svrid$count
# io_svrid$mean_wps <- io_svrid$sum_wps/io_svrid$count
# io_svrid$mean_rps <- io_svrid$sum_rps/io_svrid$count
# 
# io_svrid$svrid <- factor(io_svrid$svrid)
# io_svrid$time <- factor(io_svrid$date)
# DT_trend <- data.frame(time = as.p(levels(io_svrid$time)),
#                        mean_rps = as.numeric(tapply(io_svrid$mean_rps,io_svrid$time,mean)),
#                        mean_wps = as.numeric(tapply(io_svrid$mean_wps,io_svrid$time,mean)),
#                        mean_util = as.numeric(tapply(io_svrid$mean_util,io_svrid$time,mean)))
# p1 <- ggplot(DT_trend,aes(x = time)) + geom_point(aes(y = mean_rps)) + geom_line(aes(y = mean_rps,group=1)) + ggtitle('RPS')
# p2 <- ggplot(DT_trend,aes(x = time)) + geom_point(aes(y = mean_wps)) + geom_line(aes(y = mean_wps,group=1)) + ggtitle('WPS')
# p3 <- ggplot(DT_trend,aes(x = time)) + geom_point(aes(y = mean_util)) + geom_line(aes(y = mean_util,group=1)) + ggtitle('UTIL')
# multiplot(p1,p2,p3)
# S6. [useless]util distribution of fail and norm disks
# util_dist_cmdb <- bsDisk_svrid
# util_dist_cmdb$class <- 'normal'
# util_dist_f <- f201409
# util_dist_f$util <- util_dist_cmdb$util[match(util_dist_f$svrid,util_dist_cmdb$svrid)]
# util_dist_f$class <- 'failed'
# 
# cdf_util <- rbind(util_dist_cmdb[,c('util','class')],util_dist_f[,c('util','class')])
# ggplot(cdf_util,aes(x = util,group = class,color = class)) + stat_ecdf()