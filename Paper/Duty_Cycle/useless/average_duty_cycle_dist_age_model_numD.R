#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_duty_cycle_dist_age_model_numD.R
#
# Description: We find disk is able to group into three groups based on the relationship between average duty cycle and the failure rate.
# We call them adc-low, adc-median and adc-high. The failure rate of adc-median has a negative relationship with the average duty cycle. 
# The failure rate of the other two groups is consistant with the average duty cycle.
# Here, we find difference in these groups in order to expose the reason of failure rate trend, especially the negative relationship of the adc-median
# We make analysis on the distribution of duty cycle/age/disknumber/diskmodel
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-20 16:44:24
#
# Last   modified: 2017-08-23 19:59:42
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))

# S1. AMD ------------------------------------------------------------------------
itv <- 5
object_data <- io14
object_data$average_duty_cycle <- with(object_data,sum_util/count)
object_data$average_duty_cycle_level <- floor(object_data$average_duty_cycle/itv)*itv
list[object_data,fail_data,fr_data] <- gen_data(object_data,'average_duty_cycle_level')
p_amd_adc <- plot_amd_diff(object_data,'average_duty_cycle_level')
multiplot(plotlist = p_amd_adc[2:4],cols=3)
p_amd_adc[[2]]

# S2. adc distribution ------------------------------------------------------------------------
quantile_CDF <- function(df,col_quan,attr){
  rmean <- melt(t(aggregate(df[,col_quan],by = list(df[[attr]]),mean)))
  rsd <- melt(t(aggregate(df[,col_quan],by = list(df[[attr]]),sd)))
  rplot <- merge(rmean,rsd,by = c('Var1','Var2'))
  names(rplot) <- c('quantile','class','mean','sd')
  
  rplot <- subset(rplot,!grepl('Group',quantile))
  rplot$class <- factor(rplot$class)
  rplot$mean <- fct2num(rplot$mean)
  rplot$sd <- fct2num(rplot$sd)
  rplot$quantile <- as.numeric(gsub('X','',fct2ori(rplot$quantile)))
  
  p_diff <- ggplot(rplot, aes(x=quantile)) + 
    geom_line(aes(y=mean,group = class, colour=class))+
    geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,group = class,fill=class), alpha=0.2)+
    xlab('Percentage(%)')+coord_flip()+
    guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  return(list(p_diff,rplot))
}
col_quan <- paste('X',seq(0,100,5),sep='')
object_data <- r[,c('svrid',col_quan)]
io14$ave_duty_cycle <- floor(with(io14,sum_util/count))
object_data$ave_duty_cycle <- io14$ave_duty_cycle[match(object_data$svrid,io14$svrid)]
object_data$class <- 'Low-adc'
object_data$class[object_data$ave_duty_cycle > 30] <- 'Median-adc'
object_data$class[object_data$ave_duty_cycle > 45] <- 'High-adc'
# object_data[,col_quan] <- t(scale(t(object_data[,col_quan])))
list[data_fr,p1,p2,p3,object_data,data1] <- gen_fr(object_data,'class',prt = F)
list[p_diff_adc,rplot] <- quantile_CDF(object_data,col_quan,'class')
p_sd <- ggplot(rplot,aes(x=quantile,y=sd,group=class,color=class)) + geom_line()
multiplot(p_diff_adc,p_sd,cols=2)
