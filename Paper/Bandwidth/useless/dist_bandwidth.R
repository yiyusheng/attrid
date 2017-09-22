#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: dist_bandwidth.R
#
# Description: Distribution of bandwidth for different bandwidth_level by quantile
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-20 16:44:24
#
# Last   modified: 2017-08-28 15:40:10
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

load(file.path(dir_data,'uniform_data.Rda'))


# S1. Plot Distribution based on a attr ------------------------------------------------------------------------
quantile_CDF <- function(df,col_quan,attr){
  df[[attr]] <- factor(paste('V',df[[attr]],sep=''))
  
  rmean <- aggregate(df[,col_quan],by = list(df[[attr]]),mean)
  nm <- rmean$Group.1
  rmean <- setNames(data.frame(t(rmean[,-1])),nm=nm)
  rmean$Q <- row.names(rmean)
  rmean <- melt(rmean,id.vars = 'Q')

  rsd <- aggregate(df[,col_quan],by = list(df[[attr]]),mean)
  nm <- rsd$Group.1
  rsd <- setNames(data.frame(t(rsd[,-1])),nm=nm)
  rsd$Q <- row.names(rsd)
  rsd <- melt(rsd,id.vars = 'Q')
  
  rplot <- merge(rmean,rsd,by = c('Q','variable'))
  names(rplot) <- c('quantile','class','mean','sd')
  
  rplot <- subset(rplot,!grepl('Group',quantile))
  rplot$class <- factor(rplot$class)
  rplot$quantile <- as.numeric(gsub('Q','',rplot$quantile))
  
  p_diff <- ggplot(rplot, aes(x=quantile)) + 
    geom_line(aes(y=mean,group = class, colour=class))+
    geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,group = class,fill=class), alpha=0.2)+
    xlab('Percentage(%)')+coord_flip()+
    guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  return(list(p_diff,rplot))
}

# S1.1 load quantile ------------------------------------
time_abw <- 10000
load(file.path(dir_data,sprintf('quantile_bandwidth_truncate_%iabw.Rda',time_abw)))
col_quan <- paste('Q',seq(0,100,5),sep='')
object_dataA <- quan_xps[,c('svrid',col_quan)]

# S1.2 load average bandwidth ------------------------------------
attr_bw <- data.frame(attr = c('rps','wps','xps'),maxlimit = c(4000,5000,9000))
object_dataB <- add_average_bandwidth(io14, attr_bw,5)
object_dataB <- subset(object_dataB,average_wps_level!=0 & average_xps_level!=0)

# S1.3 merge them ------------------------------------
object_data <- merge(object_dataA,object_dataB)
object_data <- object_data[,c('svrid','fn',names(object_dataB)[grepl('level',names(object_dataB))],col_quan)]

# S1.4 generate data and figure ------------------------------------
list[object_data] <- gen_data(object_data,'average_xps_level')
list[p_diff_adc,rplot] <- quantile_CDF(object_data,col_quan,'average_xps_level')
p_sd <- ggplot(rplot,aes(x=quantile,y=sd,group=class,color=class)) + geom_line()
multiplot(p_diff_adc,p_sd,cols=2)
rplot_dcast <- dcast(quantile~class,data=rplot,value.var = 'mean')
