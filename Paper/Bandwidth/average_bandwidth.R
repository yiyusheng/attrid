#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: individual_difference-util.R
#
# Description: 
# We present the distribution of xps(rps+wps) and correlate xps and failure rate
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
source('dir_func.R')

load(file.path(dir_data,'uniform_data.Rda'))

attr_bw <- data.frame(attr = c('rps','wps','xps'),maxlimit = c(4000,5000,9000))
object_data <- add_average_bandwidth(io14, attr_bw)

# S1. Distribution and Failure Rate (rps,wps,xps) ------------------------------------
data_fr_list <- list();p_fr_list <- list();p_count_list <- list();data_ob_list <- list()
for (i in seq_len(nrow(attr_bw))){
  ave_level_name <- paste('average',attr_bw$attr[i],'level',sep='_')
  list[data_fr_list[[i]],p_fr_list[[i]],p_count_list[[i]],p1,data_ob_list[[i]],xl3] <- gen_fr(object_data,attr=ave_level_name,prt = F,countLimit = 100)
  trunc_value <- data_fr_list[[i]]$percentage[2:4]
  
  p_fr_list[[i]] <- p_fr_list[[i]] + xlab(paste(attr_bw$attr[i],'(kB/s)')) + ylim(c(0,15))+
    annotate("text", x=quantile(data_fr_list[[i]][,1],0.2), y=9, size = 8,label= sprintf('corr:%.2f',cor(data_fr_list[[i]][,1],data_fr_list[[i]]$AFR)))
  
  p_count_list[[i]] <- p_count_list[[i]] + xlab(paste(attr_bw$attr[i],'(kB/s)'))+coord_cartesian(ylim=c(0,5))+
    annotate("text", x=quantile(data_fr_list[[i]][,1],0.5), y=4, size = 8,label= sprintf("(%.2f%%, %.2f%%, %.2f%%)",trunc_value[1],trunc_value[2],trunc_value[3]))
  
  data_fr_list[[i]]$cumsum_perc <- cumsum(data_fr_list[[i]]$percentage)
}

# S2. Extract data and plot distribution and Failure Rate------------------------------------
dfr1 <- data_fr_list[[1]];dfr2 <- data_fr_list[[2]];dfr3 <- data_fr_list[[3]]
quan50 <- c(quantile(data_ob_list[[3]]$average_rps,.5),quantile(data_ob_list[[1]]$average_wps,.5),quantile(data_ob_list[[1]]$average_xps,.5))
pearson_coe <- sapply(data_fr_list,function(df)cor(df[,1],df$AFR))
multiplot(plotlist=c(p_count_list,p_fr_list),layout = matrix(seq_len(6),nrow=2,byrow = T))

# S3. plot age/model/disknumber ------------------------------------------------------------------------
col_bw <- c('rps','wps','xps')
attr_bw <- data.frame(attr = col_bw,maxlimit = c(4000,5000,9000))
object_data <- add_average_bandwidth(io14, attr_bw)
list[expand_object_data,fail_data,fr_data] <- gen_data(object_data,'average_xps_level')
p_amd_rps <- plot_amd_diff(expand_object_data,'average_rps_level')
p_amd_wps <- plot_amd_diff(expand_object_data,'average_wps_level')
p_amd_xps <- plot_amd_diff(expand_object_data,'average_xps_level')
multiplot(plotlist=c(p_amd_rps[2:4],p_amd_wps[2:4],p_amd_xps[2:4]),layout=matrix(seq_len(9),nrow=3,byrow = F))
