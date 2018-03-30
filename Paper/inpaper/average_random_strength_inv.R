#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_random_strength.R
#
# Description: 
# We present the distribution of the strength of request random and correlate it and the failure rate
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-07-05 16:32:08
#
# Last   modified: 2017-09-24 17:08:17
#
#
#

rm(list = ls());source('~/rhead');setwd(file.path(dir_c,'Disk_Workload/Paper'));
source('dir_func.R')

gen_result_random_strength <- function(i,all=F){
  cat(sprintf('[%s]\t %s SATRT!!!\n',date(),i))
  r <- gen_result_feature(DT_quan,col_name[i],quantile(DT_quan[[col_name[i]]],0.99))
  if(all){
    return(r[1:4])
  }else{
    return(r[[4]])
  }
}

# S1. Load Data ------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))
DT_raw <- quan_random

am <- 1
# thresholds<- c(10,20,50,100,200)
# attr_main <- paste('T',thresholds,sep='')
# for(i in seq_len(length(thresholds))){
#   df_frac <- get_quan_percentage(DT_raw,thresholds[i])
#   DT_raw[[attr_main[i]]] <- 100-df_frac$fraction[match(DT_raw$svrid,df_frac$svrid)]*100
# }
# col_name <- setdiff(names(DT_raw),c('svrid','count'))

DT_quan <- gen_data(DT_raw[,c('svrid','mean_inv')],expand=T)
DT_quan <- subset(DT_quan,numD==12) # update 2018-01-04
names(DT_quan)[which(names(DT_quan)=='mean_inv')] <- 'mean'

# S2. generate result ------
list[data_fr,p_fr,p_count,corr,data_ob,data_f] <- gen_result_feature(DT_quan,'mean',bins = 20,attr_max = am,smooth=T)
# list[data_fr.abw,p_fr.abw,p_count.abw,corr.abw,data_all.abw] <- gen_result_feature(DT_quan,'abw',bins = 40,attr_max = 9000)
# list[data_fr.adc,p_fr.adc,p_count.adc,corr.adc,data_all.adc] <- gen_result_feature(DT_quan,'adc',bins = 40,attr_max = 100)

# S3. divide by quantile
# DT_quan <- binning_data(DT_quan,'mean',am,bins=20)
# splitDTQ <- split(DT_quan,DT_quan$mean_level)
# list[data_fr_adc,div_adc] <- gen_fr_split(splitDTQ=splitDTQ,attrQ='adc',attrX='mean',Q=0.33)
# list[data_fr_abw,div_abw] <- gen_fr_split(splitDTQ=splitDTQ,attrQ='abw',attrX='mean',Q=0.33)
# lr <- glm(AFR~mean_level,data = data_fr,family = 'gaussian')
# summary(lr)

# S_plot ------
p_rs_fr <- p_fr +xlab('ASI')+gen_theme()+theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))+ xlim(c(0,am-1))

# p_rs_dist <- p_count+xlab('ASI')+ xlim(c(0,am-1)) + ylab('Percentage (%)')
# 
# p_rs_splitadc <- ggplot(data_fr_adc,aes(x=mean_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
#   guides(linetype=guide_legend(title='ADC (%)',keywidth = 5),color=guide_legend(title='ADC (%)')) + 
#   xlab('ASI') + ylab('AFR (%)') + gen_theme()+
#   theme(legend.position = c(0.95,0.95),legend.justification = c(1,1),legend.background = element_rect(fill = alpha('grey',0.5)))
# 
# p_rs_splitabw <- ggplot(data_fr_abw,aes(x=mean_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
#   guides(linetype=guide_legend(title='ABW (KB/s)',keywidth = 5),color=guide_legend(title='ABW (KB/s)')) + 
#   xlab('ASI') + ylab('AFR (%)') + gen_theme()+
#   theme(legend.position = c(0.95,0.95),legend.justification = c(1,1),legend.background = element_rect(fill = alpha('grey',0.5)))
# 
# multiplot(plotlist = list(p_rs_splitabw,p_rs_splitadc))
# 
# p_rs_adc.quantile <- plot_relationship_quantile(data_all.adc,'adc_level','mean')+
#   ylab('ASI')+xlab('ADC (%)')+theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))##
# 
# p_rs_abw.quantile <- plot_relationship_quantile(data_all.abw,'abw_level','mean')+
#   ylab('ASI')+xlab('ABW (KB/s)')+theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))##
# 
# save_fig(p_rs_fr,'rs_fr')
# save_fig(p_rs_dist,'rs_dist')
# save_fig(p_rs_splitadc,'rs_fr_adc')
# save_fig(p_rs_splitabw,'rs_fr_abw')
# save_fig(p_rs_abw.quantile,'rs_dist_abw')
# save_fig(p_rs_adc.quantile,'rs_dist_adc')
# 
# 
# save(p_rs_fr,p_rs_dist,file = file.path(dir_data,'Paper','Rda','p_rs.Rda'))
