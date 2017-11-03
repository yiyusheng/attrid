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

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
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

thresholds<- c(10,20,50,100,200)
attr_main <- paste('T',thresholds,sep='')
for(i in seq_len(length(thresholds))){
  df_frac <- get_quan_percentage(DT_raw,thresholds[i])
  DT_raw[[attr_main[i]]] <- 100-df_frac$fraction[match(DT_raw$svrid,df_frac$svrid)]*100
}
col_name <- setdiff(names(DT_raw),c('svrid','count'))

DT_quan <- gen_data(DT_raw[,c('svrid','mean')],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# S2. generate result ------
# idx <- seq_len(length(col_name))
# r <- foreachX(idx,'gen_result_random_strength',outname = -1)
# idx <- which(col_name %in% c('mean','sd',attr_main,paste('Q',c(1:3,20,95),sep='')))
# r <- lapply(idx,gen_result_random_strength,all=T)
# corr <- unlist(r)
# plot(corr)

list[data_fr,p_fr,p_count,corr,data_ob,data_f] <- gen_result_feature(DT_quan,'mean',bins = 20,attr_max = 400)

# S3. divide by quantile
# DT_quan <- binning_data(DT_quan,'mean',400,balanced_binning=F,bin_count = 10000)
DT_quan <- binning_data(DT_quan,'mean',400,bins=20)
splitDTQ <- split(DT_quan,DT_quan$mean_level)
list[data_fr_adc,div_adc] <- gen_fr_split(0.25,'adc',splitDTQ,quantile(DT_quan$adc,0.33),quantile(DT_quan$adc,0.66))
list[data_fr_abw,div_abw] <- gen_fr_split(0.25,'abw',splitDTQ,quantile(DT_quan$abw,0.33),quantile(DT_quan$abw,0.66))

list[data_fr_adc,div_adc] <- gen_fr_split(0.25,'adc',splitDTQ)
list[data_fr_abw,div_abw] <- gen_fr_split(0.25,'abw',splitDTQ)

# S4. change in bin [160-180]
a <- subset(data_ob,mean_level==140)
b <- subset(data_ob,mean_level==180 )
ggplot(a,aes(x=abw,fill=numD))+geom_histogram()+xlim(c(0,10000))
ggplot(b,aes(x=abw,fill=numD))+geom_histogram()+xlim(c(0,10000))

# S5. linear regression
lr <- glm(AFR~mean_level,data = data_fr,family = 'gaussian')
summary(lr)

# S_plot ------
p_rs_fr <- p_fr +xlab('ASI')+gen_theme()+theme(legend.position = c(0.95,0.95),legend.justification = c(1,1))+
  scale_fill_manual(values=c('grey60','grey20')) + xlim(c(0,399)) + ylab('Failure Rate (%)')

p_rs_dist <- p_count+xlab('ASI')+ xlim(c(0,399)) + ylab('Percentage (%)')

p_rs_splitadc <- ggplot(data_fr_adc,aes(x=mean_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
  guides(linetype=guide_legend(title='ADC (%)'),color=guide_legend(title='ADC (%)')) + 
  xlab('ASI') + ylab('Failure Rate (%)') + gen_theme()+
  theme(legend.position = c(0.95,0.95),legend.justification = c(1,1),legend.background = element_rect(fill = alpha('grey',0.5)))

p_rs_splitabw <- ggplot(data_fr_abw,aes(x=mean_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
  guides(linetype=guide_legend(title='ABW (KB/s)'),color=guide_legend(title='ABW (KB/s)')) + 
  xlab('ASI') + ylab('Failure Rate (%)') + gen_theme()+
  theme(legend.position = c(0.95,0.95),legend.justification = c(1,1),legend.background = element_rect(fill = alpha('grey',0.5)))

save_fig(p_rs_fr,'rs_fr')

save_fig(p_rs_dist,'rs_dist')

save_fig(p_rs_splitadc,'rs_splitadc')

save_fig(p_rs_splitabw,'rs_splitabw')

save(p_rs_fr,p_rs_dist,file = file.path(dir_data,'Paper','Rda','p_rs.Rda'))
