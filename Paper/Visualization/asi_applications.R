#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_random_strength.R
#
# Description: 
# Try to find applications and generate the asi from them
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
# load(file.path(dir_data,''))
# S2. generate result ------
DT_raw <- quan_random
DT_raw$bs1 <- cmdbSMP$bs1[match(DT_raw$svrid,cmdbSMP$svrid)]
DT_quan <- gen_data(DT_raw[,c('svrid','mean','bs1')],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]

# list[data_fr,p_fr,p_count,corr,data_ob,data_f] <- gen_result_feature(DT_quan,'mean',bins = 20,attr_max = 400)
# lr <- glm(AFR~mean_level,data = data_fr,family = 'gaussian')
# summary(lr)

# S3. extract applications
table.appc <- melt(table(cmdbSMP$bs1))
data.f <- f201409
data.f$bs1 <- cmdbSMP$bs1[match(data.f$svrid,cmdbSMP$svrid)]
table.appf <- melt(table(data.f$bs1))
table.app <- setNames(merge(table.appc,table.appf,by = 'Var1'),nm = c('bs','count','fcount'))

list[fr,object_data,fail_data] <- gen_data(DT_quan,'bs1',io14,f201409,expand = F)
table.5attr <- aggregate(object_data[,c('adc','abw','age','mean','dcq100')],list(object_data$bs1),function(x)roundX(mean(x)))
fr <- mchAttr(fr,table.5attr,'bs1','Group.1',c('adc','abw','age','mean','dcq100'))
fr.app <- subset(fr,count>30000)

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
