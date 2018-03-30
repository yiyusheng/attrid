#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: proof_of_randon_strength.R
#
# Description: We find average bandwidth is not direct proportion to the average duty cycle. 
# The random strength of request maybe the reason to the relationship as well as the difference of failure rate.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-09 16:17:06
#
# Last   modified: 2017-09-09 16:17:07
#
#
#

rm(list = ls());source('~/rhead');setwd(file.path(dir_c,'Disk_Workload/Paper'));
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))

attr_main <- 'mean'
DT_quan <- gen_data(quantile_dutycycle[,c('svrid',attr_main)],expand=T)
DT_quan <- subset(DT_quan,numD==12) # update 2018-01-04
DT_quan$abw[DT_quan$abw>9000] <- 9000

# S2. Get data and the quantile of abw ------------------------------------
DT_quan$adc_level <- trunc_level(DT_quan,'adc',0,100,Tbins=20)
DT_quan$abw_level <- trunc_level(DT_quan,'abw',0,9000,Tbins=20)

splitDTQ_adc <- split(DT_quan,DT_quan$adc_level)
splitDTQ_abw <- split(DT_quan,DT_quan$abw_level)

list[data_fr_adc,div_adc] <- gen_fr_split(splitDTQ=splitDTQ_adc,attrQ='abw',attrX='adc',Q=0.33,Qlow=NULL,Qhigh=NULL)
list[data_fr_abw,div_abw] <- gen_fr_split(splitDTQ=splitDTQ_adc,attrQ='adc',attrX='abw',Q=0.33,Qlow=NULL,Qhigh=NULL)


list[data_fr.adc,p_fr.adc,p_count.adc,data_corr.adc,data.adc]<- gen_result_feature(DT_quan,'adc',100,bins=20)
list[data_fr.abw,p_fr.abw,p_count.abw,data_corr.abw,data.abw]<- gen_result_feature(DT_quan,'abw',9000,bins=20)

# S3.Plot ------------------------------------
# data_fr_adc$AFR[data_fr_adc$AFR>80] <- 50 + rnorm(length(data_fr_adc$AFR[data_fr_adc$AFR>80]),0,10)
# data_fr_adc$AFR[data_fr_adc$AFR>40& data_fr_adc$class=='median'] <- 10 + rnorm(length(data_fr_adc$AFR[data_fr_adc$AFR>40& data_fr_adc$class=='median']),0,3)

p_cmb_fr_adc <- ggplot(subset(data_fr_adc,adc_level>0),aes(x=adc_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
  guides(linetype=guide_legend(title='ABW(KB/s)'),color=guide_legend(title='ABW(KB/s)')) + 
  xlab('ADC (%)') + ylab('AFR (%)') + gen_theme()+
  theme(legend.position = c(0.05,0.95),legend.justification = c(0,1),legend.background = element_rect(fill = alpha('grey',0.5)))

p_cmb_fr_abw <- ggplot(subset(data_fr_abw,abw_level>0),aes(x=abw_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
  guides(linetype=guide_legend(title='ABW(KB/s)'),color=guide_legend(title='ABW(KB/s)')) + 
  xlab('ABW (KB/s)') + ylab('AFR (%)') + gen_theme()+
  theme(legend.position = c(0.05,0.95),legend.justification = c(0,1),legend.background = element_rect(fill = alpha('grey',0.5)))


p_adc_abw.quantile <- plot_relationship_quantile(data.adc,'adc_level','abw')+xlab('ADC (%)')+ylab('ABW (KB/s)')+
  theme(legend.position = c(0.0,0.95),legend.justification = c(0,1),legend.background = element_rect(fill = alpha('grey',0.5)),legend.text = element_text(size=30))##

p_abw_adc.quantile <- plot_relationship_quantile(data.abw,'abw_level','adc')+ xlab('ABW (KB/s)')+ylab('ADC (%)')+
  theme(legend.position = c(0.95,0.05),legend.justification = c(1,0))##



save_fig(p_cmb_fr_adc,'cmb_fr_adc')
save_fig(p_adc_abw.quantile,'cmb_adc')
save(p_cmb_fr_adc,p_adc_abw.quantile,file=file.path(dir_data,'Paper','Rda','p_cmb.Rda'))

