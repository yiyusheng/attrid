#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: high_rank.R
#
# Description: The high rank duty cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-15 15:32:43
#
# Last   modified: 2017-09-19 10:50:50
#
#
#

# S1. Load function and data ------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'adc_level_dutycycle.Rda'))

# S1. adc of duty cycle greater than a special quantile ------------------------------------
DT_quan <- gen_data(level_dutycycle,expand = T)
DT_quan <- subset(DT_quan,numD==12) # update 2018-01-04

# qname <- sort(c(seq(9000,9800,100),seq(9900,10000,10)))
qname <- c(seq(500,9000,500),seq(9300,10000,100))
lname <- paste('L',qname,sep='')
r <- lapply(lname,function(l)gen_result_feature(DT_quan,l,100,bins = 20))

data_fr_list <- lapply(r,'[[',1)
p_fr_list <- lapply(r,'[[',2)
p_count_list <- lapply(r,'[[',3)
corr_largedc <- data.frame(level_adc = qname/100,corr = sapply(r,'[[',4))

# S.plot ------------------------------------
p_hr_corr <- ggplot(subset(corr_largedc,level_adc>0),aes(x=level_adc/100,y=corr))+geom_line(size=0.5)+geom_point(size=2)+
  xlab('Quantile of Duty Cycle (%)')+ylab('Correlation Coefficient')+gen_theme() + ylim(c(0,1))

p_hr_qA <- p_fr_list[[which(lname=='L2500')]]+xlab('ADC (%)')+scale_fill_manual(values=c('grey80','grey20'))+coord_cartesian(ylim=c(0,15),xlim=c(0,105))
p_hr_qB <- p_fr_list[[which(lname=='L7500')]]+xlab('ADC (%)')+scale_fill_manual(values=c('grey80','grey20'))+coord_cartesian(ylim=c(0,15),xlim=c(0,105))
p_hr_qC <- p_fr_list[[which(lname=='L9500')]]+xlab('ADC (%)')+scale_fill_manual(values=c('grey80','grey20'))+coord_cartesian(ylim=c(0,15),xlim=c(0,105))
p_hr_qD <- p_fr_list[[which(lname=='L10000')]]+xlab('ADC (%)')+scale_fill_manual(values=c('grey80','grey20'))+coord_cartesian(ylim=c(0,15),xlim=c(0,105))

# data_fr <- data_fr_list[[which(lname=='L9000')]]
# png(filename = file.path(dir_data,'Paper','jpg','ahdc_fr.jpg'),width = 1920, height = 1080, bg = "white")
# multiplot(plotlist = p_fr_list,layout = matrix(1:20,byrow=T,nrow=4))
# dev.off()

save_fig(p_hr_corr,'hr_corr')
save_fig(p_hr_qA,'hr_qA')
save_fig(p_hr_qB,'hr_qB')
save_fig(p_hr_qC,'hr_qC')
save_fig(p_hr_qD,'hr_qD')
save(p_hr_corr,p_hr_qA,p_hr_qB,p_hr_qC,p_hr_qD,file=file.path(dir_data,'Paper','Rda','p_hr.Rda'))
