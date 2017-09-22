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
gen_corr <- function(i,only_corr=T){
  cat(sprintf('[%s] No.%f START!!!\n',date(),i))
  col_attr <- paste('L',i,sep='')
  list[data_fr,p_fr,p_count] <- gen_result_feature(DT_quan,col_attr,100)
  corr <- cor(data_fr[,1],data_fr$AFR)
  if(only_corr){
    return(data.frame(i,corr))
  }else{
    return(list(data_fr,p_fr))
  }
}
step <- 0.0001
div <- c(seq(0,0.98,0.001),seq(0.99,1,step))
# div <- c(seq(0.99,1,step))

idx <- div/step
# system.time(r <- foreachX(idx,'gen_corr',frac_cores = 0.9))
r <- lapply(idx,gen_corr)
corr_largedc <- setNames(do.call(rbind,r),nm=c('level_adc','corr'))

r1 <- lapply(c('L5000','L8000','L9500','L9990','L10000'),function(l)gen_result_feature(DT_quan,l,100,bins = 20))
data_fr_list <- lapply(r1,'[[',1)
p_fr_list <- lapply(r1,'[[',2)
p_count_list <- lapply(r1,'[[',3)

# S.plot ------------------------------------
p_hr_corr <- ggplot(subset(corr_largedc,level_adc>0),aes(x=level_adc/100,y=corr))+geom_line(size=0.5)+geom_point(size=2)+
  xlab('The Quantile(%)')+ylab('The Correlation Coefficient')+
  guides(fill = guide_legend(title=NULL),color=guide_legend(title='interval')) +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.title = element_text(size=24),legend.position = 'bottom')

p_hr_q999 <- p_fr_list[[4]]+xlab('The Average Duty Cycle(%)')
p_hr_q1000 <- p_fr_list[[5]]+xlab('The Average Duty Cycle(%)')

save_fig(p_hr_corr,'hr_corr')
save_fig(p_hr_q999,'hr_q9990')
save_fig(p_hr_q1000,'hr_q10000')
save(p_hr_corr,p_hr_q9990,p_hr_q10000,file=file.path(dir_data,'Paper','Rda','p_hr.Rda'))
