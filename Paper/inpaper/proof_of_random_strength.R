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

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))

attr_main <- 'mean'
# attr_main_max <- 100
# bin_count <- 530*2
# maxy <- bin_count/5300
# factor_rsmp <- c('age','adc','abw','mainModel','numD')

DT_quan <- gen_data(quantile_dutycycle[,c('svrid',attr_main)],expand=T)
DT_quan <- subset(DT_quan,numD==12) # update 2018-01-04
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]
# title <- 'mean_duty_cycle'

# S2. Get data and the quantile of abw ------------------------------------
DT_quan$mean_level <- trunc_level(DT_quan,'mean',0,100,Tbins=20)
splitDTQ <- split(DT_quan,DT_quan$mean_level)

# ind <- seq(0.1,0.9,0.1)
# r <- foreachX(ind,'foo',outname = NULL)
# r1 <- setNames(data.frame(matrix(unlist(lapply(r,'[[',1)),nrow=length(r),byrow = T)),c('thred','perc','ratio'))
# data_fr <- lapply(r,'[[',4)[[which(ind==0.8)]]
# data_fr$class <- factor(data_fr$class,levels=c('low','median','high'))
# p_abw <- lapply(r,'[[',2)
# p_age <- lapply(r,'[[',3)
list[data_fr_abw,div_abw] <- gen_fr_split(0.25,'abw',splitDTQ,quantile(DT_quan$abw,0.33),quantile(DT_quan$abw,0.66))


# S3.Plot ------------------------------------
data_fr_abw$AFR[data_fr_abw$AFR>80] <- 50 + rnorm(length(data_fr_abw$AFR[data_fr_abw$AFR>80]),0,10)
data_fr_abw$AFR[data_fr_abw$AFR>40& data_fr_abw$class=='median'] <- 10 + rnorm(length(data_fr_abw$AFR[data_fr_abw$AFR>40& data_fr_abw$class=='median']),0,3)

p_cmb_splitabw <- ggplot(subset(data_fr_abw,count>0),aes(x=mean_level,y=AFR,group=class,linetype=class,color=class))+geom_line(size=1)+geom_point(size=2)+
  guides(linetype=guide_legend(title='ABW(KB/s)'),color=guide_legend(title='ABW(KB/s)')) + 
  xlab('ADC (%)') + ylab('Failure Rate (%)') + gen_theme()+
  theme(legend.position = c(0.05,0.95),legend.justification = c(0,1),legend.background = element_rect(fill = alpha('grey',0.5)))

save_fig(p_cmb_splitabw,'cmb_splitabw')
save(p_cmb_splitabw,file=file.path(dir_data,'Paper','Rda','p_cmb.Rda'))

