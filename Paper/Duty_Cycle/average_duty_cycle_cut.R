#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_duty_cycle_cut.R
#
# Description: I divide duty cycle into multiple classes by the quantile of duty cycle of each disk drive. 
# here, we show the difference and failure rate for each classes
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-17 23:40:22
#
# Last   modified: 2017-08-17 23:40:24
#
#
#



rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))

# load(file.path(dir_data,'average_duty_cycle_cut.Rda'));cut_point <- seq(0,1,0.25)
# load(file.path(dir_data,'average_duty_cycle_cut80.Rda'));cut_point <- seq(0.8,1,0.05)
load(file.path(dir_data,'average_duty_cycle_cut(0-10-100).Rda'));cut_point <- seq(0,1,0.1)

n <- paste('Q',cut_point[-length(cut_point)]*100,sep='')
# n <- paste(rep(n,each=3),rep(c('L','M','R'),3),sep='')

itv <- 5
data_fr <- list();p_fr <- list();p_count <- list();p_countF <- list()
for(x in n){
  xl <- paste(x,'level',sep='_')
  r[[xl]] <- round(r[[x]]/itv)*itv
  list[data_fr[[x]],p_fr[[x]],p_count[[x]],p_countF[[x]]] <- gen_fr(r,attr=xl,prt=F)
  cat(sprintf('%s %f\n',x,sd(nl$count)))
}

multiplot(plotlist = p_fr,layout = matrix(1:12,nrow=3,byrow = T))
multiplot(plotlist = p_diff,layout = matrix(1:12,nrow=3,byrow = T))

