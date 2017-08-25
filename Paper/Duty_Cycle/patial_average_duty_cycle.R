#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: average_duty_cycle_cut.R
#
# Description: I divide duty cycle into multiple classes by the quantile of duty cycle of each disk drive. 
# here, we show the difference and failure rate for each classes
# [Partial ADC]
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
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))

# C1. configure A
load(file.path(dir_data,'average_duty_cycle_cut(0-10-100).Rda'));cut_point <- seq(0,1,0.1)
n <- paste('Q',seq(40,90,10),sep='')

# C2. configure B
# load(file.path(dir_data,'average_duty_cycle_cut.Rda'));cut_point <- seq(0,1,0.25)
# load(file.path(dir_data,'average_duty_cycle_cut80.Rda'));cut_point <- seq(0.8,1,0.05)
# load(file.path(dir_data,'average_duty_cycle_cut(0-5-10-20-80-90-95-100).Rda'));cut_point <- c(0,0.05,0.1,0.2,0.8,0.9,0.95,1)
# n <- paste('Q',cut_point[-length(cut_point)]*100,sep='')
# n <- paste(rep(n,each=3),rep(c('L','M','R'),3),sep='')
# n <- n[c(4,7,10,15,18,21)]

itv <- 5
data_fr <- list();p_fr <- list();p_count <- list();p_countF <- list()
for(x in n){
  xl <- paste(x,'level',sep='_')
  r[[xl]] <- ceiling(r[[x]]/itv)*itv
  list[data_fr[[x]],p_fr[[x]],p_count[[x]],p_countF[[x]]] <- gen_fr(r,attr=xl,prt=F)
  
  p_fr[[x]] <- p_fr[[x]]+ylim(c(0,20))+
    annotate("text", x=25, y=15, size = 8,label= sprintf('corr:%.2f',roundX(cor(data_fr[[x]][,1],data_fr[[x]]$AFR))))+
  
  cat(sprintf('%s %f\n',x,sd(data_fr[[x]]$count)))
}

multiplot(plotlist = p_fr,layout = matrix(seq_len(6),nrow=2,byrow = T))
multiplot(plotlist = p_count,layout = matrix(seq_len(6),nrow=2,byrow = T))


