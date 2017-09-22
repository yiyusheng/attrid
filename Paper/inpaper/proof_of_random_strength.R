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
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]
# title <- 'mean_duty_cycle'

# S2. Get data and the quantile of abw ------------------------------------
DT_quan$mean_level <- trunc_level(DT_quan,'mean',0,100,Tbins=20)
splitDTQ <- split(DT_quan,DT_quan$mean_level)

foo <- function(i){
  r <- lapply(splitDTQ,function(df){
    quan_low <- quantile(df$abw,i)
    quan_high <- quantile(df$abw,1-i)
    df$class <- 'median'
    df$class[df$abw<=quan_low] <- 'low'
    df$class[df$abw>quan_high] <- 'high'
    df$class <- factor(df$class,levels=c('low','median','high'))
    return(df)
  })
  DT_abw_class <- do.call(rbind,r)
  
  # S3. Generate the Failure Rate
  list[data_fr] <- gen_data(DT_abw_class,c('mean_level','class'),expand = F,rsmp = '')
  a <- by(DT_abw_class,list(DT_abw_class$mean_level,DT_abw_class$class),function(df)mean(df$age))
  a1 <- setNames(melt(array(a,dim(a),dimnames(a))),c('mean_level','class','mean_age'))
  data_fr <- merge(data_fr,a1)
  perf <- sum(data_fr$percf[data_fr$class=='low'])
  return(data_fr)
}

# ind <- seq(0.1,0.9,0.1)
# r <- foreachX(ind,'foo',outname = NULL)
# r1 <- setNames(data.frame(matrix(unlist(lapply(r,'[[',1)),nrow=length(r),byrow = T)),c('thred','perc','ratio'))
# data_fr <- lapply(r,'[[',4)[[which(ind==0.8)]]
# data_fr$class <- factor(data_fr$class,levels=c('low','median','high'))
# p_abw <- lapply(r,'[[',2)
# p_age <- lapply(r,'[[',3)
data_fr <- foo(0.25)

# S3.Plot ------------------------------------
p_cmb_splitabw <- ggplot(data_fr,aes(x=mean_level,y=AFR,group=class,color=class))+geom_line(size=1)+geom_point(size=2)+
  guides(color=guide_legend(title='The Average Bandwidth')) + xlab('The Average Duty Cycle (%)') + ylab('Failure Rate(%)') + gen_theme()+
  theme(legend.position = c(0.05,0.95),legend.justification = c(0,1),legend.background = element_rect(fill = alpha('grey',0.5)))

save_fig(p_cmb_splitabw,'cmb_splitabw')
save(p_cmb_splitabw,file=file.path(dir_data,'Paper','Rda','p_cmb.Rda'))

# p2 <- ggplot(data_fr,aes(x=mean_level,y=mean_age,group=class,color=class))+geom_line()+geom_point()+
#   guides(color=guide_legend(title='mean_bandwidth')) + xlab(xl) + ylab('mean_age(days)') + 
#   theme(axis.text = element_text(size = 12),axis.title = element_text(size = 16),legend.text = element_text(size = 12),legend.position = 'bottom')
# return(list(list(i,perf/100,perf/100/i),p1,p2,data_fr))