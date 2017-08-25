#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: extreme_duty_cycle.R
#
# Description: We extract the maximum duty cycle for each disk drive.
# Then we plot the distribution and failure rate of the maximum duty cycle.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-25 10:43:35
#
# Last   modified: 2017-08-25 10:43:37
#
#
#

# S1. Load function and data ------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))

# S2. Extract the maximum duty cycle
object_data <- r[,c('svrid','X100')]
itv <- 5
object_data$extreme <- ceiling(object_data$X100/itv)*itv 
list[data_fr,p_fr,p_count] <- gen_fr(object_data,'extreme',prt=F,countLimit = 100)
p1 <- p_count+xlab('extreme duty cycle(%)')
p2 <- p_fr + xlab('extreme duty cycle(%)')+ geom_smooth(method='lm')
corr <- with(data_fr,cor(extreme,AFR))
print(corr)
summary(glm(AFR~extreme,data = data_fr,family = 'gaussian'))


# S3. correlation of 98% quantile to 100% quanitle break by 0.1%
load(file.path(dir_data,'large_quantile_dutycycle.Rda'));DT <- r
gen_corr <- function(i,itv){
  col_attr <- paste('X',i,sep='')
  object_data <- DT[,c('svrid',col_attr)]
  object_data$extreme <- ceiling(object_data[[col_attr]]/itv)*itv 
  list[data_fr,p_fr,p_count] <- gen_fr(object_data,'extreme',prt=F,countLimit = 100)
  corr <- with(data_fr,cor(extreme,AFR))
  return(data.frame(i,corr,itv))
}
idx <- 980:1000
system.time(r <- foreachX(idx,'gen_corr',frac_cores = 0.9,itv=1))
r1 <- setNames(do.call(rbind,r),nm=c('quantile','corr','itv'))
system.time(r <- foreachX(idx,'gen_corr',frac_cores = 0.9,itv=5))
r5<- setNames(do.call(rbind,r),nm=c('quantile','corr','itv'))

corr <- rbind(r1,r5)
p_corr <- ggplot(corr,aes(x=quantile/10,y=corr,group=itv,color=factor(itv))) + geom_point(size=5)+geom_line()+
  xlab('Quantile(%)')+ylab('Correlation Coefficient')+
  guides(fill = guide_legend(title=NULL),color=guide_legend(title='interval')) +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.title = element_text(size=24),legend.position = 'bottom')

