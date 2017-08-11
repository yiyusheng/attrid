#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: continuous_idle_duty_cycle.R
#
# Description: we generage the average length/mean duty cycle/count of idle and working period of duty cycle.
# Here, we compare these attributes in disk drives to find the difference and correlate them and the disk failure.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 09:08:31
#
# Last   modified: 2017-08-11 09:08:32
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'average_legnth_idle_duty_cycle.Rda'))


# S1. Prepare data
object_data <- r[[5]]
object_data <- subset(object_data,svrid %in% io14$svrid)
object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
object_data$age <- cmdbSMP$age[match(object_data$svrid,cmdbSMP$svrid)]
object_data <- svrid_expand_disk(object_data)
col_dc <- names(object_data)[2:7]

# Add cut columns
for(cn in col_dc){
  cut_point <- unique(seq(floor(min(object_data[[cn]])),ceiling(max(object_data[[cn]])),length.out = 20))
  if(length(cut_point)==1){
    object_data[[paste(cn,'level',sep='-')]] <- rep(cut_point,length(object_data[[cn]]))
  }else{
    object_data[[paste(cn,'level',sep='-')]] <- cut(object_data[[cn]],cut_point,cut_point[-length(cut_point)])
  }
}

# failure record
fail_data <- f201409
col_need <- names(object_data)[grepl('level',names(object_data))]
fail_data <- mchAttr(fail_data,object_data,'svrid','svrid',col_need)

# S2. Diff and FR
diff_fr <- lapply(col_need,function(cn){
  # Difference
  dif <- setNames(melt(table(object_data[[cn]])),nm = c('cn','count'))
  dif$ratio <- array_rate(dif$count)
  dif$cumratio <- cumsum(dif$ratio)
  dif$class <- cn
  p_diff <- ggplot(dif,aes(x=cn,y=ratio)) + geom_bar(stat='identity') + 
    xlab(cn) + ylab('Percentage(%)')
  
  # Failure Rate
  fr <- ioAFR(object_data,fail_data,attr = cn)
  fr$count_rate <- array_rate(fr$count)
  fr$level <- 'low'
  fr$level[fr$AFR > 5] <- 'high'
  names(fr)[1] <- 'cn'
  fr$class <- cn
  p_fr <- ggplot(fr,aes(x = cn,y = AFR,fill = level)) + geom_bar(stat = 'identity')+
    xlab(cn) + ylab('Failure Rate(%)') + guides(fill = guide_legend(title=NULL)) 
  return(list(dif,p_diff,fr,p_fr))
  
})

# S3. Plot
dif <- do.call(rbind,lapply(diff_fr,'[[',1));fr <- do.call(rbind,lapply(diff_fr,'[[',3))
# p_diff_list <- lapply(diff_fr,'[[',2);multiplot(plotlist = p_diff_list,cols = 1)
p_fr_list <- lapply(diff_fr,'[[',4);multiplot(plotlist = p_fr_list,cols=1)

