#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: relationship_age-bandwidth.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-12 11:25:55
#
# Last   modified: 2017-09-12 11:25:56
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
require('reshape')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))

DT <- setNames(merge(quan_xps[,c('svrid','mean')],cmdbSMP[,c('svrid','age')]),nm = c('svrid','abw','age'))
DT <- setNames(merge(quantile_dutycycle[,c('svrid','mean')],DT),nm=c('svrid','adc','abw','age')) 
DT_expd <- gen_data(DT,expand = T)

fail_data <- subset(f201409,svrid %in% DT_expd$svrid_old)
fail_data$numD <- model_svrid$numD[match(fail_data$svrid,model_svrid$svrid)]
table_f <- setNames(melt(table(fail_data$svrid)),nm=c('svrid','count'))
fail_data <- fail_data[!duplicated(fail_data$svrid),]
fail_data$fcount <- table_f$count[match(fail_data$svrid,table_f$svrid)] #times of failure happened in a server
fail_data <- svrid_expand_fail(fail_data,freq = 'fcount')

DT_expd$fail <- 0
DT_expd$fail[DT_expd$svrid %in% fail_data$svrid] <- 1

DT_expd$adc_level <- gen_binned_array(DT_expd,'adc',gen_balanced_binning_point(0,100,20))
DT_expd$abw_level <- gen_binned_array(DT_expd,'abw',gen_balanced_binning_point(0,9000,5))
DT_expd$id <- seq_len(nrow(DT_expd))

# S2. resample data based on abw and adc
resample_2D <- function(DT,attr1,attr2,count=1000){
  r <- lapply(seq_len(nrow(table_attr)),function(i){
    sample(DT$id[DT[[attr1]]==table_attr$attr1[i] & DT[[attr2]]==table_attr$attr2[i]],count,replace = T)    
  })
  return(DT[unlist(r),])
}
table_attr <- setNames(melt_table(DT_expd[['adc_level']],DT_expd[['abw_level']]),nm = c('attr1','attr2','count'))
DT_resample <- resample_2D(DT_expd,'adc_level','abw_level',1000)

rela <- by(DT_resample,list(DT_resample$abw_level,DT_resample$adc_level),function(df)mean(df$age))
rela <- setNames(melt(array(rela,dim(rela),dimnames(rela))),c('abw_level','adc_level','mean_age'))
rela <- subset(rela,!is.na(mean_age))


# S3. plot
png(filename = file.path(dir_data,'Paper','test','age_bandwidth.jpg'),width = 1100, height = 500, bg = "white")
# p <- ggplot(smp_df(DT_resample,1000),aes(x=adc_level,y=abw_level,color=age))+geom_point()+coord_cartesian(ylim=c(0,1e4))
p <- ggplot(rela,aes(x=adc_level,y=mean_age,group=factor(abw_level),color=factor(abw_level)))+geom_line()
print(p)
dev.off()