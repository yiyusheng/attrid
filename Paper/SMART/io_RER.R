#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: io_RER.R
#
# Description: generate the relationship between the features of Read error rate and the IO features
# we find the interval and amplitude are related to the failure rate.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-12-05 17:09:15
#
# Last   modified: 2017-12-07 11:55:03
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

plot_bar_2factors <- function(df,attr1,attr2,max1,bins1=20,ch,yl1=0,yl2=0,add_age=F){
  df <-df[!is.na(df[[attr1]]) & !is.na(df[[attr2]]),]
  df <- subset(df,char3==ch)
  
  if(add_age==T){
    df <- subset(df,age/365/5 < 1)
    df[[attr1]] <- df[[attr1]]*df$age/365/5
  }
  
  df <- binning_data(df,attr1,max1,bins = bins1)
  table_df <- melt(tapply(df[[attr2]],df[[paste(attr1,'level',sep='_')]],mean))
  names(table_df) <- c(attr1,attr2)
  
  if(yl1 !=0 | yl2 != 0){
    p <- ggplot(table_df,aes_string(attr1,attr2))+geom_bar(stat = 'identity')+
      ylab(attr2)+xlab(sprintf('%s[%d][%s]',attr1,nrow(df),ch))+coord_cartesian(ylim=c(yl1,yl2))
  }else{
    p <- ggplot(table_df,aes_string(attr1,attr2))+geom_bar(stat = 'identity')+ylab(attr2)+xlab(sprintf('%s[%d][%s]',attr1,nrow(df),ch))
  }
  return(p)
}

# S1. Load function and data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'io_features.Rda'))
load(file.path(dir_data,'smart_RSC_RER.Rda'))
load(file.path(dir_dataSMT,'sta_disk_model.Rda'))
io_features$age <- cmdbSMP$age[match(io_features$svrid,cmdbSMP$svrid)]
col_char3 <- c('9WK', '9WJ', '9QK', '9SF',
               'Z1P', 'Z1N', 'Z29', 'Z1K','Z1M', 'Z1X')
col_io <- c("L9900", "L9950", "L10000", "adc", "abw", "ratio", "asi")

# S2. The unchanged disk drives 
r_unchanged_rer <- subset(r_mean, !(sn %in% r_rer$sn))
r_unchanged_rer <- mchAttr(r_unchanged_rer,sta_ss,'sn','sn',c('svrid','char3'))
r_unchanged_rer <- mchAttr(r_unchanged_rer,io_features,'svrid','svrid',c(names(io_features)[-1]))
r_unchanged_rer <- subset(r_unchanged_rer,!is.na(asi))
io_feature_para <- data.frame(attr = col_io,max = c(100,100,100,100,9000,1,500))

plist <- lapply(seq_len(nrow(io_feature_para)),function(i){
  lapply(col_char3,function(ch){
    plot_bar_2factors(r_unchanged_rer,fct2ori(io_feature_para$attr[i]),'Raw_Read_Error_Rate_Value',io_feature_para$max[i],ch=ch,add_age=F)
  })
})

png(filename = file.path(dir_data,'Paper','SMART','unchanged_rer.jpg'),width = 1920, height = 1080, bg = "white")
multiplot(plotlist = unlist(plist,recursive = F),
          layout = matrix(seq_len(length(col_char3)*nrow(io_feature_para)),ncol=length(col_char3),byrow = T))
dev.off()

# S3. The changed disk drives
r_changed_rer <- mchAttr(r_rer,sta_ss,'sn','sn',c('svrid','char3'))
r_changed_rer <- mchAttr(r_changed_rer,io_features,'svrid','svrid',c(names(io_features)[-1]))
plist_freq <- lapply(seq_len(nrow(io_feature_para)),function(i)
  lapply(col_char3,function(ch){
    plot_bar_2factors(subset(r_changed_rer,itv!=-1),fct2ori(io_feature_para$attr[i]),'itv',io_feature_para$max[i],ch=ch,yl2=10,add_age=F)
  })
)
plist_amp <- lapply(seq_len(nrow(io_feature_para)),function(i)
  lapply(col_char3,function(ch){
    plot_bar_2factors(subset(r_changed_rer,avg_rdc!=-1),fct2ori(io_feature_para$attr[i]),'avg_rdc',io_feature_para$max[i],ch=ch,yl1=-20,add_age=F)
  })
)

png(filename = file.path(dir_data,'Paper','SMART','changed_rer_freq.jpg'),width = 1920, height = 1080, bg = "white")
multiplot(plotlist = unlist(plist_freq,recursive = F),
           layout = matrix(seq_len(length(col_char3)*nrow(io_feature_para)),ncol=length(col_char3),byrow = T))
dev.off()

png(filename = file.path(dir_data,'Paper','SMART','changed_rer_inc.jpg'),width = 1920, height = 1080, bg = "white")
multiplot(plotlist = unlist(plist_amp,recursive = F),
          layout = matrix(seq_len(length(col_char3)*nrow(io_feature_para)),ncol=length(col_char3),byrow = T))
dev.off()
