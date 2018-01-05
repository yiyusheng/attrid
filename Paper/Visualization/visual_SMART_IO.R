#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: visual_SMART_IO.R
#
# Description: to show the relationship between the IO features and the change of SMART attributes.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-28 10:15:34
#
# Last   modified: 2017-11-28 10:15:35
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')
col_model <- c('ST31000524NS','ST2000NM0011','ST32000645NS',
               'MB1000EBZQB','ST1000NM0011','ST3500514NS','ST500NM0011')

# S1. Load data ------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'io_features.Rda'))
load(file.path(dir_data,'change_SMART.Rda'))
load(file.path(dir_dataSMT,'sta_disk_model.Rda'))
cng_smart <- mchAttr(cng_smart,sta_ss,'sn','sn',c('svrid','model','devid'))
cng_smart <- cng_smart[,c(setdiff(names(cng_smart),col_smart),col_smart)]

expand_io_based_smart <- function(io_features,cng_smart){
  tmp <- cng_smart[!duplicated(cng_smart$sn),]
  table_svrid_smart <- melt(table(tmp$svrid))
  io_features$numD <- table_svrid_smart$value[match(io_features$svrid,table_svrid_smart$Var1)]
  svrid_expand_disk(io_features)
}

disk_features <- expand_io_based_smart(io_features,cng_smart)
seq_itv <- seq(5,20,5)
cng_smart_list <- lapply(seq_itv,function(itv)subset(cng_smart,interval == itv))

# S2. analysis ------
col_io_features <- c('L9900','L9950','L10000','adc','abw','ratio','asi')
df_smart_io <- cng_smart_list[[4]]
df_smart_io <- subsetX(df_smart_io, svrid %in% io_features$svrid)
df_smart_io <- mchAttr(df_smart_io,io_features,'svrid','svrid',col_io_features)

matrix.corr <- matrix(0,length(col_smart),length(col_io_features))
smart.corr <- matrix(0,length(col_smart),length(col_smart))
for(i in seq_len(length(col_smart))){
  for(j in seq_len(length(col_io_features))){
    c1 <- df_smart_io[[col_smart[i]]]
    c2 <- df_smart_io[[col_io_features[j]]]
    c1[c1 <= 0] <- NA
    c2[c2 <= 0] <- NA
    matrix.corr[i,j] <- cor(c1,c2,use = 'pairwise.complete.obs')
    cat(sprintf('%d\t%d\t%.4f\n',i,j,matrix.corr[i,j]))
  }
  for(j in seq_len(length(col_smart))){
    c1 <- df_smart_io[[col_smart[i]]]
    c2 <- df_smart_io[[col_smart[j]]]
    c1[c1 <= 0] <- NA
    c2[c2 <= 0] <- NA
    smart.corr[i,j] <- cor(c1,c2,use = 'pairwise.complete.obs')
    cat(sprintf('%d\t%d\t%.4f\n',i,j,smart.corr[i,j]))
  }
}
smart.corr <- apply(smart.corr,1,roundX)
matrix.corr <- apply(matrix.corr,1,roundX)

p <- lapply(col_smart,function(n){
  lapply(col_io_features,function(m){
    cat(sprintf('%s vs %s\n',m,n))
    df <- df_smart_io[df_smart_io[[n]]!=0,]
    if(nrow(df)>1e4)df <- smp_df(df,1e4)
    p <- ggplot(df,aes_string(x=m,y=n))+geom_point()+ggtitle(nrow(df))
    ggsave(file=file.path(dir_data,'Paper','smart_io',paste(m,'-',n,'.png',sep='')), plot=p, width = 16, height = 12, dpi = 100)
  })
})
# p1 <- do.call(c,p)
# multiplot(plotlist = p1,cols=7)
