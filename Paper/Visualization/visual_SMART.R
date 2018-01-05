#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: visual_SMART.R
#
# Description: observe the reduction of different smart attributes and find features.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-11-29 16:49:20
#
# Last   modified: 2017-11-29 16:49:21
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

# S1. Load basic data ------------------------------------
dir_dataset <- file.path(dir_dataSMT,'smart5k')
fname <- list.files(dir_dataset)
load(file.path(dir_data,'change_SMART.Rda'))
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_dataSMT,'sta_disk_model.Rda'))
col_char3 <- c('9WK', 'Z1P', 'Z1N', 'Z29', 'Z1K', 
               '9WJ', '9QK', 'Z1M', 'Z1X', '9SF')

# S2. choose smart attributes/model and load data ------------------------------------

# attributes
idx_smart <- 1
attr <- col_smart[idx_smart]
cng_smart <- mchAttr(cng_smart,sta_ss,'sn','sn',c('char3','fn'))
cng_smart_itv20 <- subset(cng_smart,interval==20)
cng_smart_itv20_idx <- cng_smart_itv20[!is.na(cng_smart_itv20[attr]) & cng_smart_itv20[attr] != 0 &
                                         cng_smart_itv20$char3 %in% col_char3,]
table_fn <- melt_table(cng_smart_itv20_idx$fn,cng_smart_itv20_idx$char3)
table_fn <- dcast(table_fn,Var1~Var2,value.var = 'value')

# file
idx_char3 <- 1
fn <- table_fn$Var1[which.max(table_fn[[col_char3[idx_char3]]])]
load(file.path(dir_dataset,fn))

# extract data
cng_smart_itv20_idx <- subset(cng_smart_itv20_idx,sn %in% smart$sn)
sample_sn <- factor(sample(unique(cng_smart_itv20_idx$sn),
                           min(500,length(unique(cng_smart_itv20_idx$sn)))))
smart_idx <- subsetX(smart,sn %in% sample_sn)
splitSMT <- split(smart_idx,smart_idx$sn)

# S4. plot ------------------------------------
prow <- 4;pcol <- 4;pmar <- 2
par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))
smp_ind <- sample(1:length(splitSMT),prow*pcol,replace = T)
smp_df <- data.frame(ind=smp_ind,sn=names(splitSMT)[smp_ind])
smp_df <- smp_df[order(smp_df$sn),]
for (i in smp_df$ind) {
  df <- splitSMT[[i]]
  arr <- df[[attr]]
  title <- sprintf('%s\n[%s]',attr,df$sn[1])
  if(length(arr)==0)arr<-rep(0,1000)
  if(df$svrid[1] %in% f201409$svrid){
    plot(arr,main=title,col='red')
  }else{
    plot(arr,main=title,col='blue')
  }
}

# S5.
d <- lapply(splitSMT,function(df){
  arr <- df$Raw_Read_Error_Rate_Value
  diff1_arr <- c(0,diff(arr))
  diff2_arr <- c(0,0,diff(arr,lag=2))
  return(list(diff1_arr+diff2_arr,diff1_arr,diff2_arr))
})
d0 <- do.call(c,lapply(d,'[[',1))
d1 <- do.call(c,lapply(d,'[[',2))
d2 <- do.call(c,lapply(d,'[[',3))
hist(d0[d0< -15])
hist(d1[d1< -15])
hist(d2[d2< -15])

df$diff <- c(0,diff(df$Raw_Read_Error_Rate_Value))
ggplot(df,aes(x=time,y=Raw_Read_Error_Rate_Value))+geom_line()+
  geom_point(aes(color=factor(diff< (mean(diff)-3*sd(diff)))))
