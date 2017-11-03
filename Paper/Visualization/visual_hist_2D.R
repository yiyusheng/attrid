#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: visual_hist.R
#
# Description: visualize duty cycle and bandwidth for each disk drives
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-16 22:06:18
#
# Last   modified: 2017-08-16 22:06:19
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

# S1. Load data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))

io14$numD <- model_svrid$numD[match(io14$svrid,model_svrid$svrid)]
io14$adc <- quantile_dutycycle$mean[match(io14$svrid,quantile_dutycycle$svrid)]
io14$abw <- quan_xps$mean[match(io14$svrid,quan_xps$svrid)]

dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
fn <- fname[which(fname=='data110.Rda')]
load(file.path(dir_dataset,fn))

# S2. Prepare data ------------------------------------
DT_raw <- format_bandwidth(DT)
DT_raw <- factorX(subset(DT_raw,!is.na(xps) & svrid %in% io14$svrid))
splitDTR <- split(DT_raw,DT_raw$svrid)

# S3. Plot ------------------------------------
plot_rs_dc <- function(i){
  df <- splitDTR[[i]]
  title <- sprintf('%s[%d]',df$svrid[1],df$numD[1])
  df$rs <- with(df,xps/util)
  df$rs[df$util==0] <- 0
  df$util <- factor(df$util)
  data_rs_dc <- data.frame(dc = as.numeric(levels(df$util)),
                           count = tapply(df$rs,df$util,length),
                           rs = tapply(df$rs,df$util,mean),
                           sd = tapply(df$rs,df$util,sd),
                           svrid = fct2ori(df$svrid[1]))
  with(data_rs_dc,plot(dc,rs,main=title,xlab = 'duty cycle',ylab='random strength'))
  return(data_rs_dc)
}

plot_dc_bw <- function(i){
  df <- smp_df(splitDTR[[i]],1000)
  title <- sprintf('%s[%d]',df$svrid[1],df$numD[1])
  xlim_Q <- quantile(df$util,1)
  ylim_Q <- quantile(df$xps,1)
  plot(df$util,df$xps,xlim = c(0,xlim_Q),ylim=c(0,ylim_Q),main = title)
}

plot_2d <- function(fi,func){
  prow <- 4;pcol <- 4;pmar <- 2
  par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))
  
  if(length(fi)==1){
    smp_ind <- data.frame(ind=rep(fi,prow*pcol))
  }else if(length(fi)>prow*pcol){
    smp_ind <- data.frame(ind=sample(fi,prow*pcol,replace = F))
  }else{
    smp_ind <- data.frame(ind=sample(fi,prow*pcol,replace = T))
  }
  
  smp_ind$numD <- sapply(splitDTR[smp_ind$ind],function(df)df$numD[1])
  smp_ind <- smp_ind[order(smp_ind$numD),]
  
  f <- get(func)
  r <- lapply(smp_ind$ind,f)
  return(list(r,smp_ind))
}

filtered_ind <- seq_len(length(splitDTR))
filtered_ind1 <- seq_len(length(splitDTR))[names(splitDTR) %in% quan_xps$svrid[quan_xps$mean<=500]]
filtered_ind2 <- seq_len(length(splitDTR))[names(splitDTR) %in% io14$svrid[io14$abw >= 7500 & io14$numD==1]]
filtered_ind3 <- seq_len(length(splitDTR))[names(splitDTR) %in% io14$svrid[io14$adc >= 0 & io14$adc <= 100 & io14$numD==12]]

list[r,smp_ind] <- plot_2d(filtered_ind3,'plot_rs_dc')

table_df <- r[[8]]
sid <- names(splitDTR)[smp_ind$ind[1]]
df <- splitDTR[[sid]]
df_xps <- subset(quan_xps,svrid == sid,c('svrid','mean','sd','Q25','Q50','Q75','Q100'))
df_dc <- subset(quantile_dutycycle,svrid == sid,c('svrid','mean','sd','Q25','Q50','Q75','Q100'))

# numD and the relationship between adc and abw
DT_sgl <- smp_df(subset(io14,numD==1),1000)
DT_mtp <- smp_df(subset(io14,numD==12),1000)
par(mfrow = c(1,1))
plot(DT_mtp$adc,DT_mtp$abw)

# xps and util ------
a <- tapply(DT_raw$xps,DT_raw$util,function(x)quantile(x,c(0.1,0.25,0.5,0.75,0.9)))
a1 <- list2df(a,n = c('Q0','Q25','Q50','Q75','Q100','util'))
a1$util <- as.numeric(a1$util)
ggplot(a1,aes(x=util))+geom_boxplot(aes(ymin=Q0,lower=Q25,middle=Q50,upper=Q75,ymax=Q100),stat='identity')

b <- tapply(DT_raw$xps,DT_raw$util,quantileX)
b1 <- list2df(b,n = c(paste('Q',0:100,sep=''),'util'))
b2 <- setNames(melt(b1,id.vars = 'util'),nm=c('util','quantile','value'))
b2$util <- as.numeric(b2$util)               
b2$quantile <- as.numeric(gsub('Q','',b2$quantile))

p <- lapply(1:100,function(i)
  ggplot(subset(b2,util == i & value < 1e4),aes(x=value,y=quantile))+geom_line()+xlim(c(0,1e4)))

png(filename = file.path(dir_data,'Paper','jpg','distribution.jpg'),width = 1920*2, height = 1080*2, bg = "white")
multiplot(plotlist = p,layout = matrix(1:100,nrow=10,byrow = T))
dev.off()

# abw and adc with ratio
load(file.path(dir_data,'quantile_dutycycle_nozero.Rda'))
DT_quan <- gen_data(quan_random[,c('svrid','mean')],expand=T)
DT_quan$adc <- quantile_dutycycle$mean[match(DT_quan$svrid_old,quantile_dutycycle$svrid)]
DT_quan$abw <- quan_xps$mean[match(DT_quan$svrid_old,quan_xps$svrid)]
list[data_fr,p_fr,p_count,corr,data_rs,data_f] <- gen_result_feature(subset(DT_quan,numD=='1'),'mean',500,balanced_binning = F,bin_count = 5000)
p_rs_abw_sng <- plot_relationship(subset(data_rs,numD=='1'),'mean_level','abw')+xlab('The Sequential Strength')+ylab('The Average Bandwidth(kB/s)')
p_rs_abw_mtp <- plot_relationship(subset(data_rs,numD=='12'),'mean_level','abw')+xlab('The Sequential Strength')+ylab('The Average Bandwidth(kB/s)')
p_rs_adc_sng <- plot_relationship(subset(data_rs,numD=='1'),'mean_level','adc')+xlab('The Sequential Strength')+ylab('The Average Bandwidth(kB/s)')
p_rs_adc_mtp <- plot_relationship(subset(data_rs,numD=='12'),'mean_level','adc')+xlab('The Sequential Strength')+ylab('The Average Bandwidth(kB/s)')
