#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: distribution_bandwidth_duty_cycle.R
#
# Description: We find the weak relationship between the bandwidth and failure rate. 
# To explain the reason, we introduce the sequencial pattern and random pattern to read/write data.
# We try to capture the pattern by the bandwidth and the duty cycle.
# Small bandwidth and small duty cycle means sequencial pattern and small bandwidth and large duty cycle means random pattern.
# To verify the assumption, we plot the heat map for bandwidth and duty cycle.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-29 09:43:15
#
# Last   modified: 2017-08-29 09:43:16
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper/');source('~/rhead')
source('dir_func.R')

get_rec <- function(DT,attr,trunc_value=0.05){
  rec_name <- paste('rec',attr,sep='_')
  rec_name_level <- paste(rec_name,'level',sep='_')
  DT[[rec_name]] <- 0
  DT[[rec_name]][DT[[attr]]!=0] <- log2(1/DT[[attr]][DT[[attr]]!=0])
  DT[[rec_name]][DT[[rec_name]]>0] <- 0
  DT[[rec_name]][DT[[rec_name]]< -14] <- -14
  DT[[rec_name_level]] <- ceiling(DT[[rec_name]]*100)/100
  return(DT)
}

dist_bandwidth_dutycycle <- function(i,bt,bins){
  fn <- fname[i]
  cat(sprintf('[%s]\t SATRT!!!\n',fn))
  load(file.path(dir_dataset,fn))
  # DT <- subset(DT,svrid %in% levels(DT$svrid)[sample(1:length(levels(DT$svrid)),20)])
  
  DT <- format_bandwidth(subset(DT,svrid %in% io14$svrid),bt,bins)
  DT <- subset(DT,wps_trunc!=0)
  
  DT <- get_rec(DT,'rps')
  DT <- get_rec(DT,'wps')
  DT <- get_rec(DT,'xps')
  
  DT$rps_level <- factor(DT$rps_level,levels=seq(0,bt[1],bt[1]/bins))
  DT$wps_level <- factor(DT$wps_level,levels=seq(0,bt[2],bt[2]/bins))
  DT$xps_level <- factor(DT$xps_level,levels=seq(0,bt[3],bt[3]/bins))
  
  DT$util <- factor(DT$util,levels=0:100)
  
  table_bw_dc <- list(table(DT$rps_level,DT$util),table(DT$wps_level,DT$util),table(DT$xps_level,DT$util),
                      table(DT$rec_rps_level,DT$util),table(DT$rec_wps_level,DT$util),table(DT$rec_xps_level,log2(DT$util)))
                      # table(DT$rps_level,DT$wps_level),table(DT$rps_level,DT$xps_level),table(DT$wps_level,DT$xps_level))

  cat(sprintf('[%s]\t END!!!\n',fn))
  return(table_bw_dc)
}

###### STA:MAIN ######
load(file.path(dir_data,'uniform_data.Rda'))
time_trunc <- 2
bw_trunc <- c(4000,5000,9000)*time_trunc

dir_dataset <- dir_data14DC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'dist_bandwidth_dutycycle',frac_cores = 0.9,bt=bw_trunc,bins=100)

dist_data <- list();plist <- list()
for(i in 1:6){
  d <- setNames(melt(Reduce('+',lapply(r,'[[',i))),nm=c('attr1','attr2','count'))
  p <- ggplot(subset(d,1==1))+geom_raster(aes(x=attr1,y=attr2,fill=log2(count)))+
    geom_contour(aes(x=attr1,y=attr2,z=log2(count)),binwidth=2)+
    scale_fill_gradientn(colours=c("#0000FF","#FFFFFF","#FF0000"))
  print(p)
  
  dist_data[[i]] <- d
  plist[[i]] <- p
}


multiplot(plotlist = plist,layout = matrix(1:6,nrow=2,byrow = T))
save(dist_data,plistfile=file.path(dir_data,'dist_bandwidth_dutycycle.Rda'))



# p <- ggplot(subset(dist_xps_dc,count>0 & dutycycle>0))+
#   geom_raster(aes(x=xps,y=dutycycle,fill=log2(count)),interpolate = F)+
#   geom_contour(aes(x=xps,y=dutycycle,z=log2(count),colour = ..level..),binwidth=2)+
#   scale_fill_gradientn(colours=c("#0000FF","#FFFFFF","#FF0000"))
# print(p)
