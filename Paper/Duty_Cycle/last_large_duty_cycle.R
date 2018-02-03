#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: last_large_duty_cycle.R
#
# Description: 
# Question: How long does the large duty cycle last can affect the disk failure significantly
#
# Copyright (c) 2018, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2018-01-09 17:55:15
#
# Last   modified: 2018-01-09 17:55:17
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'fraction.Rda'))
load(file.path(dir_data,'length_last_dutycycle.Rda'))
thresholds <- seq(10,90,10)
minutes <- seq(2,24,2)

large_count <- function(arr,count){
  arr <- arr[arr>count]
  sum(arr/count)
}

multiplot.origin <- function(list.data,attry,tp='b'){
  len.list <- length(list.data)
  len.plot <- floor(len.list^0.5)
  height.plot <- len.list/len.plot
  prow <- len.plot;pcol <- height.plot;pmar <- 2
  par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))
  
  for (i in seq_len(prow*pcol)) {
    df <- list.data[[i]]
    data.x <- unlist(df[names(df)[grepl('_level',names(df))]])
    data.y <- df[[attry]]
    plot(data.x,data.y,type=tp)
  }
}

# generate feature of lasting large duty cycles
features_thred <- lapply(thresholds,function(t){
  cat(sprintf('[%s]\tthred:%s\tSTART!!!\n',date(),t))
  DT_thred <- lapply(length_last_dutycycle[[which(thresholds==t)]],'[[',1) # stat of duty cycle greater than thred
  DT_feature <- data.frame(svrid = names(DT_thred))
  DT_feature$items <- io14$count[match(DT_feature$svrid,io14$svrid)]
  r <- lapply(minutes,function(m){
    rr <- sapply(DT_thred,function(x)large_count(x,m))
    rr <- rr/DT_feature$items[match(names(rr),DT_feature$svrid)]*288
    DT_feature[[paste('M',m*5,sep='')]] <<- rr
    return(0)
  })
  DT_feature <- gen_data(DT_feature,expand=T)
  DT_feature <- subset(DT_feature,numD==12)
  cat(sprintf('[%s]\tthred:%s\tEND!!!\n',date(),t))
  return(DT_feature)
})

rr <- lapply(thresholds,function(t){
  DT_thred_M <- features_thred[[which(thresholds==t)]]
  r <- lapply(minutes,function(m){
    at <- paste('M',m*5,sep='')
    Q95 <- quantile(DT_thred_M[[at]],0.95)
    Q05 <- quantile(DT_thred_M[[at]],0.05)
    cat(sprintf('Fraction:%d\tMinutes:%d\tCount:%d\tQ05:%.2f\tQ95:%.2f\t%s:%.2f\t',
                t,m,nrow(DT_thred_M),Q05,Q95,at,mean(DT_thred_M[[at]])))
    r <- gen_result_feature(DT = DT_thred_M,attr = at,attr_min=Q05,attr_max = Q95,bins = 20,remove_zero = T)
  })
})

multiplot.origin(lapply(rr[[which(thresholds==90)]],'[[',1),'AFR')

r <- rr[[which(thresholds==10)]]
data_fr_list <- lapply(r,'[[',1)
p_fr_list <- lapply(r,'[[',2)
p_count_list <- lapply(r,'[[',3)
corr_largedc <- sapply(r,'[[',4)








# DT_quan[[attrT]] <- round(fraction[[attrT]][match(DT_quan$svrid_old,fraction$svrid)])
# DT_quan_thred <- DT_quan[DT_quan[[attrT]]<101,]
# DT_quan_thred$count_frac <- ifelse(DT_quan_thred$T10==0,0,DT_quan_thred$count_grt/(DT_quan_thred[[attrT]]/100)/DT_quan_thred$items*100)
# Q95 <- quantile(DT_quan_thred[[attr]],0.95)
# Q05 <- quantile(DT_quan_thred[[attr]],0.05)
# 
# cat(sprintf('Fraction:%d\tCount:%d\tQ05:%.2f\tQ95:%.2f\t%s:%.2f\t',i,nrow(DT_quan_thred),Q05,Q95,attr,mean(DT_quan_thred$count_grt)))
# r <- gen_result_feature(DT = DT_quan_thred,attr = attr,attr_min=0,attr_max = 1.4,bins = 20,remove_zero = T)
# list[data_fr,p_fr,p_count] <- r
# 
# r <- lapply(1:20,function(i){
#   DT_quan_thred <- DT_quan[DT_quan[[attrT]]==i,]
#   Q95 <- quantile(DT_quan_thred[[attr]],0.95)
#   Q05 <- quantile(DT_quan_thred[[attr]],0.05)
#   
#   cat(sprintf('Fraction:%d\tCount:%d\tQ05:%.2f\tQ95:%.2f\t%s:%.2f\t',i,nrow(DT_quan_thred),Q05,Q95,attr,mean(DT_quan_thred$count_grt)))
#   r <- gen_result_feature(DT = DT_quan_thred,attr = attr,attr_min=Q05,
#                           attr_max = Q95,bins = 10,remove_zero = T)
# })
# data_fr_list <- lapply(r,'[[',1)
# p_fr_list <- lapply(r,'[[',2)
# p_count_list <- lapply(r,'[[',3)
# corr_largedc <- sapply(r,'[[',4)
# 
# png(filename = file.path(dir_data,'Paper','jpg','last_ldc.jpg'),width = 1920*2, height = 1080*2, bg = "white")
# multiplot(plotlist = p_fr_list,layout = matrix(seq_len(length(p_fr_list)),byrow=T,nrow=4))
# dev.off()
# 
# 
# # visualization ---
# io14$util <- with(io14,sum_util/count)
# thred <- 10;attrT <- paste('T',thred,sep='');attr <- 'count_frac'
# DT_thred <- lapply(length_last_dutycycle[[which(thresholds==thred)]],'[[',1)
# DT_thred[sapply(DT_thred,length)<=10] <- NULL
# DT_thred_sta <- data.frame(svrid = names(DT_thred),
#                        count = sapply(DT_thred,length),
#                        mean = sapply(DT_thred,mean))
# Q.corr <- sapply(DT_thred_sta,function(arr)quantile(arr,seq(0.05,1,0.05)))
# DT_corr <- cbind(DT_thred_corr,t(Q.corr))
# names(DT_corr) <- c(names(DT_thred_sta),paste('Q',seq(5,100,5),sep=''))
# 
# 
# prow <- 4;pcol <- 4;pmar <- 2
# par(mfrow = c(prow,pcol),mar=c(pmar,pmar,pmar,pmar))
# 
# smp_ind <- data.frame(ind=sample(1:length(DT_thred),prow*pcol))
# smp_ind$svrid <- names(DT_thred)[smp_ind$ind]
# smp_ind$numD <- model_svrid$numD[match(smp_ind$svrid,model_svrid$svrid)]
# smp_ind$adc <- io14$util[match(smp_ind$svrid,io14$svrid)]
# smp_ind$Q95 <- DT_corr$Q95[match(smp_ind$svrid,DT_corr$svrid)]
# smp_ind <- smp_ind[order(smp_ind$Q95),]
# for (i in seq_len(prow*pcol)) {
#   arr <- DT_thred[[smp_ind$ind[i]]]
#   title <- sprintf('%s[%s]\n[%.2f][%.2f][%.2f]',
#                    smp_ind$svrid[i],smp_ind$numD[i],length(arr),mean(arr),smp_ind$Q95[i])
#   table.arr <- melt(table(arr))
#   plot(table.arr$arr,table.arr$value,main=title,type='h')
# }
# 
# 
# sapply(paste('Q',seq(5,100,5),sep=''),function(q)cor(DT_corr$count,DT_corr[[q]]))
# sapply(paste('Q',seq(5,100,5),sep=''),function(q)cor(DT_corr$mean,DT_corr[[q]]))
