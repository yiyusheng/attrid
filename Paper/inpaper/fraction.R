#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: fraction.R
#
# Description: fraction of duty cycle larger than a threshold 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-09-11 16:32:25
#
# Last   modified: 2017-09-11 16:32:26
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. data prepare ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))

DT <- quantile_dutycycle
thresholds <- seq(0,100,5)
attr <- paste('T',thresholds,sep='')
for(i in seq_len(length(thresholds))){
  df_frac <- get_quan_percentage(DT,thresholds[i])
  DT[[attr[i]]] <- df_frac$fraction[match(DT$svrid,df_frac$svrid)]*100
}

attr_main <- paste('T',thresholds,sep='')
DT_quan_all <- gen_data(DT[,c('svrid','sd',attr_main)],expand=T)
DT_quan_all <- subset(DT_quan_all,numD==12) # update 2018-01-04
DT_quan_grt <- subset(DT_quan_all,adc >= 20) # update 2017-12-27
DT_quan_les <- subset(DT_quan_all,adc < 20)  # update 2017-12-27

# S2. analysis ------------------------------------
system.time(r_grt <- lapply(attr_main,function(at)gen_result_feature(DT=DT_quan_grt,attr=at,attr_max=100,bins = 20)))
data_fr_list_grt <- lapply(r_grt,'[[',1)
p_fr_list_grt <- lapply(r_grt,'[[',2)
p_count_list_grt <- lapply(r_grt,'[[',3)
data_corr_grt <- data.frame(thred = thresholds[-1],corr=sapply(r_grt,'[[',4)[-1])

# system.time(r_les <- lapply(attr_main,function(at)gen_result_feature(DT=DT_quan_les,attr=at,attr_max=100,bins = 20)))
# data_fr_list_les <- lapply(r_les,'[[',1)
# p_fr_list_les <- lapply(r_les,'[[',2)
# p_count_list_les <- lapply(r_les,'[[',3)
# data_corr_les <- data.frame(thred = thresholds[-1],corr=sapply(r_grt,'[[',4)[-1])

# S3. plot ------------------------------------
# S3.1 orginal distribution of five factors
thredset <- seq(50,80,10)
plist_thred <- lapply(thredset,function(i){
  p1 <- p_fr_list_grt[[which(thresholds == i)]]+
    xlab('Fraction of Large Duty Cycle (%)')+coord_cartesian(ylim=c(0,35),xlim=c(0,105))+
    scale_fill_manual(values=c('grey60','grey20'))+ ylab('Failure Rate (%)')
  p2 <- p_count_list_grt[[which(thresholds == i)]]+coord_cartesian(xlim=c(0,105))
  # p3 <- p_fr_list_les[[which(thresholds == i)]]+
  #   xlab('Fraction of Large Duty Cycle (%)')+coord_cartesian(ylim=c(0,35),xlim=c(0,105))+
  #   scale_fill_manual(values=c('grey60','grey20'))+ ylab('Failure Rate (%)')
  # p4 <- p_count_list_les[[which(thresholds == i)]]+coord_cartesian(xlim=c(0,105))
  # list(p1,p2,p3,p4)
  list(p1,p2)
})
plist_thred_grt <- lapply(plist_thred,'[[',1)
plist_thred_grt_hist <- lapply(plist_thred,'[[',2)
# plist_thred_les <- lapply(plist_thred,'[[',3)
# plist_thred_les_hist <- lapply(plist_thred,'[[',4)

# png(filename = file.path(dir_data,'Paper','jpg','frac_grt_les.jpg'),width = 1920, height = 1080, bg = "white")
# multiplot(plotlist = c(plist_thred_grt,plist_thred_grt_hist,plist_thred_les,plist_thred_les_hist),layout = matrix(1:16,byrow=T,nrow=4))
# dev.off()

p_frac_corr <- ggplot(data_corr_grt,aes(x=thred,y=corr))+geom_line(size=0.5)+geom_point(size=2)+
  xlab('Duty Cycle Threshold (%)') + ylab("Correlation Coefficient")+gen_theme()

list[p_frac_frA_grt,p_frac_frB_grt,p_frac_frC_grt,p_frac_frD_grt] <- plist_thred_grt
# list[p_frac_frA_les,p_frac_frB_les,p_frac_frC_les,p_frac_frD_les] <- plist_thred_les


# S3.2 duty cycle distribution with special disk with similar ADC
load(file.path(dir_data14DC,'data109.Rda'))
svrid_need <- c('TYSV12121570','TYSV12120EE8')
DT_smp <- subset(DT,svrid %in% svrid_need)
DT_smp$svrid <- fct2ori(DT_smp$svrid)
DT_smp$svrid[DT_smp$svrid==svrid_need[1]] <- 'Disk1'
DT_smp$svrid[DT_smp$svrid==svrid_need[2]] <- 'Disk2'
point1 <- ecdf(DT_smp$util[DT_smp$svrid=='Disk1'])(60)
point2 <- ecdf(DT_smp$util[DT_smp$svrid=='Disk2'])(60)
p_frac_illu <- ggplot(DT_smp,aes(x=util,group=svrid))+stat_ecdf(aes(color=svrid))+guides(color=guide_legend(title=NULL))+
  geom_vline(xintercept=60,color='red',linetype=2)+
  geom_hline(yintercept = point1,color='red',linetype=2)+
  geom_hline(yintercept = point2,color='red',linetype=2)+
  scale_x_continuous(breaks = seq(0,100,10))+
  annotate("text", x=60*0.8,y=point2*1.3,label= sprintf('fraction of duty cycle less\n than the threshold: %.4f',point2),size=8)+
  geom_segment(aes(x = 60*0.9, y = point2*1.1, xend = 60*.98, yend = point2*1.02), size=0.2,arrow = arrow(length = unit(0.2, "cm")))+
  annotate("text", x=60*1.2,y=point1*0.7,label= sprintf('fraction of duty cycle less\n than the threshold: %.4f',point1),size=8)+
  geom_segment(aes(x = 60*1.1, y = point1*0.9, xend = 60*1.02, yend = point1*0.98), size=0.2,arrow = arrow(length = unit(0.2, "cm")))+
  annotate("text", x=60,y=0.03,label= 'Threshold: 60%',size=8)+
  xlab('Duty cycle (%)')+ylab('CDF')+
  gen_theme()

# S3.3 reduction of failure rate of fraction
list[b50,b60,b70,b80,b90] <- data_fr_list_grt[seq(11,19,2)]
t80_frac90 <- subset(DT_quan_grt,T80>90)
t80_frac90 <- t80_frac90[!duplicated(t80_frac90$svrid_old),]
t80_frac90$fn <- io14$fn[match(t80_frac90$svrid_old,io14$svrid)]
t80_frac70 <- subset(DT_quan_grt,T80>60 & T80<90)
t80_frac70 <- t80_frac70[!duplicated(t80_frac70$svrid_old),]
t80_frac70$fn <- io14$fn[match(t80_frac70$svrid_old,io14$svrid)]
save(t80_frac70,t80_frac90,file=file.path(dir_data,'fraction_reduction_svrid.Rda'))

DT_rdc <- DT_quan_grt[,c(names(DT_quan_grt)[!grepl('T.*',names(DT_quan_grt))],c('T50','T60','T70','T80'))]
DT_rdc$T80_level <- sort_level(factor(as.character(ceiling(DT_rdc$T80/10)*10)))
DT_rdc$cv <- with(DT_rdc,sd/adc)
table.cv <- melt(tapply(DT_rdc$cv,DT_rdc$T80_level,mean)) # in paper
ggplot(subset(DT_rdc,T80_level!='0'))+stat_ecdf(aes(x=cv,linetype=T80_level,color=T80_level))

load(file.path(dir_data,'diff_dutycycle.Rda'))
DT_rdc <- mchAttr(DT_rdc,diff_duty_cycle,'svrid_old','svrid',names(diff_duty_cycle)[-1])
DT_rdc$count <- io14$count[match(DT_rdc$svrid_old,io14$svrid)]
ggplot(subset(DT_rdc,T80_level!='0'))+stat_ecdf(aes(x=diff,linetype=T80_level,color=T80_level))

# S3.4 cluster for T60 and T80
km <- kmeans(DT_rdc[,c('T60','T80')],centers = 3)
library(cluster)
library(fpc)
plot(DT_rdc[,c('T60','T80')],col=km$cluster)
points(km$center,col=1:2,pch=8,cex=1)
# S4. Save ------------------------------------
save_fig(p_frac_frA_grt,'frac_fr50')
save_fig(p_frac_frB_grt,'frac_fr60')
save_fig(p_frac_frC_grt,'frac_fr70')
save_fig(p_frac_frD_grt,'frac_fr80')
save_fig(p_frac_corr,'frac_corr')
save_fig(p_frac_illu,'frac_illu')
save(p_frac_frA,p_frac_frB,p_frac_frC,p_frac_frD,p_frac_corr,p_frac_illu,file=file.path(dir_data,'Paper','Rda','p_frac.Rda'))
