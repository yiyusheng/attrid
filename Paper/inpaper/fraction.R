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

rm(list = ls());source('~/rhead');setwd(file.path(dir_c,'Disk_Workload/Paper'));
source('dir_func.R')

# S1. data prepare ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))

DT <- quantile_dutycycle
thresholds <- sort(c(5,15,seq(10,90,10)))
attr_main <- paste('T',thresholds,sep='')

attr <- paste('T',thresholds,sep='')
for(i in seq_len(length(thresholds))){
  df_frac <- get_quan_percentage(DT,thresholds[i])
  DT[[attr[i]]] <- df_frac$fraction[match(DT$svrid,df_frac$svrid)]*100
}
# fraction <- DT[,c('svrid','mean','sd',attr_main)]
# save(fraction,file=file.path(dir_data,'fraction.Rda'))

DT_quan_all <- gen_data(DT[,c('svrid','mean','sd',attr_main)],expand=T)
DT_quan_all <- subset(DT_quan_all,numD==12) # update 2018-01-04

# S2. analysis ------------------------------------
system.time(r_all <- lapply(attr_main,function(at)gen_result_feature(DT=DT_quan_all,attr=at,attr_max=100,bins = 20)))
data_fr_list_all <- lapply(r_all,'[[',1)
p_fr_list_all <- lapply(r_all,'[[',2)
p_count_list_all <- lapply(r_all,'[[',3)
data_corr_all <- data.frame(thred = thresholds[-1],corr=sapply(r_all,'[[',4)[-1])

# S3. plot ------------------------------------
# S3.1 orginal distribution of five factors
thredset <- thresholds
plist_thred <- lapply(thresholds,function(i){
  p5 <- p_fr_list_all[[which(thresholds == i)]]+
    xlab('RAT of Large Duty Cycle (%)')+coord_cartesian(ylim=c(0,35),xlim=c(0,105))+gen_theme(p=10)
  p6 <- p_count_list_all[[which(thresholds == i)]]+coord_cartesian(xlim=c(0,105))
  list[p1,p2,p3,p4] <- list(1,2,3,4)
  list(p1,p2,p3,p4,p5,p6)
})
plist_thred_all <- lapply(plist_thred,'[[',5)
plist_thred_all_hist <- lapply(plist_thred,'[[',6)

list[p_frac_frA_grt,p_frac_frB_grt,p_frac_frC_grt,p_frac_frD_grt] <- plist_thred_all[which(thresholds %in% c(50,60,70,80))]
list[p_frac_frA_les,p_frac_frB_les,p_frac_frC_les,p_frac_frD_les] <- plist_thred_all[which(thresholds %in% c(5,10,15,20))]


# S3.2 duty cycle distribution with special disk with similar ADC
# load(file.path(dir_data14DC,'data109.Rda'))
# svrid_need <- c('TYSV12121570','TYSV12120EE8')
# DT_smp <- subset(DT,svrid %in% svrid_need)
# DT_smp$svrid <- fct2ori(DT_smp$svrid)
# DT_smp$svrid[DT_smp$svrid==svrid_need[1]] <- 'Disk1'
# DT_smp$svrid[DT_smp$svrid==svrid_need[2]] <- 'Disk2'
# point1 <- ecdf(DT_smp$util[DT_smp$svrid=='Disk1'])(60)
# point2 <- ecdf(DT_smp$util[DT_smp$svrid=='Disk2'])(60)
# p_frac_illu <- ggplot(DT_smp,aes(x=util,group=svrid))+stat_ecdf(aes(color=svrid))+guides(color=guide_legend(title=NULL))+
#   geom_vline(xintercept=60,color='red',linetype=2)+
#   geom_hline(yintercept = point1,color='red',linetype=2)+
#   geom_hline(yintercept = point2,color='red',linetype=2)+
#   scale_x_continuous(breaks = seq(0,120,10))+
#   annotate("text", x=60*0.45,y=point2*1.18,label= sprintf('fraction of duty cycle less\nthan the threshold: %.4f',point2),size=8)+
#   geom_segment(aes(x = 60*0.9, y = point2*1.1, xend = 60*.98, yend = point2*1.02), size=0.2,arrow = arrow(length = unit(0.2, "cm")))+
#   annotate("text", x=60*1.37,y=point1*0.7,label= sprintf('fraction of duty cycle less\nthan the threshold: %.4f',point1),size=7)+
#   geom_segment(aes(x = 60*1.1, y = point1*0.9, xend = 60*1.02, yend = point1*0.98), size=0.2,arrow = arrow(length = unit(0.2, "cm")))+
#   annotate("text", x=60*1.35,y=0.03,label= 'The threshold: 60%',size=8)+
#   geom_segment(aes(x = 60*1.05, y = 0.03, xend = 60*1.02, yend = -0.01), size=0.2,arrow = arrow(length = unit(0.2, "cm")))+
#   xlab('Duty cycle (%)')+ylab('CDF')+
#   gen_theme()+theme(legend.position = c(0.02,0.98),legend.justification = c(0,1),text = element_text(lineheight = 0.5))

x<-seq(0,1,0.0001) 
qu <- 0.3
ytop<-dnorm(qu)
MyDF<-data.frame(x=x*100,y=dnorm(x,0,qu))
MyDF$y <- MyDF$y/sum(MyDF$y)
p<-qplot(x=MyDF$x,y=MyDF$y,geom="line") 
shade1 <- rbind(c(25,0), subset(MyDF, x > 25))
shade2 <- rbind(c(50,0), subset(MyDF, x > 50))
shade3 <- rbind(c(75,0), subset(MyDF, x > 75))
d1 <- MyDF$y[MyDF$x==25];D1 <- sum(shade1$y)
d2 <- MyDF$y[MyDF$x==50];D2 <- sum(shade2$y)
d3 <- MyDF$y[MyDF$x==75];D3 <- sum(shade3$y)
p_frac_illu <- p + xlab('Duty Cycle (%)') +ylab('Probability Density')+
  geom_polygon(data = shade1, aes(x, y),fill=colr1[3])+geom_polygon(data = shade2, aes(x, y),fill=colr1[2])+geom_polygon(data = shade3, aes(x, y),fill=colr1[4])+
  geom_segment(aes(x=50,y=d1,xend=25,yend=d1))+annotate('text',x=35,y=d1+d1*.13,label=sprintf('Threhold=0.25\nRAT=%.1f%%',D1*100),size=8)+
  geom_segment(aes(x=75,y=d2,xend=50,yend=d2))+annotate('text',x=60,y=d2+d1*.13,label=sprintf('Threhold=0.50\nRAT=%.1f%%',D2*100),size=8)+
  geom_segment(aes(x=100,y=d3,xend=75,yend=d3))+annotate('text',x=85,y=d3+d1*.13,label=sprintf('Threhold=0.75\nRAT=%.1f%%',D3*100),size=8)+
  gen_theme()+theme(axis.text.y=element_blank())
  
# S3.3
  
# S4. Save ------------------------------------
save_fig(p_frac_frA_grt+
           geom_segment(aes(x=45,y=0,xend=45,yend=15),size=0.5)+
           annotate("text", x=45, y=17,label= 'Critical RAT: 45%',size=10),'frac_fr50')
save_fig(p_frac_frB_grt+
           geom_segment(aes(x=30,y=0,xend=30,yend=15),size=0.5)+
           annotate("text", x=30, y=17,label= 'Critical RAT: 30%',size=10),'frac_fr60')
save_fig(p_frac_frC_grt+
           geom_segment(aes(x=20,y=0,xend=20,yend=15),size=0.5)+
           annotate("text", x=20, y=17,label= 'Critical RAT: 20%',size=10),'frac_fr70')
save_fig(p_frac_frD_grt+
           geom_segment(aes(x=15,y=0,xend=15,yend=15),size=0.5)+
           annotate("text", x=17, y=17,label= 'Critical RAT: 15%',size=10),'frac_fr80')

save_fig(p_frac_frA_les+xlab('RAT of Small Duty Cycle (%)'),'frac_fr05')
save_fig(p_frac_frB_les+xlab('RAT of Small Duty Cycle (%)'),'frac_fr10')
save_fig(p_frac_frC_les+xlab('RAT of Small Duty Cycle (%)'),'frac_fr15')
save_fig(p_frac_frD_les+xlab('RAT of Small Duty Cycle (%)'),'frac_fr20')
save_fig(p_frac_illu,'frac_illu')
save(p_frac_frA,p_frac_frB,p_frac_frC,p_frac_frD,p_frac_corr,p_frac_illu,file=file.path(dir_data,'Paper','Rda','p_frac.Rda'))
