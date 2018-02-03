#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: ratio_rps_wps.R
#
# Description: Ratio of rps and wps
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-22 20:20:59
#
# Last   modified: 2017-08-22 20:21:00
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load Data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
load(file.path(dir_data,'quantile_bandwidth.Rda'))
load(file.path(dir_data,'quantile_ratio.Rda'))

DT_quan_all <- merge(quan_wps[,c('svrid','mean')],quan_xps[,c('svrid','mean')],by='svrid')
DT_quan_all$mean_all <- round(with(DT_quan_all,mean.x/mean.y),digits = 2)*100
DT_quan_all <- mchAttr(DT_quan_all,quan_ratio,'svrid','svrid',c('mean','sd',paste('Q',0:100,sep='')))
DT_quan_all$mean_individual <- round(quan_ratio$mean[match(DT_quan_all$svrid,quan_ratio$svrid)],digits = 4)*100
DT_quan_all <- subset(DT_quan_all,svrid %in% quan_ratio$svrid)

DT_quan <- gen_data(DT_quan_all,expand=T)
DT_quan <- subset(DT_quan,numD==12) # update 2018-01-04
DT_quan$change.ratio <- (DT_quan[['Q90']] - DT_quan[['Q10']])*100

# S2. ABW and Ratio
list[data_fr.abw,p_fr.abw,p_count.abw,data_corr.abw,data.abw]<- gen_result_feature(DT_quan,'abw',9000,bins=20)


# S3. Failure rate ------------------------------------
list[data_fr,p_fr,p_count,data_corr,data_ratio]<- gen_result_feature(DT_quan,'mean_individual',100)
list[data_fr1,p_fr1,p_count1,data_corr1,data_ratio1]<- gen_result_feature(DT_quan,'mean_all',100)
list[data_fr.cr,p_fr.cr,p_count.cr,data_corr.cr,data_ratio.cr]<- gen_result_feature(DT_quan,'change.ratio',100)


# S_end. plot ------------------------------------
p_ratio_fr <- p_fr+xlab('ART (%)')+ylim(c(0,6))+scale_fill_manual(values=c('grey60','grey20'))+ ylab('AFR (%)')##
# p_ratio_dist <- p_count+xlab('ART (%)')+coord_cartesian(ylim=c(0,25))  + ylab('Percentage (%)')
p_cr_fr <- p_fr.cr+xlab('Change of Ratio (%)')+scale_fill_manual(values=c('grey60','grey20'))##

p_abw_ratio.quantile <- plot_relationship_quantile(data.abw,'abw_level','mean_individual')+
  xlab('ABW (KB/s)')+ylab('ART (%)')+theme(legend.position = c(0.95,0.05),legend.justification = c(1,0))+
  scale_fill_manual(values=c('grey70','grey50','grey30','grey20'))##

data_ratio <- binning_data(DT = data_ratio,attr = 'abw',attr_max = 9000,bins = 3)
table_ratio <- setNames(melt_table(data_ratio$mean_individual_level,data_ratio$abw_level),c('ratio','abw','count'))
table_ratio$rate <- table_ratio$count/sum(table_ratio$count)*100
bin_width <- diff(sort(unique(table_ratio$ratio))[c(1,2)])
p_ratio_dist <- ggplot(table_ratio,aes(x=ratio))+geom_bar(aes(y=rate,fill=factor(abw)),stat = 'identity',position=position_nudge(x=-bin_width/2))+
  xlab('ART (%)') + ylab('Percentage(%)')+guides(fill = guide_legend(title='ABW (KB/S)')) + gen_theme()+scale_fill_manual(values=c('grey70','grey40','grey10')) ##

save_fig(p_ratio_fr,'ratio_fr')
save_fig(p_ratio_dist,'ratio_dist')
save_fig(p_abw_ratio.quantile,'ratio_abw')
save_fig(p_cr_fr,'ratio_cr_fr')
save(p_ratio_fr,p_ratio_dist,p_abw_ratio.quantile,file=file.path(dir_data,'Paper','Rda','p_ratio.Rda'))
