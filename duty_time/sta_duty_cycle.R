#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_duty_cycle.R
#
# Description: statistic the size and coefficient variation of duty cycle
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-22 11:12:17
#
# Last   modified: 2017-05-22 11:12:19
#
#
#
rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')
load(file.path(dir_data,'duty_time_groupby_mean.Rda'))

# S0.preprocess
DT_mean <- replace_value(DT_mean,v2 = -1)
DT_mean <- subsetX(DT_mean,count > 15000)
quan_util <- with(DT_mean,data.frame(mean = quantileX(utilM),SD = quantileX(utilS),CV = quantileX(utilCV)))
quan_util$id <- 0:100

# S1.group
DT0 <- subset(DT_mean,utilM < 1)
DT1 <- subset(DT_mean,utilM < 2 & utilM >= 1)
DT2 <- subset(DT_mean,utilM >= 2 & utilM < 10)
DT3 <- subset(DT_mean,utilM >= 10)
p0 <- ggplot(smp_df(DT0,0.2),aes(x = utilM,y = utilS,color = utilCV)) + geom_point() + scale_colour_gradient(low = "red", high = "green")
p1 <- ggplot(smp_df(DT1,0.2),aes(x = utilM,y = utilS,color = utilCV)) + geom_point()+ scale_colour_gradient(low = "red", high = "green")
p2 <- ggplot(smp_df(DT2,0.2),aes(x = utilM,y = utilS,color = utilCV)) + geom_point()+ scale_colour_gradient(low = "red", high = "green")
p3 <- ggplot(smp_df(DT3,0.2),aes(x = utilM,y = utilS,color = utilCV)) + geom_point()+ scale_colour_gradient(low = "red", high = "green")
multiplot(p0,p1,p2,p3,cols = 2)

cut_mean <- sort(unique(round(quan_util$mean)));DT_mean$mean_level <- fct2num(cut(DT_mean$utilM,cut_mean,cut_mean[-length(cut_mean)]))
cut_SD <- sort(unique(round(quan_util$SD)));DT_mean$SD_level <- fct2num(cut(DT_mean$utilS,cut_SD,cut_SD[-length(cut_SD)]))
cut_CV <- sort(unique(round(quan_util$CV)));DT_mean$CV_level <- fct2num(cut(DT_mean$utilCV,cut_CV,cut_CV[-length(cut_CV)]))
DT_mean <- replace_value(DT_mean,v2 = -1)
tb_level <- with(DT_mean,melt(table(mean_level,SD_level,CV_level)))
tb_level <- subset(tb_level,value != 0)


# S2.plot
ggplot(smp_df(DT_mean,0.1),aes(x = log2(utilM))) + 
  geom_point(aes(y = utilS),color = cbPalette[1],alpha = 0.05) + 
  geom_point(aes(y = utilCV),color = cbPalette[6],alpha = 0.05)

ggplot(smp_df(DT_mean,0.1),aes(x = 2^utilM)) + 
  geom_point(aes(y = utilS),color = cbPalette[1],alpha = 0.5)
