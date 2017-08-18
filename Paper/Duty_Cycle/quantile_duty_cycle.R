#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: quantile_duty_cycle.R
#
# Description: compare the quantile of duty cycle for each disk drive and correlate the pattern with disk failure.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-11 11:24:27
#
# Last   modified: 2017-08-11 11:24:28
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('../NewSC16/base_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))

# S1. Data Prepare
io14$ave_duty_cycle <- with(io14,sum_util/count)
col_quan <- paste('X',seq(5,95,5),sep='')

object_data <- r[,c('svrid',col_quan)]
object_data <- subset(object_data,svrid %in% io14$svrid)

object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
object_data$age <- cmdbSMP$age[match(object_data$svrid,cmdbSMP$svrid)]

object_data$ave_duty_cycle <- io14$ave_duty_cycle[match(object_data$svrid,io14$svrid)]
# object_data[,col_quan] <- object_data[,col_quan]-object_data$ave_duty_cycle
object_data[,col_quan] <- t(scale(t(object_data[,col_quan])))
object_data <- replace_value(object_data)

cut_point <- c(0:5,100)
object_data$ave_duty_cycle_level <- fct2num(cut(object_data$ave_duty_cycle,cut_point,cut_point[-1]))

object_data <- svrid_expand_disk(subset(object_data,!is.na(ave_duty_cycle_level)))

# S2. cluster for each group

km <- function(df,tag){
  km <- kmeans(df[,col_quan],centers = 5)
  df$kmgroup <- km$cluster
  
  # Diff
  rmean <- melt(t(aggregate(df[,col_quan],by = list(df$kmgroup),mean)))
  rsd <- melt(t(aggregate(df[,col_quan],by = list(df$kmgroup),sd)))
  rplot <- merge(rmean,rsd,by = c('Var1','Var2'))
  
  names(rplot) <- c('quantile','class','mean','sd')
  rplot <- subset(rplot,!grepl('Group',quantile))
  rplot$class <- factor(rplot$class)
  rplot$quantile <- as.numeric(gsub('X','',fct2ori(rplot$quantile)))
  
  mean_adc <- melt(t(aggregate(df[,"ave_duty_cycle"],by = list(df$kmgroup),mean)));mean_adc <- subset(mean_adc,!grepl('Group',Var1))
  ratio_adc <- data.frame(tapplyX(df$ave_duty_cycle,df$kmgroup,function(arr)c(summary(arr),length(arr))))
  ratio_adc$tag <- tag
  ratio_adc$class <- row.names(ratio_adc)
  ratio_adc$class_adc <- with(ratio_adc,paste(class,round(Mean,digits = 3),sep='-'))
  rplot$class_adc <- ratio_adc$class_adc[match(rplot$class,ratio_adc$class)]
  
  p_diff <- ggplot(rplot, aes(quantile)) + 
    geom_line(aes(y=mean,group = class_adc, colour=class_adc))+
    geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,group = class_adc,fill=class_adc), alpha=0.2)+
    ylab(paste('Centralized Duty Cycle(%)[adc=',tag,']',sep=''))+xlab('Percentage(%)')+coord_flip()+
    guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')

  # Failure rate
  obj_data <- df[,names(df)[!names(df) %in% col_quan]]
  fail_data <- f201409
  fail_data <- mchAttr(fail_data,obj_data,'svrid','svrid','kmgroup')
  
  fr <- ioAFR(obj_data,fail_data,attr = 'kmgroup')
  fr$class_adc <- ratio_adc$class_adc[match(fr$kmgroup,ratio_adc$class)]
  fr$count_rate <- array_rate(fr$count)
  fr$level <- 'low'
  fr$level[fr$AFR > 5] <- 'high'
  p_fr <- ggplot(fr,aes(x = class_adc,y = AFR)) + geom_bar(stat = 'identity')+
    xlab(paste('Centralized Duty Cycle(%)[adc=',tag,']',sep='')) + ylab('Failure Rate(%)') +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  
  return(list(p_diff,p_fr,ratio_adc))
}

splitDT <- split(object_data,object_data$ave_duty_cycle_level)
r_foo <- lapply(1:(length(splitDT)-1),function(i)km(splitDT[[i]],names(splitDT)[i]))
r_all_foo <- km(object_data,'all')
r_0_10 <- km(subset(object_data,ave_duty_cycle <= 10),'0-10')


p_diff <- c(r_all_foo[1],lapply(r_foo,'[[',1))
p_fr <- c(r_all_foo[2],lapply(r_foo,'[[',2))
ratio_adc <- rbind(r_all_foo[[3]],do.call(rbind,lapply(r_foo,'[[',3)))
multiplot(plotlist = p_diff,layout = matrix(1:6,ncol = 3,byrow=T))
multiplot(plotlist = p_fr,layout = matrix(1:6,ncol = 3,byrow=T))
