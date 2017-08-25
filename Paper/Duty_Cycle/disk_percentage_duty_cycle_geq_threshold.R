#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: disk_percentage_duty_cycle_geq_threshold.R
#
# Description: We show the percentage of number of normal disks and failed disks when the threshold of duty cycle value changes
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-21 22:39:16
#
# Last   modified: 2017-08-21 22:39:17
#
#
#

# S1. Load functions and data ------------------------------------------------------------------------
rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'count_duty_cycle_value.Rda'))

# S2. Set point and generate the count ------------------------------------
cut_point <- seq(0,100,1)
count_point <- seq(20,100,10)
object_data <- duty_cycle_value_count
object_data$count <- apply(object_data[-1],1,sum)

# S3. Generate the percentage difference and failure rate ------------------------------------
itv <- 1
load(file.path(dir_data,paste(sprintf('fraction_great_duty_cycle_%i.Rda',itv))))
data_zero <- do.call(rbind,lapply(data_frList,function(df)df[df$fraction==0,]))
names(data_zero)[names(data_zero)=='class'] <- 'threshold'
data_zero$great_perc <- with(data_zero,100-percentage)
data_zero$great_percf <- with(data_zero,100-percf)
data_zero$rate_less <- with(data_zero,percf/percentage)
data_zero$rate_great <- with(data_zero,(100-percf)/(100-percentage))
p <- ggplot(data_zero,aes(x=threshold))+geom_bar(aes(y=AFR),stat = 'identity')+
  geom_point(aes(y=rate_great),color='red')+geom_line(aes(y=rate_great),color='red')+
  geom_point(aes(y=rate_less),color='blue')+geom_line(aes(y=rate_less),color='blue')

# S4. Generate the failure rate when we divide the disk by threshold into two parts.
itv <- 5
load(file.path(dir_data,paste(sprintf('fraction_great_duty_cycle_%i.Rda',itv))))
data_zero <- do.call(rbind,lapply(data_frList,function(df)df[df$fraction==0,]))
disk_count <- sum(data_frList[[1]]$count)
f_count <- sum(data_frList[[1]]$fCount)

# part greater than threshold
data_one <- data_zero
data_one$fraction <- 1
data_one$count <- disk_count-data_one$count
data_one$fCount <- f_count-data_one$fCount
data_one$AFR <- round(with(data_one,fCount/count*600),digits = 2)
data_one$percentage <- array_rate(data_one$count)
data_one$percf <- array_rate(data_one$fCount)
data_one$level <- 'low'
data_one$level[data_one$AFR>5] <- 'high'

# bind them
data_thred <- rbind(data_zero,data_one)
data_thred$threshold <- data_thred$class
data_thred$class <- 'Small part'
data_thred$class[data_thred$fraction==1] <- 'Large part'
corr <- c(with(data_thred[data_thred$class=='Small part',],cor(threshold,AFR)),
          with(data_thred[data_thred$class=='Large part',],cor(threshold,AFR)))


# plot failure rate
p_thred <- ggplot(data_thred,aes(x=threshold,y=AFR,fill=factor(class)))+geom_bar(stat='identity',position='dodge')+
  xlab('Threshold(%)')+ylab('Correlation Coefficient')+
  guides(fill = guide_legend(title=NULL),color=guide_legend(title='interval')) +
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.title = element_text(size=24),legend.position = 'bottom')

print(p_thred)
print(corr)
