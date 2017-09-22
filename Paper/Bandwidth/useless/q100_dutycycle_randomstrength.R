#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: q100_dutycycle_randomstrength.R
#
# Description: generate the relationship among q100(dc), q100(rs) and failure rate
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-31 16:10:09
#
# Last   modified: 2017-08-31 16:10:11
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

# S1. Load data ------------------------------------
load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'quantile_random_strength.Rda'))
load(file.path(dir_data,'quantile_dutycycle.Rda'))
q100 <- merge(quan_random[,c('svrid','Q100')],r[,c('svrid','Q100')],by='svrid')
names(q100) <- c('svrid','Q100_random_strength','Q100_duty_cycle')
q100$Q100_random_strength_trunc <- scale(trunc_level(q100,'Q100_random_strength',1,20),scale = F)
q100$Q100_duty_cycle_trunc <- scale(trunc_level(q100,'Q100_duty_cycle',100,20)/100,scale=F)
q100$Q200 <- with(q100,Q100_duty_cycle_trunc+Q100_random_strength_trunc)
q100$Q200 <- q100$Q200+min(q100$Q200)*(-1)
q100$Q200_trunc <- trunc_level(q100,'Q200',2,20)

# S2. Generate failure rate  ------------------------------------
list[data_fr,p_fr,p_count] <- gen_fr(q100,'Q200',prt=F)



# S3. 2D failure rate. useless due to imbalance distribution
# list[data_fr] <- gen_fr(q100,c('Q100_duty_cycle_trunc','Q100_random_strength_trunc'),prt=F)
# p_fr <- ggplot(subset(data_fr,count>100)) + geom_raster(aes(x=Q100_duty_cycle_trunc,y=Q100_random_strength_trunc,fill=AFR))+
#   geom_contour(aes(x=Q100_duty_cycle_trunc,y=Q100_random_strength_trunc,z=AFR),binwidth=2)+
#   scale_fill_gradientn(colours=c("#0000FF","#FFFFFF","#FF0000"))
# print(p_fr)
# 
# p_count <- ggplot(subset(data_fr,count>100)) + geom_raster(aes(x=Q100_duty_cycle_trunc,y=Q100_random_strength_trunc,fill=count))+
#   geom_contour(aes(x=Q100_duty_cycle_trunc,y=Q100_random_strength_trunc,z=count),binwidth=2)+
#   scale_fill_gradientn(colours=c("#0000FF","#FFFFFF","#FF0000"))
# print(p_count)
# 
# a <- dcast(Q100_duty_cycle_trunc~Q100_random_strength_trunc,data=data_fr,value.var = 'AFR')
# row.names(a) <- a$Q100_duty_cycle_trunc;a$Q100_duty_cycle_trunc <- NULL
# a <- rbind(a,colSums(a));a <- cbind(a,rowSums(a))
# 
# 
# b <- dcast(Q100_duty_cycle_trunc~Q100_random_strength_trunc,data=data_fr,value.var = 'percentage')
# row.names(b) <- b$Q100_duty_cycle_trunc;b$Q100_duty_cycle_trunc <- NULL
# b <- rbind(b,colSums(b));b <- cbind(b,rowSums(b))
