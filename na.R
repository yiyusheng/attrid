#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: na.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-20 10:31:07
#
# Last   modified: 2016-09-20 10:31:09

# a wrong failure record makes original result wrong
# data.f <- subset(data.fMore,(f_time > as.POSIXct('2013-02-02') & f_time < as.POSIXct('2013-12-01')) |
#                    f_time > as.POSIXct('2014-01-01')) 

# wrong computering on time
# cmdb$shiptimeToLeft <- floor(lowerTime - cmdb$use_time)
# cmdb$shiptimeToRight <- floor(upperTime - cmdb$use_time)
# units(cmdb$shiptimeToLeft) <- 'days'
# units(cmdb$shiptimeToRight) <- 'days'
# cmdb$shiptimeToLeft <- as.numeric(cmdb$shiptimeToLeft)/365
# cmdb$shiptimeToRight <- as.numeric(cmdb$shiptimeToRight)/365

# a wrong setting
# cmdb$shTime <- floor(cmdb$shiptimeToLeft + (1/12))

### DO NOT DELETE ###
# cmdbio$total <- disk_ip$total[match(cmdbio$ip,disk_ip$ip)]
# # C
# cmdbio <- subset(cmdbio,!is.na(total) & (dClass != 'C' | total %in% c(500,250,1000)))
# # TS
# cmdbio$totalMerge <- cmdbio$total
# cmdbio$totalMerge[cmdbio$totalMerge <= 18000] <- 12000
# cmdbio$totalMerge[cmdbio$totalMerge > 18000] <- 24000
# cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 12000] <- 'TS1T'
# cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 24000] <- 'TS2T'

###################################
# S2. plot correlation between disk age and annual failure rate of pretented failure. 
# load(file.path(dir_dataSource,'flist(uwork[2012-2014]).Rda'))
# fTypeNeed <- c('硬盘故障（有冗余）','硬盘故障（有冗余，槽位未知）',
#             '硬盘故障（无冗余）','硬盘故障（无冗余，在线换盘）',
#             '硬盘即将故障（有冗余）','操作系统硬盘故障（无冗余）')
# 
# # NOTICE TIME
# flistUwork <- subset(data.flist,class == -1 & ftype %in% fTypeNeed &
#                      svr_id %in% tmp.cmdb$svr_asset_id & f_time > as.POSIXct('2014-01-01')) 
# 
# flistUwork <- mchAttr(flistUwork,tmp.cmdb,'svr_id','svr_asset_id',
#                       c('use_time','shTimeQu','dClass'))
# 
# cm1 <- AFR_attr_notime(flistUwork,tmp.cmdb,'shTimeQu','shTimeQu',1,dev = 'C')
# cm2 <- AFR_attr_notime(flistUwork,tmp.cmdb,'shTimeQu','shTimeQu',12,dev = 'TS')
# cm <- rbind(cm1,cm2)
# cm <- factorX(subset(cm,!is.na(AFR)))
# cm <- classExchg(cm)
# p <- AFR_plot(cm,'fig1')
####################################
# sum(cm$count_f[cm$item < 4 & cm$class == 'Nserv'])/sum(cm$count_io[cm$item < 4 & cm$class == 'Nserv'])*100
# sum(cm$count_f[cm$item >= 4 & cm$class == 'Nserv'])/sum(cm$count_io[cm$item >= 4 & cm$class == 'Nserv'])*100
# sum(cm$count_f[cm$item < 4 & cm$class == 'Sserv'])/sum(cm$count_io[cm$item < 4 & cm$class == 'Sserv'])/12*100
# sum(cm$count_f[cm$item >= 4 & cm$class == 'Sserv'])/sum(cm$count_io[cm$item >= 4 & cm$class == 'Sserv'])/12*100
# sum(cm$count_f[cm$class == 'Nserv'])/sum(cm$count_io[cm$class == 'Nserv'])*100
# sum(cm$count_f[cm$class == 'Sserv'])/sum(cm$count_io[cm$class == 'Sserv'])/12*100

# A1.plot
# analysis_sta <- function(sul,zero.rm){
#   if(zero.rm == T)sul[,names(sul) == 'X0'] <- NULL
#   sul_stdd <- cbind(sul$svrid,data.frame(t(apply(sul[,!grepl('svrid',names(sul))],1,function(x)c(round(x/sum(x)*100,digits = 4),sum(x))))))
#   names(sul_stdd)[ncol(sul_stdd)] <- 'sum';names(sul_stdd)[1] <- 'svrid'
#   sul_stdd <- subset(sul_stdd,sum > 50000)
#   
#   sul <- subset(sul,svrid %in% sul_stdd$svrid)
#   cut_util <- c(0,0,3,100,102,103)
#   sul_cut <- data.frame(svrid = sul$svrid,
#                         # low = apply(sul[,names(sul) %in% paste('X',cut_util[1]:cut_util[2],sep='')],1,sum),
#                         low = sul$X0,
#                         media = apply(sul[,names(sul) %in% paste('X',(cut_util[2]+1):cut_util[3],sep='')],1,sum),
#                         high = apply(sul[,names(sul) %in% paste('X',(cut_util[3]+1):cut_util[4],sep='')],1,sum))
#   sul_cut[,-1] <- t(conv_row_percent(sul_cut[,-1]))
#   ggplot(sul_cut,aes(x = low,y = high)) + geom_point(alpha = 0.01)
#   sul_cut$round_low <- factor(round(sul_cut$low))
#   ggplot(sul_cut,aes(x = round_low)) + geom_boxplot(aes(y = high))
#   
#   sul_mean <- data.frame(svrid = sul$svrid,
#                          mean = apply(sul[,-1],1,function(x)round(sum(x * 0:100)/sum(x),digits = 2)))
#   ggplot(sul_mean,aes(mean)) + stat_ecdf() + geom_point()
# }