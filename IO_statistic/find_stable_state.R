#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: find_stable_state.R
#
# Description: find stable state generate from sta_state_cut.R and analysize it
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-09 21:13:04
#
# Last   modified: 2017-03-09 21:13:05
#
#
#
rm(list = ls());source('head.R')
load(file.path(dir_data,'data_month_state.Rda'))
load(file.path(dir_data,'perday_201608_dcast.Rda'));rm(r_sum,r_sd);r_mean_day <- r_mean


# filter svrid with less than 10 months data
sta_month <- melt(tapply(data_month_state$date,data_month_state$svrid,length))
data_month_state <- factorX(subset(data_month_state,svrid %in% sta_month$Var1[sta_month$value >= 10]))

# generate uni_level
data_month_state$uni_level <- fct2num(data_month_state$util_level) * 100 + fct2num(data_month_state$xps_level) * 10 + fct2num(data_month_state$iops_level)

# dcast by svrid and date
data_month_state_dcast <- dcast(data_month_state[,c('svrid','date','uni_level')],svrid~date,value.var = 'uni_level')
data_month_state_dcast$num_states <- apply(data_month_state_dcast[,-1],1,function(x)length(unique(x)))
data_month_state_dcast$frac_modal <- apply(data_month_state_dcast[,grepl('2014|2015',names(data_month_state_dcast))],1,function(x){
  r <- table(x)
  as.numeric(r[which.max(r)[1]])/length(x)
})

###### STATISTIC ######
sta_level <- melt_table(data_month_state$util_level,data_month_state$xps_level,data_month_state$iops_level)
names(sta_level) <- c('util','xps','iops','count')

sta_frac_ul <- melt_table(data_month_state_dcast$num_states,data_month_state_dcast$frac_modal)
names(sta_frac_ul) <- c('num_states','frac_modal','count')

sta_single_ul <- melt(table(data_month_state_dcast$`2014-07-01`[data_month_state_dcast$num_states == 1]))
names(sta_single_ul) <- c('state','count')


###### WATCH DATA ######
hard_io_svrid <- factorX(subset(data_month_state_dcast,`2014-07-01` == 444 & num_states == 1))
hard_perday <- factorX(subset(r_mean_day,svrid %in% hard_io_svrid$svrid))
splitHP <- split(hard_perday,hard_perday$svrid)
a <- splitHP[[1]]
a1 <- aggregate(hard_perday[,-c(1,2)],by = list(hard_perday$svrid),function(x)round(mean(x,na.rm = T),digits = 4))
a2 <- aggregate(hard_perday[,-c(1,2)],by = list(hard_perday$svrid),function(x)round(coef_var(x),digits = 4))


###### PLOT ######
ggplot(subset(a,Var1 == '60'),aes(Var3,Var2)) + geom_raster(aes(fill = value))
ggplot(a,aes(Var1,Var2)) + geom_point(aes(size = value,alpha = value,color = factor(Var3)))
b <- aggregate(data_month_state[,c('xps_level','util_level','iops_level')],by = list(data_month_state$svrid),function(x)length(unique(x)))
b$num_change_attr <- apply(b[,-1],1,function(x)sum(x == 1))
ggplot(data_month_state_dcast,aes(x = num_states)) + geom_histogram(binwidth = 1)
ggplot(sta_frac,aes(x = num_states,y = frac_modal)) + geom_raster(aes(fill = count))
