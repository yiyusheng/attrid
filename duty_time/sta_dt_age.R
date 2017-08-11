#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_dt_age.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-06-06 16:49:24
#
# Last   modified: 2017-06-06 16:49:25
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/duty_time/');source('~/rhead')
source('~/Code/R/Disk_Workload/Failure/mcfFunc.R')
load(file.path(dir_data,'mcf201406_data.Rda'))


get_intpoint_of_line <- function(ageS,dtS,ageE,dtE){
  cat(sprintf('%d\n',count))
  slope <- (dtE-dtS)/(ageE-ageS)
  seqx <- ageS:ageE
  seqy <- (0:(length(seqx)-1))*slope + dtS
  t(rbind(seqx,seqy))
}

get_table <- function(points,int1 = 1,int2 = 1){
  points[,1] <- round(points[,1]/int1)
  points[,2] <- round(points[,2]/int2)
  r <- melt_table(points[,1],points[,2])
  names(r) <- c('age','dt','count')
  r$age <- factor(r$age)
  r
}

###### MAIN ######
life_censored <- setNames(merge(duty_censored[,c('svrid','lifeS','lifeE')],age_censored[,c('svrid','lifeS','lifeE')],
                                by = c('svrid')),c('svrid','dtS','dtE','ageS','ageE'))
points <- with(life_censored,mapply(get_intpoint_of_line,ageS,dtS,ageE,dtE,SIMPLIFY = F))
points <- do.call(rbind,points)
table30 <- get_table(points,30,30)
table180 <- get_table(points,180,30)
table365 <- get_table(points,365,30)

ggplot(subset(table365,age > 0),aes(x = dt,y = log2(count),group = age,color = age)) + geom_line() + geom_point()
