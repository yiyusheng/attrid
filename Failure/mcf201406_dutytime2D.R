#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mcf201406_dutytime2D.R
#
# Description: generate mcf with an 2D time view (age + duty time). failure list is from 201406-201409
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-21 15:35:30
#
# Last   modified: 2017-04-21 15:35:32
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('Failure/mcfFunc.R')

# S1. Load
load(file.path(dir_dataCF,'uwork2014_06_09.Rda')) #52376
load(file.path(dir_dataCF,'serverInfo.Rda'))
load(file.path(dir_dataDW,'duty_time_groupby_mean.Rda'))
dateS <- as.p('2014-06-01')
dateE <- as.p('2014-10-01')

# S2. preprocess of fail record
FR <- f2014_06_09
FR <- revise_ftype(FR)
FR <- filter_ip(FR) #52273
FR <- check_disk_replacement(FR) #9413
FR$utime <- cmdb$use_time[match(FR$svrid,cmdb$svr_asset_id)]
FR <- subsetX(FR,!is.na(utime)) #8988
FR$life <- round(as.numeric(difftime(FR$ftime,FR$utime,tz = 'UTC',units = 'days')))


# S3. preprocess of all objects
cmdb_censor <- cmdb[,c('svr_asset_id','use_time')]
names(cmdb_censor) <- c('svrid','utime')
cmdb_censor$ageS <- round(as.numeric(difftime(dateS,cmdb_censor$utime)))
cmdb_censor$ageS[cmdb_censor$ageS < 0] <- 0
cmdb_censor$ageE <- round(as.numeric(difftime(dateE,cmdb_censor$utime)))

# S4. group
DT_mean$util_level <- cut_level(DT_mean$utilM,c(0,1,2,5,10,20,50,101),f2n = F)
# DT_mean$util_level <- cut_level(DT_mean$utilM,seq(0,101))

# S4. data preparation
life_censored <- cmdb_censor[,c('svrid','ageS','ageE')]
life_censored$util <- roundX(DT_mean$utilM[match(life_censored$svrid,DT_mean$svrid)])
life_censored <- subsetX(life_censored,svrid %in% DT_mean$svrid & ageE < 365*5 & !is.na(util)) #124677
life_censored$dutyS <- round(life_censored$ageS*life_censored$util/100,digits = 0)
life_censored$dutyE <- round(life_censored$ageE*life_censored$util/100,digits = 0)

event_info <- FR[,c('svrid','life')]
event_info$util <- roundX(DT_mean$utilM[match(event_info$svrid,DT_mean$svrid)])
event_info <- subsetX(event_info, svrid %in% life_censored$svrid & !is.na(util)) #3811
event_info$duty <- round(event_info$life*event_info$util/100,digits = 0)

# generate mcf For all
atrisk7 <- mcf_cencored2D(event_info,life_censored,7)
atrisk30 <- mcf_cencored2D(event_info,life_censored,30)

at <- subset(atrisk7,count != 0)
at$count[at$count > 100] <- 100
a <- dcast(age~duty,data = atrisk30,value.var = 'count')
ggplot(at,aes(age,duty,fill = count)) + geom_raster() + geom_abline(slope = 1,color = 'red')

