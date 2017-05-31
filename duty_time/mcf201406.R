#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mcf201406.R
#
# Description: comparison of mcf-age and mcf-dutytime. generate mcf-age broken by util-level
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-21 15:35:30
#
# Last   modified: 2017-05-23 09:32:30
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/duty_time/');source('~/rhead')
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
source('~/Code/R/Disk_Workload/Failure/mcfFunc.R')

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
cmdb_censor$lifeS <- round(as.numeric(difftime(dateS,cmdb_censor$utime)))
cmdb_censor$lifeS[cmdb_censor$lifeS < 0] <- 0
cmdb_censor$lifeE <- round(as.numeric(difftime(dateE,cmdb_censor$utime)))

# S4. generate mcf of age
age_censored <- cmdb_censor[,c('svrid','lifeS','lifeE')]
age_censored <- subsetX(age_censored,svrid %in% DT_mean$svrid & lifeE < 365*5) #124677
age_censored$util <- DT_mean$utilM[match(age_censored$svrid,DT_mean$svrid)]
age_event <- FR[,c('svrid','life')]
age_event <- subsetX(age_event, svrid %in% age_censored$svrid) #3811
age_event$util <- DT_mean$utilM[match(age_event$svrid,DT_mean$svrid)]
mcf_age <- mcf_cencored(age_event,age_censored)
ggplot(mcf_age,aes(x = life)) + geom_line(aes(y = mcf))

# S5. generate mcf of duty time
duty_censored <- age_censored
duty_censored$lifeS <- floor(with(duty_censored,lifeS*util/100))
duty_censored$lifeE <- floor(with(duty_censored,lifeE*util/100))
duty_event <- age_event
duty_event$life <- floor(duty_event$life*duty_event$util/100)
mcf_duty <- mcf_cencored(duty_event,duty_censored)
ggplot(mcf_duty,aes(x = life)) + geom_line(aes(y = mcf))
save(dateS,dateE,FR,cmdb_censor,age_censored,age_event,duty_censored,duty_event,file = file.path(dir_data,'mcf201406_data.Rda'))

# S6. merge mcf of age and duty time
mcf_duty$class <- 'duty time'
mcf_age$class <- 'age'
mcf_rbind <- rbind(mcf_duty,mcf_age)
ggplot(mcf_rbind,aes(x = life,y = mcf,linetype = class)) + geom_line()

# S7. mcf on age for util_level [figure in paper]
DT_mean$util_level <- cut_level(DT_mean$utilM,c(0,1,2,5,10,20,50,101),f2n = F)
# DT_mean$util_level <- cut_level(DT_mean$utilM,seq(0,101))
mcf_util_level <- mcf_group(age_event,age_censored,DT_mean,'util_level')
mcf_util_level$util_level <- sort_level(mcf_util_level$util_level)
p <- ggplot(mcf_util_level,aes(x = life/365)) + 
  geom_line(aes(y = mcf,group = util_level,color = util_level)) + geom_hline(aes(yintercept = 1),linetype = 2) + 
  scale_x_continuous(breaks = seq(0,5,0.5)) + scale_y_continuous(breaks = seq(0,3,0.5)) + scale_color_brewer(palette = 'Greens') +
  guides(color = guide_legend(title = 'group of\nutilization')) +
  xlab('Age (yeas)') + ylab('Mean Cumulative Function') + 
  theme(legend.position = c(0.05,0.95), legend.justification = c(0,1), legend.background = element_rect(fill = alpha('grey',0.5)),
        axis.text = element_text(size = 24),axis.title = element_text(size = 26),legend.text = element_text(size = 20),legend.title = element_text(size = 20))
ggsave(file = file.path(dir_data,'paper1','MCF_util.eps'),plot = p,width = 10, height = 6, dpi = 100)
p
