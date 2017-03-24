#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: check_anomaly_iops_xps.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-14 08:53:20
#
# Last   modified: 2017-03-14 08:53:23
#
#
#

rm(list = ls());source('head.R')
fn <- 'd1.Rda'
load(file.path(dir_datatendcast,fn))
dt_dcast <- iops_dcast_clear(dt_dcast,gsub('\\d.*','',fn))

# C1. range check
cut_iops <- c(-1,10^(0:10))
iopsr_melt <- melt(dt_dcast[,grepl('iopsr',names(dt_dcast))])
iopsw_melt <- melt(dt_dcast[,grepl('iopsw',names(dt_dcast))])

table_iopsr <- melt(table(cut(iopsr_melt$value,cut_iops,cut_iops[-length(cut_iops)],right = F)))
table_iopsw <- melt(table(cut(iopsw_melt$value,cut_iops,cut_iops[-length(cut_iops)],right = F)))

table_rps <- melt(table(cut(dt_dcast$rps,cut_iops,cut_iops[-length(cut_iops)],right = F)))
table_wps <- melt(table(cut(dt_dcast$wps,cut_iops,cut_iops[-length(cut_iops)],right = F)))

# C2. correlation coefficience
dd_iops_aggre <- iops_aggragate(dt_dcast)
corr_r <- cor(dd_iops_aggre$rps,dd_iops_aggre$iopsr,use = "pairwise.complete.obs")
corr_w <- cor(dd_iops_aggre$wps,dd_iops_aggre$iopsw,use = "pairwise.complete.obs")
corr_all <- cor(dd_iops_aggre$xps,dd_iops_aggre$iops,use = "pairwise.complete.obs")
