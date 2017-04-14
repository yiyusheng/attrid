#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: partition_26.R
#
# Description: There are 26 cleared Rda files in Load_Data_2014. I partition the last one into two Rda files. 
# One contains 100 servers and the other includes the rest. I'll use the small one to debug.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-10 09:36:26
#
# Last   modified: 2017-04-10 09:36:28
#
#
#

rm(list = ls());source('~/rhead')
load(file.path(dir_data14,'data26.Rda'))
sid <- levels(DT$svrid)
sid26 <- sid[1:(length(sid) - 100)]
sid27 <- sid[(length(sid) - 99):length(sid)]
DTori <- DT

DT <- subsetX(DTori,svrid %in% sid26)
save(DT,file = file.path(dir_data14,'data26.Rda'))

DT <- subsetX(DTori,svrid %in% sid27)
save(DT,file = file.path(dir_data14,'data27.Rda'))