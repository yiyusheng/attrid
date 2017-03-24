#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: head.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-05-27 15:36:07
#
# Last   modified: 2016-10-10 16:31:34
#
#
#


# source('base.R')
# osFlag = Sys.info()[1] == 'Windows' 
# dirName <- 'attrid'
# 
# colorSet <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# attrName <- c('util','rps','iopsr','wps','iopsw')
# 
# 
# if (osFlag){
#   dir_code <- paste('D:/Git/',dirName,sep='')
#   dir_data <- paste('D:/Data/',dirName,sep='')
#   dir_dataSource <- 'D:/Data/dataLoadforDiskAnalysis'
#   source('D:/Git/R_Function/Rfun.R')
# }else{
#   dir_code <- paste('/home/yiyusheng/Code/R/',dirName,sep='')
#   dir_data <- paste('/home/yiyusheng/Data/',dirName,sep='')
#   dir_dataSource <- '/home/yiyusheng/Data/dataLoadforDiskAnalysis'
#   dir_dataten <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid/'
#   dir_datatendcast <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcast/'
#   dir_datatendcastClear <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcastClear/'
#   source('/home/yiyusheng/Code/R/R_Function/Rfun.R')
#   # options('width' = 150)
# }
# 
# require('caret')
# require('e1071')
# require('scales')
# require('grid')
# require('ggplot2')
# require('reshape2')
# require('plyr')