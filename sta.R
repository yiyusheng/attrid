#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2016-12-15 09:38:53
#
#
#
rm(list = ls())
source('head.R')

fname <- paste('data',1:26,'.Rda',sep='')
dir_data5k <- '/home/yiyusheng/Data/allIOMerge/merge_5k'
require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(30,length(idx)),type = 'FORK',outfile = 'out_sta')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = F) %dopar% {
    load(file.path(dir_data5k,fname[i]))
    return(list(fn = fname[i],numItem = nrow(data),numUnit = length(levels(data$svrid))))
}
stopCluster(ck)
save(r,file = file.path(dir_data,'sta.Rda'))
ni <- sum(as.numeric(sapply(r,'[[','numItem')))
nu <- sum(as.numeric(sapply(r,'[[','numUnit')))
cat(sprintf('numItems: %.0f\tnumUnits: %.0f',ni,nu))

