#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_level_xps_rela.R
#
# Description: correlate level of rps and level of wps to verify the idler/busyr is corresponding to a idlew/busyw
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-30 11:18:25
#
# Last   modified: 2017-03-30 11:18:27
#
#
#
rm(list = ls());source('~/rhead')

sta_level_xps_rela <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  dd <- filter_badiops_NA(dt_dcast,attrName,fn)
  # dd <- subsetX(dd,svrid %in% levels(dd$svrid)[1:50])
  
  dd$rps_level <- sort_level(cut_level(dd$rps,cutList$cut_rps,f2n = F))
  dd$wps_level <- sort_level(cut_level(dd$wps,cutList$cut_wps,f2n = F))
  dd$xps_diff <- sort_level(factor(as.numeric(dd$rps_level)*10 + as.numeric(dd$wps_level)))

  r <- data.frame(tapplyX(dd$xps_diff,dd$svrid,function(x)table(x)))
  r <- cbind(levels(dd$svrid),r)
  names(r)[1] <- 'svrid'
  r
}

###### MAIN ######
load(file.path(dir_data,'sta_dcastClear.Rda'))
fname <- list.files(dir_datatendcastClear)
attrName <- c('util','rps','wps','iopsr','iopsw')
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)),
                cut_rps = c(0,1,1024,1e6),cut_iopsr = c(0,2^(seq(0,16,1)),1e6),cut_sizer = roundX(c(0,2^(seq(-6.5,1.5,0.5)),10)),
                cut_wps = c(0,64,4096,1e6),cut_iopsw = c(0,2^(seq(0,16,1)),1e6),cut_sizew = roundX(c(0,2^(seq(-6.5,1.5,0.5)),10)))
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_level_xps_rela')
sta_level_xps_rela <- do.call(rbind,r)
sta_level_xps_rela$svrid <- sort_level(sta_level_xps_rela$svrid)
save(cutList,sta_level_xps_rela,file = file.path(dir_data,'sta_level_xps_rela.Rda'))

###### ANSLYSIS ######
slxr <- subsetX(sta_level_xps_rela,svrid %in% r_sta_svrid$svrid[r_sta_svrid$dayCount > 300])
col_value <- names(slxr)[grepl('X\\d+',names(slxr))]
slxr[,col_value] <- t(apply(slxr[,col_value],1,array_rate))
slxr$idler <- rowSums(slxr[,c('X11','X12','X13')])
slxr$idlew <- rowSums(slxr[,c('X11','X21','X31')])

table_slxr <- melt(table(round(slxr$idler,digits = 2),round(slxr$idlew,digits = 2)))
table_slxr$value[table_slxr$value == 0] <- 0.5
ggplot(table_slxr,aes(x = Var1,y = Var2,fill = log2(value))) + geom_raster() #useful

slxr$diff <- slxr$idler - slxr$idlew
slxr <- slxr[,setdiff(names(slxr),c('X22','X23','X32','X33'))]
slxr$Xrest <- with(slxr,X12 + X13 + X21 + X31)
ggplot(slxr,aes(diff)) + geom_histogram(binwidth = 0.1)
ggplot(slxr,aes(x = X11,y = abs(diff))) + geom_point(alpha = 0.1)

lapplyX(seq(0,0.5,0.05),function(x)nrow(subset(slxr,idler <= x & idlew <= x))/nrow(slxr))

slxr$idler_round <- round(slxr$idler,digits = 1)
idle_rate <- list2df(tapply(slxr$diff,slxr$idler_round,function(x)sum(abs(x) < 0.1)/length(x)),n = c('rate','idler'))
ggplot(idle_rate,aes(x = as.numeric(idler),y = rate)) + geom_bar(stat = 'identity')
