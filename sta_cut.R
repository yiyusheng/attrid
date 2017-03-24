#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_cut.R
#
# Description: I observed that wps is aggragate in 16-64 kB/s indicating a possible workload of logging. 
# I wanna know whether that's true for all svrid or just for a range of svrid.
# Because 16 kB/s means 1.38GB/day, it is a large consume of disk capacity.
# Generally, I statistic all five attributes with the same method.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-23 10:48:05
#
# Last   modified: 2017-03-23 11:43:44
#
#
#

rm(list = ls());source('~/rhead')

sta_attrid <- function(dd,aid,fnm){
  level_aid <- paste(aid,'_level',sep='')
  cut_aid <- cutList[[paste('cut_',aid,sep='')]]
  dd[[level_aid]] <- cut_level(dd[[aid]],cut_aid,F)

  sta_aid_svrid <- as.data.frame(do.call(rbind,tapply(dd[[level_aid]],dd$svrid,table)))
  sta_aid_svrid <- cbind(levels(dd$svrid),sta_aid_svrid)
  names(sta_aid_svrid) <- c('svrid',paste('X',cut_aid[-length(cut_aid)],sep=''))
  sta_aid_svrid$fn <- fnm
  
  dd$svridtime <- factor(paste(dd$svrid,dd$time,sep = '#'))
  sta_aid_day <- as.data.frame(do.call(rbind,tapply(dd[[level_aid]],dd$svridtime,table)))
  sta_aid_day <- cbind(levels(dd$svridtime),sta_aid_day)
  names(sta_aid_day) <- c('svriddate',paste('X',cut_aid[-length(cut_aid)],sep=''))
  tmp <- strsplit(fct2ori(sta_aid_day$svriddate),split = '#')
  sta_aid_day$svrid <- factor(sapply(tmp,'[[',1))
  sta_aid_day$time <- as.Date(sapply(tmp,'[[',2))
  sta_aid_day <- sta_aid_day[,c('svrid','time',paste('X',cut_aid[-length(cut_aid)],sep=''))]
  row.names(sta_aid_day) <- NULL
  sta_aid_day$fn <- fnm
  
  list(sta_aid_svrid,sta_aid_day)
}

sta_cut <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  dd <- filter_badiops_NA(dt_dcast,attrName)
  dd$time <- as.Date(dd$time)
  # dd <- factorX(subset(dd,svrid %in% levels(dd$svrid)[1:10]))
  
  list[sta_util_svrid,sta_util_day] <- sta_attrid(dd,'util')
  list[sta_rps_svrid,sta_rps_day] <- sta_attrid(dd,'rps')
  list[sta_iopsr_svrid,sta_iopsr_day] <- sta_attrid(dd,'iopsr')
  list[sta_wps_svrid,sta_wps_day] <- sta_attrid(dd,'wps')
  list[sta_iopsw_svrid,sta_iopsw_day] <- sta_attrid(dd,'iopsw')
  
  sta_day <- list(sta_util_day,sta_rps_day,sta_iopsr_day,sta_wps_day,sta_iopsw_day)
  sta_svrid <- list(sta_util_svrid,sta_rps_svrid,sta_iopsr_svrid,sta_wps_svrid,sta_iopsw_svrid)
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  list(sta_day,sta_svrid)
}

###### MAIN STATISTIC ######
load(file.path(dir_data,'sta_dcastClear.Rda'))
fname <- list.files(dir_datatendcastClear)
fname <- fname[!grepl('a\\d.*',fname)]
attrName <- c('util','rps','iopsr','wps','iopsw')
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)),cut_rps = c(0,2^(seq(0,16,1)),1e6),cut_iopsr = c(0,2^(seq(0,16,1)),1e6),
                cut_wps = c(0,2^(seq(0,16,1)),1e6),cut_iopsw = c(0,2^(seq(0,16,1)),1e6))
idx <- seq_len(length(fname))
r <- foreachX(idx,'sta_cut')
save(r,file = file.path(dir_data,'sta_cut.Rda'))

###### ANALYSIS ######
ss_util <- lapplyX(lapply(r,'[[',2),'[[',1)
ss_rps <- lapplyX(lapply(r,'[[',2),'[[',2)
ss_iopsr <- lapplyX(lapply(r,'[[',2),'[[',3)
ss_wps <- lapplyX(lapply(r,'[[',2),'[[',4)
ss_iopsw <- lapplyX(lapply(r,'[[',2),'[[',5)

# idle of write
col_value <- names(ss_wps)[grepl('X\\d+',names(ss_wps))]
ss_wps$count <- apply(ss_wps[,col_value],1,sum)
ss_wps$max <- apply(ss_wps[,col_value],1,function(x)names(ss_wps[,col_value])[which.max(x)])
table_wps <- melt(table(ss_wps$max))
ss_wps$bts_wtn <- as.numeric((as.matrix(ss_wps[,col_value]) %*% cutList$cut_wps[-length(cutList$cut_wps)])*300/1e6)
ss_wps$fn <- factor(r_sta_svrid$fn[match(ss_wps$svrid,r_sta_svrid$svrid)])
ss_wps$fclass <- gsub('\\d.*','',ss_wps$fn)
ss_wps$max <- factor(ss_wps$max)

# for different type of files
frac_max <- do.call(rbind,tapply(ss_wps$max,ss_wps$fclass,function(x)array_rate(table(x))))
frac_max <- melt(frac_max);frac_max$cut_value <- as.numeric(gsub('X','',frac_max$Var2))
ggplot(frac_max,aes(x = Var1,y = value,fill = factor(cut_value))) + geom_bar(stat = 'identity') + scale_fill_brewer(palette="Spectral")

# for rate [used in xmind]
ss_wps_rate <- ss_wps
ss_wps_rate[,col_value] <- roundX(ss_wps_rate[,col_value]/ss_wps_rate$count)
sub_idle <- subset(ss_wps_rate,max %in% c('X16','X32'))
sub_idle$frac_idle <- sub_idle$X16 + sub_idle$X32
summary(sub_idle$X16 + sub_idle$X32)
idle_range <- c(8*86400*365/1e6,64*86400*365/1e6)
ggplot(sub_idle,aes(log2(bts_wtn))) + geom_histogram(binwidth = 0.1) 
wps_idle <- sub_idle
save(wps_idle, file = file.path(dir_data,'sc_wps_idle.Rda'))

# what about read for these wps idle server
col_value <- names(ss_rps)[grepl('X\\d+',names(ss_rps))]
ss_rps_idlewps <- factorX(subset(ss_rps,svrid %in% sub_idle$svrid[sub_idle$frac_idle > median(sub_idle$frac_idle)]))
sri_rate <- ss_rps_idlewps
sri_rate$count <- apply(sri_rate[,col_value],1,sum)
sri_rate[,col_value] <- roundX(sri_rate[,col_value]/sri_rate$count)
sri_rate$max <- apply(sri_rate[,col_value],1,function(x)names(sri_rate[,col_value])[which.max(x)])
table_rps <- melt(table(sri_rate$max))
sri_rate$bts_wtn <- as.numeric((as.matrix(sri_rate[,col_value]) %*% cutList$cut_wps[-length(cutList$cut_wps)])*300/1e6)
sri_rate$fn <- factor(r_sta_svrid$fn[match(sri_rate$svrid,r_sta_svrid$svrid)])
sri_rate$fclass <- gsub('\\d.*','',sri_rate$fn)
sri_rate$max <- factor(sri_rate$max)
