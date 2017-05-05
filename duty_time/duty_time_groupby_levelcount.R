#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: duty_time_group.R
#
# Description: we use util to represent duty time and tag each util as real (real workload) or basic (basic workload).
# then we statistic count of util and sum of util for real and basic for each servers.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-12 10:44:32
#
# Last   modified: 2017-04-12 10:44:34
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead')

gen_util_sta <- function(df){
  tmp_count <- tapply(df$util,df$tag,length)
  tmp_count[is.na(tmp_count)] <- 0
  names(tmp_count) <- paste('count',1:4,sep='')
  
  tmp_sum <- tapply(df$util,df$tag,sum)
  tmp_sum[is.na(tmp_sum)] <- 0
  names(tmp_sum) <- paste('sum',1:4,sep='')
  
  r <- data.frame(svrid = df$svrid[1],
             util_count = nrow(df),t(tmp_count),
             util_sum = sum(df$util)/100,t(tmp_sum)/100)
}

gen_2d_class <- function(df){
  r1 <- melt(table(df$util_level,df$tag))
  r1$svrid <- fct2ori(df$svrid[1])
  return(r1)
}

duty_time_group <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSTART!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  
  DT$tag <- tagSet[1]
  DT$tag[DT$rps > 0 & DT$wps <= 64] <- tagSet[2]
  DT$tag[DT$rps == 0 & DT$wps > 64] <- tagSet[3]
  DT$tag[DT$rps > 0 & DT$wps > 64] <- tagSet[4]
  DT$tag <- factor(DT$tag,levels = tagSet)
  
  DT$util_level <- cut_level(DT$util,cutList$cut_util,F)
  
  splitDT <- split(DT,DT$svrid)
  # r <- lapplyX(splitDT,gen_util_sta)
  r <- lapply(splitDT,gen_2d_class)

  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  return(r)
}

###### MAIN1: Generate feature for svrid ######
dir_dataset <- dir_data14DC
cutList <- list(cut_util = sort(c(0,3,2^(0:6),95,101)))
tagSet <- c('idle','busy1','busy2','busy3')

fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'duty_time_group')
duty_time_svrid <- unlist(r,recursive = F)

col_class <- c(paste(rep(c('A','B','C','D'),each = 10),rep(1:10,4),sep=''))
dts_rate <- sapply(duty_time_svrid,function(df)array_rate(df$value))
dts_rate <- data.frame(t(dts_rate))
names(dts_rate) <- col_class


###### MAIN2: cluster for all svrid ######
system.time(model <- kmeans(dts_rate,centers = 10,nstart = 10,algorithm = 'Hartigan-Wong'))
roundX(model$centers)
centers <- apply(model$centers,1,function(x){
  r <- data.frame(roundX(matrix(x,byrow = T,nrow = 4)))
  r$rowS <- rowSums(r)
  r <- rbind(r,colSums(r))
  return(r)
})
svrid_centers <- data.frame(svrid = row.names(dts_rate),center = model$cluster)
save(svrid_centers,model,centers,duty_time_svrid,dts_rate,file = file.path(dir_data,'duty_time_group.Rda'))
