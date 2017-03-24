#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: aggregate_day_meltdata.R
#
# Description: statistic data of 201608 to aggreate sum,count,sd,mean for each day
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 09:30:41
#
# Last   modified: 2017-02-13 11:04:22
#
#
#
rm(list = ls());source('head.R');library(plyr)

day_appre <- function(splitData,func){
  f <- get(func)
  r <- lapply(splitData,function(df){
    # cat(sprintf('%s\n',df$svrid[1]))
    r1 <- aggregate(df[,-c(1,2)],by = list(as.character(df$time)),function(x)round(f(x,na.rm = T),digits = 4))
    r1 <- cbind(df$svrid[1],r1)
    names(r1)[1:2] <- c('svrid','date')
    r1
    })
  do.call(rbind,r)
}

gen_sta <- function(i,cut_time){
  load(file.path(dir_datafile,fname[i]))
  data <- dt_dcast
  data <- iops_dcast_clear(data)
  data$time <- cut.POSIXt(data$time,cut_time,cut_time[-length(cut_time)])
  data <- factorX(iops_dcast_clear(data))
  splitData <- split(data,data$svrid)
  
  cat(sprintf('[%s]\t%s:START\n',date(),fname[i]))
  r_mean <- day_appre(splitData,'mean')
  r_sd <- day_appre(splitData,'sd')
  r_sum <- day_appre(splitData,'sum')
  
  r_mean <- iops_aggragate(r_mean)
  r_sd <- iops_aggragate(r_sd)
  r_sum <- iops_aggragate(r_sum)
  cat(sprintf('[%s]\t%s:END\n',date(),fname[i]))
  list(r_mean,r_sd,r_sum)
}

###### STATISTIC MAIN ######
dir_datafile <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcast'
fname <- list.files(dir_datafile)
cut_days <- seq.POSIXt(as.p('2014-07-01'),as.p('2015-07-01'),by = 'days')
cut_weeks <- seq.POSIXt(as.p('2014-07-01'),as.p('2015-07-01'),by = 'weeks');cut_weeks[length(cut_weeks)] <- as.p('2015-07-01')
cut_months <- seq.POSIXt(as.p('2014-07-01'),as.p('2015-07-01'),by = 'months')

require(doParallel)
idx <- seq_len(length(fname))
file.remove('out_sta')
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_sta')
registerDoParallel(ck)
r_day <- foreach(i = idx,.verbose = F) %dopar% tryCatch(gen_sta(i,cut_days),error = function(e){cat(sprintf("ERROR:%s\t%s\n",fname[i],e))})
r_week <- foreach(i = idx,.verbose = F) %dopar% tryCatch(gen_sta(i,cut_weeks),error = function(e){cat(sprintf("ERROR:%s\t%s\n",fname[i],e))})
r_month <- foreach(i = idx,.verbose = F) %dopar% tryCatch(gen_sta(i,cut_months),error = function(e){cat(sprintf("ERROR:%s\t%s\n",fname[i],e))})
stopCluster(ck)

r_mean <- do.call(rbind,lapply(r_day,'[[',1))
r_sd <- do.call(rbind,lapply(r_day,'[[',2))
r_sum <- do.call(rbind,lapply(r_day,'[[',3))
save(r_mean,r_sd,r_sum,file = file.path(dir_data,'perday_201608_dcast.Rda'))

r_mean <- do.call(rbind,lapply(r_week,'[[',1))
r_sd <- do.call(rbind,lapply(r_week,'[[',2))
r_sum <- do.call(rbind,lapply(r_week,'[[',3))
save(r_mean,r_sd,r_sum,file = file.path(dir_data,'perweek_201608_dcast.Rda'))

r_mean <- do.call(rbind,lapply(r_month,'[[',1))
r_sd <- do.call(rbind,lapply(r_month,'[[',2))
r_sum <- do.call(rbind,lapply(r_month,'[[',3))
save(r_mean,r_sd,r_sum,file = file.path(dir_data,'permonth_201608_dcast.Rda'))
