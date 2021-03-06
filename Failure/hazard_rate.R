#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: hazard_rate.R
#
# Description: generate hazard rate for virsualized disks
# First, calculate the startAge, endAge of recording and the existance of event
# Then, use Surv from OIsurv to calculate the harzard rate of different ages.
#
# [2017/04/24]NOTE: inhereted from hazard_rate_bck.R. It is used to generate hazard rate for failure of disk.
# I will modify it to adopt failure of storage.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-27 16:31:14
#
# Last   modified: 2017-04-24 10:55:10

library(OIsurv)

surv_disk <- function(DT,attr,units_time = 1,start_time = '2010-01-01'){
  # Add start Age
  DT$startAge <- difftime(as.POSIXct(start_time,tz = 'UTC'),DT$use_time,tz = 'UTC',units = 'days')
  DT$startAge <- round(as.numeric(DT$startAge)/units_time)

  # Add end Age
  DT$endAge <- difftime(DT$f_time,DT$use_time,tz = 'UTC',units = 'days')
  DT$endAge <- round(as.numeric(DT$endAge)/units_time)
  
  # Add event information (1 observed; 0 not observed)
  DT <- factorX(subset(DT,endAge != startAge & startAge >= 0))
  DT$event <- 0
  DT$event[DT$status == 'failed'] <- 1
  
  # generate hazard rate
  splitDT <- split(DT,DT[[attr]])
  each.result <- lapply(splitDT,function(x){
    tmp <- surv_fit(x,units_time)
    tmp$class <- x[[attr]][1]
    tmp
  })
  r <- do.call(rbind,each.result)
  r <- subset(r,time <= 5*365/units_time)
  
  # plot
  className <- unique(DT[[attr]])
  p <- sapply(className,function(x)plot_hazard_rate(r,x))
  
  # return
  list(p,r)
}

surv_fit <- function(DT,units_time){
  all.surv <- Surv(DT$startAge,DT$endAge,DT$event)
  all.fit <- survfit(all.surv ~ 1)
  
  all.parse.result <- data.frame(all.fit$time,all.fit$n.risk,all.fit$n.event,
                                 all.fit$n.censor,all.fit$n.enter,all.fit$surv)
  names(all.parse.result) <- c('time','numRisk','numEvent','numCensor','numEnter','Surv')
  
  all.parse.result$h <- all.parse.result$numEvent/all.parse.result$numRisk*365/units_time*100
  all.parse.result$f <- all.parse.result$numEvent/sum(all.parse.result$numEvent)*100
  all.parse.result <- subset(all.parse.result,!is.na(h))
}

plot_hazard_rate <- function(r,className){
  p <- ggplot(subset(r,class == className),aes(x = time,y = h)) + 
    geom_bar(stat = 'identity',position = 'dodge') + 
    xlab('Age') + ylab('Hazard Rate(%)') + ggtitle(className)
  print(p)
  list(p)
}

####################################
# fn <- 'dataPrepareAFR10-15.Rda'
# load(file.path(dir_data,fn))
# load(file.path(dir_dataSource,'load_ftr_attrid.Rda'))
# # For server model
# list[p.dev.month,r.dev.month] <- surv_disk(DT,30,'dClass')
# # list[p.dev.quarter,r.dev.quarter] <- surv_disk(DT,91,'dClass')
# 
# # For disk model
# DT$mainModel <- factor(disk_ip$mainModel[match(DT$svr_asset_id,disk_ip$svr_id)])
# 
# DT$model.dev <- paste(DT$dClass,DT$mainModel,sep='-')
# list[p.mod.month,r.mod.month] <- surv_disk(factorX(subset(DT,!grepl('2000',model.dev))),30,'model.dev')
# len.p <- length(p.mod.month)
# multiplot(p.mod.month[[1]],p.mod.month[[2]],
#           p.mod.month[[3]],p.mod.month[[4]],
#           p.mod.month[[5]],p.mod.month[[6]],
#           cols = len.p/2)
# list[p.mod.quarter,r.mod.quarter] <- surv_disk(DT,91,'mainModel')

# For interface
# p.mod.sata2.month <- plot_hazard_rate(subset(r.mod.month,grepl('NM0011',class)))
# p.mod.sata3.month <- plot_hazard_rate(subset(r.mod.month,!grepl('NM0011',class)))
# p.mod.sata2.quarter <- plot_hazard_rate(subset(r.mod.quarter,grepl('NM0011',class)))
# p.mod.sata3.quarter <- plot_hazard_rate(subset(r.mod.quarter,!grepl('NM0011',class)))

