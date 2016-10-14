#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: test_surv.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-27 16:31:14
#
# Last   modified: 2016-09-27 16:31:16
rm(list = ls())
source('head.R')
library(OIsurv)
source('ggsurv.R')

####################################
fn <- 'dataPrepareAFR10-15.Rda'
load(file.path(dir_data,fn))
load(file.path(dir_dataSource,'load_ftr_attrid.Rda'))

surv_fit <- function(virtDC,units.time){
  all.surv <- Surv(virtDC$startAge,virtDC$endAge,virtDC$event)
  all.fit <- survfit(all.surv ~ 1)
  
  all.parse.result <- data.frame(all.fit$time,all.fit$n.risk,all.fit$n.event,
                                 all.fit$n.censor,all.fit$n.enter,all.fit$surv)
  names(all.parse.result) <- c('time','numRisk','numEvent','numCensor','numEnter','Surv')
  
  all.parse.result$h <- all.parse.result$numEvent/all.parse.result$numRisk*365/units.time*100
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

surv_disk <- function(virtDC,units.time = 1,attr){
  # Add start Age
  virtDC$startAge <- difftime(as.POSIXct('2010-01-01',tz = 'UTC'),virtDC$use_time,tz = 'UTC',units = 'days')
  virtDC$startAge <- round(as.numeric(virtDC$startAge)/units.time)
  virtDC$startAge[virtDC$startAge < 0] <- 0
  
  # Add end Age
  virtDC$endAge <- difftime(virtDC$f_time,virtDC$use_time,tz = 'UTC',units = 'days')
  virtDC$endAge <- round(as.numeric(virtDC$endAge)/units.time)
  virtDC <- subset(virtDC,endAge != startAge)
  
  # Add event information (1 observed; 0 not observed)
  virtDC$event <- 0
  virtDC$event[virtDC$status == 'failed'] <- 1
  
  # surv
  split.virtDC <- split(virtDC,virtDC[[attr]])
  each.result <- lapply(split.virtDC,function(x){
    tmp <- surv_fit(x,units.time)
    tmp$class <- x[[attr]][1]
    tmp
  })
  r <- do.call(rbind,each.result)
  r <- subset(r,time <= 5*365/units.time)
  
  # plot
  className <- unique(virtDC[[attr]])
  p <- sapply(className,function(x)plot_hazard_rate(r,x))
  
  # return
  list(p,r)
}



# For server model
list[p.dev.month,r.dev.month] <- surv_disk(virtDC,30,'dClass')
# list[p.dev.quarter,r.dev.quarter] <- surv_disk(virtDC,91,'dClass')

# For disk model
virtDC$mainModel <- factor(disk_ip$mainModel[match(virtDC$svr_asset_id,disk_ip$svr_id)])

virtDC$model.dev <- paste(virtDC$dClass,virtDC$mainModel,sep='-')
list[p.mod.month,r.mod.month] <- surv_disk(factorX(subset(virtDC,!grepl('2000',model.dev))),30,'model.dev')
len.p <- length(p.mod.month)
multiplot(p.mod.month[[1]],p.mod.month[[2]],
          p.mod.month[[3]],p.mod.month[[4]],
          p.mod.month[[5]],p.mod.month[[6]],
          cols = len.p/2)
# list[p.mod.quarter,r.mod.quarter] <- surv_disk(virtDC,91,'mainModel')

# For interface
# p.mod.sata2.month <- plot_hazard_rate(subset(r.mod.month,grepl('NM0011',class)))
# p.mod.sata3.month <- plot_hazard_rate(subset(r.mod.month,!grepl('NM0011',class)))
# p.mod.sata2.quarter <- plot_hazard_rate(subset(r.mod.quarter,grepl('NM0011',class)))
# p.mod.sata3.quarter <- plot_hazard_rate(subset(r.mod.quarter,!grepl('NM0011',class)))

