#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mcfFunc.R
#
# Description: main function to generate the mcf. we divide the event data and the cencored data.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-05 15:47:26
#
# Last   modified: 2017-05-05 15:47:29
#
#
#

mcf_cencored <- function(event_time,life_censored){
  uni_age <- sort(unique(c(event_time,life_censored$ageS,life_censored$ageE)))
  tableS <- melt(table(life_censored$ageS))
  tableE <- melt(table(life_censored$ageE))
  atrisk_age <- data.frame(age = uni_age,count = 0)
  
  # add count of tableSvalue to atrisk_age from idx to the end indicating onshelf
  for(i in seq_len(nrow(tableS))){
    idx <- which(atrisk_age$age == tableS$Var1[i])
    atrisk_age$count[idx:nrow(atrisk_age)] <- atrisk_age$count[idx:nrow(atrisk_age)] + tableS$value[i]
  }
  
  # minus count of tableE$value to atrisk_age from idx to the end indicating offshelf
  for(i in seq_len(nrow(tableE))){
    idx <- which(atrisk_age$age == tableE$Var1[i]) + 1
    if(idx > nrow(atrisk_age)) next
    atrisk_age$count[idx:nrow(atrisk_age)] <- atrisk_age$count[idx:nrow(atrisk_age)] - tableE$value[i]
  }
  
  fails_age <- melt(table(event_time))
  names(fails_age) <- c('age','countF')
  
  mcf_age <- merge(atrisk_age,fails_age,by = 'age',all = T)
  mcf_age$countF[is.na(mcf_age$countF)] <- 0
  mcf_age$rate_day <- mcf_age$countF/mcf_age$count
  mcf_age$mcf <- cumsum(mcf_age$rate_day)
  
  return(mcf_age)
}