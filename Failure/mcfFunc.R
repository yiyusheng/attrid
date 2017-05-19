#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mcfFunc.R
#
# Description: main function to generate the mcf. we divide the event data and the cencored data. 
# For hazard rate, the event takes place the same time as the right cencored time. So you need to set all event_time as the right time with event equaling to 1.
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

mcf_cencored <- function(event_info,life_censored){
  uni_age <- sort(unique(c(event_info$life,life_censored$ageS,life_censored$ageE)))
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
  
  fails_age <- melt(table(event_info$life))
  names(fails_age) <- c('age','countF')
  
  mcf_age <- merge(atrisk_age,fails_age,by = 'age',all = T)
  mcf_age$countF[is.na(mcf_age$countF)] <- 0
  mcf_age$rate_day <- mcf_age$countF/mcf_age$count
  mcf_age$mcf <- cumsum(mcf_age$rate_day)
  
  return(mcf_age)
}

mcf_group <- function(event_info,life_censored,df_group,attr){
  df_group$tmp <- df_group[[attr]]
  event_info$tmp <- df_group$tmp[match(event_info$svrid,df_group$svrid)]
  life_censored$tmp <- df_group$tmp[match(life_censored$svrid,df_group$svrid)]
  r <- lapply(levels(df_group$tmp),function(id){
    r1 <- mcf_cencored(subset(event_info, tmp == id),subset(life_censored, tmp == id))
    r1[[attr]] <- id
    r1
  })
  r <- do.call(rbind,r)
  r[[attr]] <- factor(r[[attr]])
  return(r)
}

mcf_cencored2D <- function(event_info,life_censored,units){
  names(event_info) <- c('svrid','age','util','duty')
  maxt1 <- round(max(c(event_info$age,life_censored$ageE))/units)*units
  mint1 <- round(min(c(event_info$age,life_censored$ageS))/units)*units
  maxt2 <- round(max(c(event_info$duty,life_censored$dutyE))/units)*units
  mint2 <- round(min(c(event_info$duty,life_censored$dutyS))/units)*units
  
  atrisk <- data.frame(expand.grid(seq(mint1,maxt1,units),seq(mint2,maxt2,units)))
  names(atrisk) <- c('age','duty')
  atrisk$count <- 0
  a <- dcast(age~duty,data = atrisk,value.var = 'count')
}










# mcf_cencored2D <- function(event_info,life_censored,factor_reduce){
#   life_censored$ageS <- round(life_censored$ageS/factor_reduce)
#   life_censored$ageE <- round(life_censored$ageE/factor_reduce)
#   life_censored$dutyS <- round(life_censored$dutyS/factor_reduce)
#   life_censored$dutyE <- round(life_censored$dutyE/factor_reduce)
#   event_info$life <- round(event_info$life/factor_reduce)
#   event_info$duty <- round(event_info$duty/factor_reduce)
# 
#   # life_censored1 <- life_censored;
#   # life_censored <- subset(life_censored1,1==1);life_censored <- smp_df(life_censored,10)
# 
#   uni_age <- sort(unique(c(event_info$life,life_censored$ageS,life_censored$ageE)))
#   uni_duty <- sort(unique(c(event_info$duty,life_censored$dutyS,life_censored$dutyE)))
#   # uni_age <- sort(unique(c(life_censored$ageS,life_censored$ageE)))
#   # uni_duty <- sort(unique(c(life_censored$dutyS,life_censored$dutyE)))
#   atrisk <- data.frame(expand.grid(uni_age,uni_duty))
#   names(atrisk) <- c('age','duty')
#   atrisk$count <- 0
#   # a1 <- subset(atrisk,age < duty & count > 0)
#   # atrisk$count[atrisk$age == 88 & atrisk$duty == 96]
# 
#   tableBL <- melt_table(life_censored$ageS,life_censored$dutyS);names(tableBL) <- c('age','duty','count') #table of point on the left buttom
#   tableTR <- melt_table(life_censored$ageE,life_censored$dutyE);names(tableTR) <- c('age','duty','count') #table of point on the right top
#   tableTL <- melt_table(life_censored$ageS,life_censored$dutyE);names(tableTL) <- c('age','duty','count') #table of point on the left top
#   tableBR <- melt_table(life_censored$ageE,life_censored$dutyS);names(tableBR) <- c('age','duty','count') #table of point on the right buttom
# 
#   # For dutyS != dutyE
#   # add count of tableBLvalue to atrisk_age from idx to the end indicating onshelf
#   for(i in seq_len(nrow(tableBL))){
#     atrisk$count[atrisk$age >= tableBL$age[i] & atrisk$duty >= tableBL$duty[i]] <-
#       atrisk$count[atrisk$age >= tableBL$age[i] & atrisk$duty >= tableBL$duty[i]] + tableBL$count[i]
#   }
# 
#   # add count of tableTR$value to atrisk_age from idx to the end
#   for(i in seq_len(nrow(tableTR))){
#     atrisk$count[atrisk$age >= tableTR$age[i] & atrisk$duty >= tableTR$duty[i]] <-
#       atrisk$count[atrisk$age >= tableTR$age[i] & atrisk$duty >= tableTR$duty[i]] + tableTR$count[i]
#   }
# 
#   # minus count of tableTL$count to atrisk from idx to the topright
#   for(i in seq_len(nrow(tableTL))){
#     atrisk$count[atrisk$age >= tableTL$age[i] & atrisk$duty >= tableTL$duty[i]] <-
#       atrisk$count[atrisk$age >= tableTL$age[i] & atrisk$duty >= tableTL$duty[i]] - tableTL$count[i]
#   }
# 
#   # minus count of tableBR$count to atrisk from idx to the topright
#   for(i in seq_len(nrow(tableBR))){
#     atrisk$count[atrisk$age >= tableBR$age[i] & atrisk$duty >= tableBR$duty[i]] <-
#       atrisk$count[atrisk$age >= tableBR$age[i] & atrisk$duty >= tableBR$duty[i]] - tableBR$count[i]
#   }
# 
#   # For dutyS == dutyE
#   life_censored_equaled_duty <- subset(life_censored,dutyS == dutyE)
#   uni_DT <- unique(life_censored_equaled_duty$dutyE)
#   for(i in seq_len(length(uni_DT))){
#     lced_subset <- subset(life_censored_equaled_duty,dutyS == uni_DT[i])
#     tableS <- melt(table(lced_subset$ageS))
#     tableE <- melt(table(lced_subset$ageE))
#     for(j in seq_len(nrow(tableS))){
#       atrisk$count[atrisk$age >= tableS$Var1[j] & atrisk$duty == uni_DT[i]] <-
#         atrisk$count[atrisk$age >= tableS$Var1[j] & atrisk$duty == uni_DT[i]] + tableS$value[j]
#     }
#     for(j in seq_len(nrow(tableE))){
#       atrisk$count[atrisk$age >= tableE$Var1[j] & atrisk$duty == uni_DT[i]] <-
#         atrisk$count[atrisk$age >= tableE$Var1[j] & atrisk$duty == uni_DT[i]] - tableE$value[j]
#     }
#   }
#   atrisk <- subset(atrisk,count > 0)
#   return(atrisk)
# 
#   tableF <- melt_table(event_info$life,event_info$duty);names(tableF) <- c('age','duty','countF')
#   mcf <- merge(atrisk,tableF,by = c('age','duty'),all.x = T)
#   mcf$countF[is.na(mcf$countF)] <- 0
# 
#   # fails_age <- melt(table(event_info$life))
#   # names(fails_age) <- c('age','countF')
#   #
#   # mcf_age <- merge(atrisk_age,fails_age,by = 'age',all = T)
#   # mcf_age$countF[is.na(mcf_age$countF)] <- 0
#   # mcf_age$rate_day <- mcf_age$countF/mcf_age$count
#   # mcf_age$mcf <- cumsum(mcf_age$rate_day)
#   # return(mcf_age)
# }