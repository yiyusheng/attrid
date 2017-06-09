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

mcf_cencored <- function(event_info,life_censored,days){
  event_info$life <- ceiling(event_info$life/days)
  life_censored$lifeS <- ceiling(life_censored$lifeS/days)
  life_censored$lifeE <- ceiling(life_censored$lifeE/days)
  
  uni_life <- sort(unique(c(event_info$life,life_censored$lifeS,life_censored$lifeE)))
  tableS <- melt(table(life_censored$lifeS))
  tableE <- melt(table(life_censored$lifeE))
  atrisk_life <- data.frame(life = uni_life,count = 0)
  
  # add count of tableSvalue to atrisk_life from idx to the end indicating onshelf
  for(i in seq_len(nrow(tableS))){
    idx <- which(atrisk_life$life == tableS$Var1[i])
    atrisk_life$count[idx:nrow(atrisk_life)] <- atrisk_life$count[idx:nrow(atrisk_life)] + tableS$value[i]
  }
  
  # minus count of tableE$value to atrisk_life from idx to the end indicating offshelf
  for(i in seq_len(nrow(tableE))){
    idx <- which(atrisk_life$life == tableE$Var1[i]) + 1
    if(idx > nrow(atrisk_life)) next
    atrisk_life$count[idx:nrow(atrisk_life)] <- atrisk_life$count[idx:nrow(atrisk_life)] - tableE$value[i]
  }
  
  fails_life <- melt(table(event_info$life))
  names(fails_life) <- c('life','countF')
  
  mcf_life <- merge(atrisk_life,fails_life,by = 'life',all = T)
  mcf_life$countF[is.na(mcf_life$countF)] <- 0
  mcf_life$rate_day <- mcf_life$countF/mcf_life$count
  mcf_life$mcf <- cumsum(mcf_life$rate_day)
  
  return(mcf_life)
}

mcf_group <- function(event_info,life_censored,df_group,attr,days = 1){
  df_group$tmp <- df_group[[attr]]
  event_info$tmp <- df_group$tmp[match(event_info$svrid,df_group$svrid)]
  life_censored$tmp <- df_group$tmp[match(life_censored$svrid,df_group$svrid)]
  r <- lapply(levels(df_group$tmp),function(id){
    r1 <- mcf_cencored(subset(event_info, tmp == id),subset(life_censored, tmp == id),days)
    r1[[attr]] <- id
    r1
  })
  r <- do.call(rbind,r)
  r[[attr]] <- factor(r[[attr]])
  return(r)
}

# mcf_cencored2D <- function(event_info,life_censored,units){
#   names(event_info) <- c('svrid','life','util','duty')
#   maxt1 <- round(max(c(event_info$life,life_censored$lifeE))/units)*units
#   mint1 <- round(min(c(event_info$life,life_censored$lifeS))/units)*units
#   maxt2 <- round(max(c(event_info$duty,life_censored$dutyE))/units)*units
#   mint2 <- round(min(c(event_info$duty,life_censored$dutyS))/units)*units
#   
#   atrisk <- data.frame(expand.grid(seq(mint1,maxt1,units),seq(mint2,maxt2,units)))
#   names(atrisk) <- c('life','duty')
#   atrisk$count <- 0
#   a <- dcast(life~duty,data = atrisk,value.var = 'count')
# }
# 
# get_point <- function(x1,y1,x2,y2){
#   slope <- (y2-y1)/(x2-x1)
#   intercept <- (x1*y2-x2*y1)/(x1-x2)
#   x1 <- floor(x1)
#   x2 <- ceiling(x2)
#   seqx <- unique(round(x1:x2))
#   seqy <- round(slope*seqx + intercept)
#   return(list(seqx,seqy))
# }

# mcf_cencored2D <- function(event_info,life_censored,factor_reduce){
#   life_censored$lifeS <- round(life_censored$lifeS/factor_reduce)
#   life_censored$lifeE <- round(life_censored$lifeE/factor_reduce)
#   life_censored$dutyS <- round(life_censored$dutyS/factor_reduce)
#   life_censored$dutyE <- round(life_censored$dutyE/factor_reduce)
#   event_info$life <- round(event_info$life/factor_reduce)
#   event_info$duty <- round(event_info$duty/factor_reduce)
# 
#   # life_censored1 <- life_censored;
#   # life_censored <- subset(life_censored1,1==1);life_censored <- smp_df(life_censored,10)
# 
#   uni_life <- sort(unique(c(event_info$life,life_censored$lifeS,life_censored$lifeE)))
#   uni_duty <- sort(unique(c(event_info$duty,life_censored$dutyS,life_censored$dutyE)))
#   # uni_life <- sort(unique(c(life_censored$lifeS,life_censored$lifeE)))
#   # uni_duty <- sort(unique(c(life_censored$dutyS,life_censored$dutyE)))
#   atrisk <- data.frame(expand.grid(uni_life,uni_duty))
#   names(atrisk) <- c('life','duty')
#   atrisk$count <- 0
#   # a1 <- subset(atrisk,life < duty & count > 0)
#   # atrisk$count[atrisk$life == 88 & atrisk$duty == 96]
# 
#   tableBL <- melt_table(life_censored$lifeS,life_censored$dutyS);names(tableBL) <- c('life','duty','count') #table of point on the left buttom
#   tableTR <- melt_table(life_censored$lifeE,life_censored$dutyE);names(tableTR) <- c('life','duty','count') #table of point on the right top
#   tableTL <- melt_table(life_censored$lifeS,life_censored$dutyE);names(tableTL) <- c('life','duty','count') #table of point on the left top
#   tableBR <- melt_table(life_censored$lifeE,life_censored$dutyS);names(tableBR) <- c('life','duty','count') #table of point on the right buttom
# 
#   # For dutyS != dutyE
#   # add count of tableBLvalue to atrisk_life from idx to the end indicating onshelf
#   for(i in seq_len(nrow(tableBL))){
#     atrisk$count[atrisk$life >= tableBL$life[i] & atrisk$duty >= tableBL$duty[i]] <-
#       atrisk$count[atrisk$life >= tableBL$life[i] & atrisk$duty >= tableBL$duty[i]] + tableBL$count[i]
#   }
# 
#   # add count of tableTR$value to atrisk_life from idx to the end
#   for(i in seq_len(nrow(tableTR))){
#     atrisk$count[atrisk$life >= tableTR$life[i] & atrisk$duty >= tableTR$duty[i]] <-
#       atrisk$count[atrisk$life >= tableTR$life[i] & atrisk$duty >= tableTR$duty[i]] + tableTR$count[i]
#   }
# 
#   # minus count of tableTL$count to atrisk from idx to the topright
#   for(i in seq_len(nrow(tableTL))){
#     atrisk$count[atrisk$life >= tableTL$life[i] & atrisk$duty >= tableTL$duty[i]] <-
#       atrisk$count[atrisk$life >= tableTL$life[i] & atrisk$duty >= tableTL$duty[i]] - tableTL$count[i]
#   }
# 
#   # minus count of tableBR$count to atrisk from idx to the topright
#   for(i in seq_len(nrow(tableBR))){
#     atrisk$count[atrisk$life >= tableBR$life[i] & atrisk$duty >= tableBR$duty[i]] <-
#       atrisk$count[atrisk$life >= tableBR$life[i] & atrisk$duty >= tableBR$duty[i]] - tableBR$count[i]
#   }
# 
#   # For dutyS == dutyE
#   life_censored_equaled_duty <- subset(life_censored,dutyS == dutyE)
#   uni_DT <- unique(life_censored_equaled_duty$dutyE)
#   for(i in seq_len(length(uni_DT))){
#     lced_subset <- subset(life_censored_equaled_duty,dutyS == uni_DT[i])
#     tableS <- melt(table(lced_subset$lifeS))
#     tableE <- melt(table(lced_subset$lifeE))
#     for(j in seq_len(nrow(tableS))){
#       atrisk$count[atrisk$life >= tableS$Var1[j] & atrisk$duty == uni_DT[i]] <-
#         atrisk$count[atrisk$life >= tableS$Var1[j] & atrisk$duty == uni_DT[i]] + tableS$value[j]
#     }
#     for(j in seq_len(nrow(tableE))){
#       atrisk$count[atrisk$life >= tableE$Var1[j] & atrisk$duty == uni_DT[i]] <-
#         atrisk$count[atrisk$life >= tableE$Var1[j] & atrisk$duty == uni_DT[i]] - tableE$value[j]
#     }
#   }
#   atrisk <- subset(atrisk,count > 0)
#   return(atrisk)
# 
#   tableF <- melt_table(event_info$life,event_info$duty);names(tableF) <- c('life','duty','countF')
#   mcf <- merge(atrisk,tableF,by = c('life','duty'),all.x = T)
#   mcf$countF[is.na(mcf$countF)] <- 0
# 
#   # fails_life <- melt(table(event_info$life))
#   # names(fails_life) <- c('life','countF')
#   #
#   # mcf_life <- merge(atrisk_life,fails_life,by = 'life',all = T)
#   # mcf_life$countF[is.na(mcf_life$countF)] <- 0
#   # mcf_life$rate_day <- mcf_life$countF/mcf_life$count
#   # mcf_life$mcf <- cumsum(mcf_life$rate_day)
#   # return(mcf_life)
# }