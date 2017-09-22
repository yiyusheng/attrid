#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: extreme_duty_cycle.R
#
# Description: Correlate the extreme duty cycle and the failure rate
# [The Amount of High Duty Cycle]
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-08-21 11:31:02
#
# Last   modified: 2017-08-21 11:31:04
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/Paper');source('~/rhead');
source('dir_func.R')

load(file.path(dir_data,'uniform_data.Rda'))
load(file.path(dir_data,'count_duty_cycle_value.Rda'))
cnt <- duty_cycle_value_count;cnt$count <- rowSums(cnt[,-1])


# S1. parameters and variables  ------------------------------------------------------------------------
col_quanall <- paste('X',0:100,sep='')
cut_point <- seq(0,100,1);  itv <- 1
# cut_point <- seq(25,80,5);  itv <- 5

# S2. get failure rate and pearson's correlation coefficient ------------------------------------
# Wrong code and see ../Bandwidth/bandwidth.R
gen_result_fraction <- function(i){
  cat(sprintf('[%s]\t task %i SATRT!!!\n',date(),i))
  quanList <- get_fraction_on_count(cnt,cut_point[i])
  quanList$fraction <- ceiling(quanList$fraction/itv)*itv
  
  list[data_fr,p_fr,p_count,p_countF,object_data] <- gen_fr(quanList,attr='fraction',prt=F,countLimit = 10)
  data_fr$class <- cut_point[i]
  corr <- with(data_fr,cor(fraction,AFR))
  
  list(data_fr,p_fr,p_count,corr)
}

idx <- seq_len(length(cut_point))
system.time(r <- foreachX(idx,'gen_result_fraction',frac_cores = 0.9,outname = NULL))
data_frList <- lapply(r,'[[',1)
p_frList <- lapply(r,'[[',2)
p_count <- lapply(r,'[[',3)
corr <- sapply(r,'[[',4)
corr_frac_fr <- data.frame(thred=cut_point,corr=corr)
save(data_frList,p_frList,p_count,corr_frac_fr,file=file.path(dir_data,paste(sprintf('fraction_great_duty_cycle_%i.Rda',itv))))

# S3. plot result ------------------------------------
load(file.path(dir_data,paste(sprintf('fraction_great_duty_cycle_%i.Rda',itv))))

plist1 <- list();plist2 <- list()
for (i in seq_len(length(cut_point))){
  plist1[[i]] <- p_frList[[i]] + xlab(sprintf('Fraction(duty cycle >= %d)',cut_point[i]))+ylim(c(0,40))
  plist2[[i]] <- p_count[[i]]+xlab(sprintf('Fraction(duty cycle >= %d)',cut_point[i]))+coord_cartesian(ylim=c(0,10))+
    annotate("text", x=quantile(data_frList[[i]]$fraction,0.5), y=9.5, size = 8,
             label= sprintf("(%.2f%%,%.2f%%)",data_frList[[i]]$percentage[1],data_frList[[i]]$percentage[2]))
    
}
cut_point_need <- seq(91,99,1)
plist1 <- plist1[which(cut_point %in% cut_point_need)]
plist2 <- plist2[which(cut_point %in% cut_point_need)]
pcorr <- ggplot(corr_frac_fr,aes(x=thred,y=corr))+geom_line()+geom_point(size=5)+ xlab('Duty Cycle Threshold') + ylab('Correlation')+
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),legend.position = 'none')

multiplot(plotlist = plist1,layout = matrix(1:length(plist1),nrow = 3,byrow=T))
multiplot(plotlist = plist2,layout = matrix(1:length(plist2),nrow = 3,byrow=T))
print(pcorr)

# S4. fraction vs. amount

get_fr_fraction_amount <- function(i){
  cat(sprintf('[%s]\t task %i SATRT!!!\n',date(),i))
  quanList <- get_fraction_on_count(cnt,cut_point[i])
  quanList$fraction <- ceiling(quanList$fraction/itv)*itv
  list[object_data,fail_data,fr] <- gen_data(quanList,attr=c('fraction','threshold'))
  fr
}
itv <- 2;cut_point <- seq(0,100,itv);  
idx <- seq_len(length(cut_point))
system.time(r <- foreachX(idx,'get_fr_fraction_amount',frac_cores = 0.9,outname = NULL))
r <- do.call(rbind,r)
p <- ggplot(subset(r,count>50 & AFR<50))+geom_raster(aes(x=fraction,y=threshold,fill=AFR))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
        legend.text = element_text(size = 12),legend.title = element_text(size = 24),legend.position = 'bottom')
print(p)
