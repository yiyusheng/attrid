#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta_cut_als.R
#
# Description: Analysize of resutl from sta_cut
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-27 11:14:09
#
# Last   modified: 2017-03-27 11:14:11
#
#
#

rm(list = ls());source('~/rhead')
load(file.path(dir_data,'sta_cut.Rda'))
load(file.path(dir_data,'sta_dcastClear.Rda'))

###### ANALYSIS ######
ss_util <- lapplyX(lapply(r,'[[',2),'[[',1)
ss_rps <- lapplyX(lapply(r,'[[',2),'[[',2)
ss_iopsr <- lapplyX(lapply(r,'[[',2),'[[',3)
ss_wps <- lapplyX(lapply(r,'[[',2),'[[',4)
ss_iopsw <- lapplyX(lapply(r,'[[',2),'[[',5)
ss_sizer <- lapplyX(lapply(r,'[[',2),'[[',6)
ss_sizew <- lapplyX(lapply(r,'[[',2),'[[',7)

###### S1.idle of write ######
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

# for count [useful plot]
wps_count <- matrix(c(colSums(ss_wps[,grepl('X\\d+',names(ss_wps))][-1]),0),ncol = 2,byrow = T)
wps_count <- data.frame(id = names(ss_wps)[grepl('X\\d+',names(ss_wps))][-1][seq(1,18,2)],
                        count = rowSums(wps_count))
wps_count <- rbind(wps_count,data.frame(id = 'X0',count = sum(ss_wps$X0)))
wps_count$count <- array_rate(wps_count$count)
wps_count$id <- sort_level(factor(gsub('X','',wps_count$id)))
ggplot(wps_count,aes(x = id,y = count)) + geom_bar(stat = 'identity')

# for rate [used in xmind]
ss_wps_rate <- ss_wps
ss_wps_rate[,col_value] <- roundX(ss_wps_rate[,col_value]/ss_wps_rate$count)
wps_busy <- subset(ss_wps_rate,!(max %in% c('X16','X32')))

wps_idle <- subset(ss_wps_rate,max %in% c('X16','X32'))
wps_idle$frac_idle <- wps_idle$X16 + wps_idle$X32
summary(wps_idle$X16 + wps_idle$X32)
with(wps_idle,summary(X32 + X16 + X4 + X8 + X64 + X128))

wps_idle <- subsetX(wps_idle, frac_idle >= median(frac_idle))
wps_working <- subsetX(wps_idle, frac_idle < median(frac_idle))

idle_range <- c(8*86400*365/1e6,64*86400*365/1e6)
save(wps_idle, wps_busy, wps_working, file = file.path(dir_data,'sca_wps.Rda'))

# the rest 19.59% of wps_idle
wps_idle_count <- subset(ss_wps_rate,svrid %in% wps_idle$svrid)
wps_idle_count$frac_idle <- with(wps_idle_count, X32 + X16)
wps_idle_count$frac_idle_expand <- with(wps_idle_count, X32 + X16 + X4 + X8 + X64 + X128)
ggplot(wps_idle_count) + stat_ecdf(aes(frac_idle),color = 'red') + stat_ecdf(aes(frac_idle_expand),color = 'blue')
nrow(subset(wps_idle_count,frac_idle < 0.9 & frac_idle_expand > 0.98))/nrow(subset(wps_idle_count,frac_idle < 0.9))


wps_low_idle <- wps_idle[rowSums(wps_idle[,12:19]) > 0.18,]
wps_idle_count$fn <- r_sta_svrid$fn[match(wps_idle_count$svrid,r_sta_svrid$svrid)]
wps_idle_sum <- array_rate(colSums(wps_idle_count[,grepl('X\\d+',names(wps_idle_count))]))

# idle wps + rps distribution[finding in xmind]
load(file.path(dir_data,'sca_wps.Rda'))
rps_idlewps <- subset(ss_rps,svrid %in% wps_busy$svrid)
rps_idlewps <- rate_data(rps_idlewps)
ggplot(rps_idlewps,aes(X0)) + stat_ecdf()

summary(rps_idlewps$X0+rps_idlewps$X1)  #xmind
table_rps0 <- melt(table(round(rps_idlewps$X0,digits = 1)))
ggplot(table_rps0,aes(Var1,value)) + geom_bar(stat = 'identity')

# size of request when the wps is idle
idle_iopsw <- subset(ss_iopsw,svrid %in% wps_idle$svrid)
array_rate(colSums(idle_iopsw[,col_value]))
idle_iopsw <- rate_data(idle_iopsw)

###### S2.idle of read ######
col_value <- names(ss_rps)[grepl('X\\d+',names(ss_rps))]
ss_rps$count <- apply(ss_rps[,col_value],1,sum)
ss_rps$max <- apply(ss_rps[,col_value],1,function(x)names(ss_rps[,col_value])[which.max(x)])
table_rps <- melt(table(ss_rps$max))
ss_rps$bts_wtn <- as.numeric((as.matrix(ss_rps[,col_value]) %*% cutList$cut_rps[-length(cutList$cut_rps)])*300/1e6)
ss_rps$fn <- factor(r_sta_svrid$fn[match(ss_rps$svrid,r_sta_svrid$svrid)])
ss_rps$fclass <- gsub('\\d.*','',ss_rps$fn)
ss_rps$max <- factor(ss_rps$max)

# for count [useful plot]
rps_count <- matrix(c(colSums(ss_rps[,grepl('X\\d+',names(ss_rps))][-1]),0),ncol = 2,byrow = T)
rps_count <- data.frame(id = names(ss_rps)[grepl('X\\d+',names(ss_rps))][-1][seq(1,18,2)],
                        count = rowSums(rps_count))
rps_count <- rbind(rps_count,data.frame(id = 'X0',count = sum(ss_rps$X0)))
rps_count$count <- array_rate(rps_count$count)
rps_count$id <- sort_level(factor(gsub('X','',rps_count$id)))
ggplot(rps_count,aes(x = id,y = count)) + geom_bar(stat = 'identity')

###### S3.idle_rps and idle_wps ######
fraction_xps <- function(ss_wps,wri,bound_low){
  # distribution of idle rps and idle xps [finding in xmind]
  bound_up <- 1 - bound_low
  sw_idle <- rate_data(ss_wps)[,c('svrid',paste('X',2^(3:16),sep=''))]
  sr_idle <- rate_data(ss_rps)[,c('svrid','X0')]
  x <- ifelse(length(wri) == 1,sw_idle$idlew <- sw_idle[,wri],sw_idle$idlew <- rowSums(sw_idle[,wri]))
  sr_idle$idler <- sr_idle$X0
  ss_xps_idle <- merge(sw_idle[,c('svrid','idlew')],sr_idle[,c('svrid','idler')],by = 'svrid')
  names(ss_xps_idle) <- c('svrid','idle_wps','idle_rps')
  
  
  # p1.fraction of idle reading and fraction of idle writing
  table_sxi <- melt(table(round(ss_xps_idle$idle_rps,digits = 2),round(ss_xps_idle$idle_wps,digits = 2)))
  table_sxi$value[table_sxi$value == 0] <- 0.25
  table_sxi_limited <- subset(table_sxi,Var1 <= bound_up & Var1 >= bound_low & Var2 <= bound_up & Var2 >= bound_low)
  p1 <- ggplot(table_sxi_limited,aes(x = Var1,y = Var2,fill = log2(value))) + geom_raster() #useful
  
  # p2.difference of fraction of idle reading and fraction of idle writing in each server.
  table_sxi_limited$diff <- round(table_sxi_limited$Var1 - table_sxi_limited$Var2,digits = 1)
  rwidle_similar <- list2df(tapply(table_sxi_limited$value,table_sxi_limited$diff,sum),n = c('count','diff'))
  rwidle_similar$count1 <- rwidle_similar$count
  rwidle_similar$count <- array_rate(rwidle_similar$count)
  p2 <- ggplot(rwidle_similar,aes(x = as.numeric(diff),y = count1)) + geom_bar(stat = 'identity')                     
  
  # p3. wps distribution when diff is small.
  rate <- sum(rwidle_similar$count[abs(as.numeric(rwidle_similar$diff)) <= 0.1])  #xmind
  # similar_xps <- subsetX(ss_xps_idle,abs(idle_wps - idle_rps) <= 0.1 & idle_wps < bound_up & idle_wps >= bound_low & idle_rps < bound_up & idle_rps > bound_low)
  # p3 <- ggplot(similar_xps,aes(x = idle_wps)) + geom_histogram(binwidth = 0.1)
  p3 <- 0
  
  rwidle_similar$class <- paste(names(sw_idle)[wri],collapse = '_')
  return(list(p1,p2,p3,rwidle_similar,rate))
}
wps_range_id <- list(2,3:4,5:6,7:8,9:10)
fx <- lapply(wps_range_id,function(x)fraction_xps(ss_wps,x,0))
p1 <- lapply(fx,'[[',1);p2 <- lapply(fx,'[[',2);rate_simi <- lapply(fx,'[[',5)
data_diff_frac <- lapplyX(fx,'[[',4)
ggplot(data_diff_frac,aes(x = as.numeric(diff),y = count,color = class)) + 
  geom_line() + geom_point(aes(shape = class)) + scale_color_brewer(palette='Spectral') +
  xlab('Difference') + ylab('Fraction') + guides(color = guide_legend(title = NULL),shape = guide_legend(title = NULL))
save(fx, file = file.path(dir_data,'sca_similar_xps.Rda'))

###### S4. find hot standby pair of servers #######
id <- c('198','199','365','366','501','503')
smp_rps <- subsetX(ss_rps,svrid %in% id)
smp_wps <- subsetX(ss_wps,svrid %in% id)
smp_util <- subsetX(ss_util,svrid %in% id)
smp_iopsr <- subsetX(ss_iopsr,svrid %in% id)
smp_iopsw <- subsetX(ss_iopsw,svrid %in% id)
smp_sizer <- subsetX(ss_sizer,svrid %in% id)
smp_sizew <- subsetX(ss_sizew,svrid %in% id)
dd <- subsetX(dt_dcast,svrid %in% id,c('svrid','time','rps','wps','util','iopsr','iopsw'))
ddd <- dd
ddd$time <- as.Date(ddd$time)
ddd1 <- aggregate(ddd[,attrNameAll],by = list(ddd$svrid,ddd$time),mean);names(ddd1)[1:2] <- c('svrid','time')
splitDD <- split(ddd1,ddd1$svrid)
dd1 <- merge(splitDD[[1]],splitDD[[2]],by = c('time'));dd1 <- remove_line_byvalue(dd1)
dd2 <- merge(splitDD[[3]],splitDD[[4]],by = c('time'));dd2 <- remove_line_byvalue(dd2)
dd3 <- merge(splitDD[[5]],splitDD[[6]],by = c('time'));dd3 <- remove_line_byvalue(dd3)
