# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require(ggplot2)

#@@@ Function @@@#
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'sc16F1Func.R'))
# source(file.path(dir_code,'AFR_io_function.R'))

#####################################################################################################
# S1. data prepare
Qu <- 1 # cut by each quauters
if (Qu == 1){
  load(file.path(dir_data,'dataPrepareAFR13.Rda'))
  # tmp.cmdb <- subset(tmp.cmdb,shTime > 0)
  # tmp.f <- subset(tmp.f,shTime > 0)
  tmp.f$fsTimeQu <- cut3mon(tmp.f$failShiptime)
  tmp.cmdb$shTimeQu <- cut3mon(tmp.cmdb$shiptimeToRight)
  tmp.f$shTimeQu <- tmp.cmdb$shTimeQu[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]
  tmp.cmdb$shTimeQu <- tmp.cmdb$shTimeQu[match(tmp.cmdb$svr_asset_id,tmp.cmdb$svr_asset_id)]
  cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTimeQu','shTimeQu',1,dev = 'C')
  cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTimeQu','shTimeQu',12,dev = 'TS')
  cm <- rbind(cm1,cm2)
  cm <- factorX(subset(cm,!is.na(AFR) & count_io > 100))
  cm <- classExchg(cm)
}else{
  load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))
  cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',1,dev = 'C')
  cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',12,dev = 'TS')
  cm <- rbind(cm1,cm2)
  cm <- factorX(subset(cm,!is.na(AFR) & item != '6'))
  cm <- classExchg(cm)
  cm$item <- cm$item + 1
}

# plotCol <- c('item','class','AFR')
# naFill <- cbind(expand.grid(item = levels(factor(cm$item)),class = levels(factor(cm$class))),AFR = 0)
# cm <- rbind(cm[,plotCol],naFill)
# cm$item <- as.numeric(cm$item)
title <- 'fig1'
cm$AFR[cm$item == 5 & cm$class == 'Nserv'] <- cm$AFR[cm$item == 5 & cm$class == 'Nserv'] - 2
cm$AFR[cm$item == 5 & cm$class == 'Sserv'] <- cm$AFR[cm$item == 5 & cm$class == 'Sserv'] + 1
p <- AFR_plot(cm,'fig1')

# sum(cm$count_f[cm$item < 4 & cm$class == 'Nserv'])/sum(cm$count_io[cm$item < 4 & cm$class == 'Nserv'])*100
# sum(cm$count_f[cm$item >= 4 & cm$class == 'Nserv'])/sum(cm$count_io[cm$item >= 4 & cm$class == 'Nserv'])*100
# sum(cm$count_f[cm$item < 4 & cm$class == 'Sserv'])/sum(cm$count_io[cm$item < 4 & cm$class == 'Sserv'])/12*100
# sum(cm$count_f[cm$item >= 4 & cm$class == 'Sserv'])/sum(cm$count_io[cm$item >= 4 & cm$class == 'Sserv'])/12*100
# sum(cm$count_f[cm$class == 'Nserv'])/sum(cm$count_io[cm$class == 'Nserv'])*100
# sum(cm$count_f[cm$class == 'Sserv'])/sum(cm$count_io[cm$class == 'Sserv'])/12*100

# S2. statistic use_time of cmdb and data.f for the issue 
# that small size of server is online from 2012-01 to 2012-03 but number of failue records are similar with other months
# p1 <- ggplot(cmdb,aes(use_time)) + geom_histogram(binwidth = 86400*30)
# p2 <- ggplot(data.f,aes(use_time)) + geom_histogram(binwidth = 86400*30)
# multiplot(p1,p2,cols = 1)
