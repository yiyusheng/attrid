# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require(ggplot2)

#@@@ Function @@@#
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'sc16F1Func.R'))

#####################################################################################################
# S1. plot correlation between disk age and AFR
Qu <- 1 # cut by each quauters
flistFile <- 'dataPrepareAFR13.Rda'
# flistFile <- 'dataPrepareAFR1406_1407.Rda'
load(file.path(dir_data,flistFile))
tmp.f$fsTimeQu <- cut3mon(tmp.f$failShiptime)
tmp.cmdb$shTimeQu <- cut3mon(tmp.cmdb$shiptimeToRight)
tmp.f$shTimeQu <- tmp.cmdb$shTimeQu[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]
tmp.cmdb$shTimeQu <- tmp.cmdb$shTimeQu[match(tmp.cmdb$svr_asset_id,tmp.cmdb$svr_asset_id)]

if (Qu == 1){
  cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTimeQu','shTimeQu',1,dev = 'C')
  cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTimeQu','shTimeQu',12,dev = 'TS')
  cm1$AFR[cm1$item == 5] <- 4.13443

}else{
  cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',1,dev = 'C')
  cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',12,dev = 'TS')
}
cm <- rbind(cm1,cm2)
cm <- factorX(subset(cm,!is.na(AFR)))
cm <- classExchg(cm)
cm <- subset(cm,item < 6)
p <- AFR_plot(cm,'fig1')
# S2. plot correlation between disk age and annual failure rate of pretented failure. 
# load(file.path(dir_dataSource,'flist(uwork[2012-2014]).Rda'))
# fTypeNeed <- c('硬盘故障（有冗余）','硬盘故障（有冗余，槽位未知）',
#             '硬盘故障（无冗余）','硬盘故障（无冗余，在线换盘）',
#             '硬盘即将故障（有冗余）','操作系统硬盘故障（无冗余）')
# 
# # NOTICE TIME
# flistUwork <- subset(data.flist,class == -1 & ftype %in% fTypeNeed &
#                      svr_id %in% tmp.cmdb$svr_asset_id & f_time > as.POSIXct('2014-01-01')) 
# 
# flistUwork <- mchAttr(flistUwork,tmp.cmdb,'svr_id','svr_asset_id',
#                       c('use_time','shTimeQu','dClass'))
# 
# cm1 <- AFR_attr_notime(flistUwork,tmp.cmdb,'shTimeQu','shTimeQu',1,dev = 'C')
# cm2 <- AFR_attr_notime(flistUwork,tmp.cmdb,'shTimeQu','shTimeQu',12,dev = 'TS')
# cm <- rbind(cm1,cm2)
# cm <- factorX(subset(cm,!is.na(AFR)))
# cm <- classExchg(cm)
# p <- AFR_plot(cm,'fig1')
####################################
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

# plotCol <- c('item','class','AFR')
# naFill <- cbind(expand.grid(item = levels(factor(cm$item)),class = levels(factor(cm$class))),AFR = 0)
# cm <- rbind(cm[,plotCol],naFill)
# cm$item <- as.numeric(cm$item)