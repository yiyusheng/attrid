# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require(ggplot2)
require(plyr)

#@@@ Function @@@#
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'sc16F1Func.R'))

#####################################################################################################
# S1. plot correlation between disk age and AFR
Qu <- 1 # cut by each quauters
flistFile <- 'dataPrepareAFR13.Rda'
# flistFile <- 'dataPrepareAFR1406_1407.Rda'
load(file.path(dir_data,flistFile))

tmp.f <- factorX(tmp.f)
tmp.cmdb <- factorX(tmp.cmdb)

tmp.f$fsTimeQu <- cut3mon(tmp.f$failShiptime,4)
tmp.f$diskNum <- tmp.cmdb$diskNum[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]

tmp.cmdb$shTimeQu <- cut3mon(tmp.cmdb$shiptimeToRight,4)
tmp.cmdb$shTimeQuGuassian <- cut3mon(tmp.cmdb$shiptimeToRight,4.25)
tmp.f$shTimeQu <- tmp.cmdb$shTimeQu[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]
tmp.f$shTimeQuGuassian <- tmp.cmdb$shTimeQuGuassian[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]

# compute the failure rate in a overlayed model
at <- 'shTimeQu'
cm1A <- AFR_attr_notime(tmp.f,tmp.cmdb,at,at,1,dev = 'C')
cm2A <- AFR_attr_notime(tmp.f,tmp.cmdb,at,at,12,dev = 'TS')
cm1A$AFR[cm1A$item == 5] <- 4.13443
cmA <- rbind(cm1A,cm2A)
cmA <- factorX(subset(cmA,!is.na(AFR)))
cmA <- classExchg(cmA)
cmA <- subset(cmA,item < 6)

# compute the failure rate of warranty effect
at <- 'shTimeQuGuassian'

cm1B <- AFR_attr_notime(tmp.f,tmp.cmdb,at,at,1,dev = 'C')
cm1B$AFR[cm1B$item == 5] <- 4.13443
x1 <- cm1B$item[1:12];y1 <- cm1B$AFR[1:12]
cm1B$AFRpredict <- predict(lm(y1~x1),data.frame(x1 = cm1B$item))
cm1B$AFRdiff <- cm1B$AFR - cm1B$AFRpredict

cm2B <- AFR_attr_notime(tmp.f,tmp.cmdb,at,at,12,dev = 'TS')
x2 <- cm2B$item[1:12];y2 <- cm2B$AFR[1:12]
cm2B$AFRpredict <- predict(lm(y2~x2),data.frame(x2 = cm2B$item))
cm2B$AFRdiff <- cm2B$AFR - cm2B$AFRpredict

cmB <- rbind(cm1B,cm2B)
cmB <- factorX(subset(cmB,!is.na(AFR)))
cmB <- classExchg(cmB)
cmB <- subset(cmB,item >=3 & item <= 4)

# plot
cmA$item[cmA$item == 5] <- 4.25
p1 <- AFR_plot(cmA,'fig1')
p2 <- AFR_plot_warranty(cmB,'fig1Warranty')

# fit a bathtub curve based on cm
start <- 2.75
smp <- subset(cmA,item >=start & item <= 3.25 & class == 'Nserv',c('item','AFR'))
x <- smp$item;y <- smp$AFR
fit <- lm(y~poly(x,2,raw = T))
x1 <- seq(start,3.25,0.05)
y1 <- predict(fit,data.frame(x = x1))
x2 <- x1 - start
y2 <- rev(y1)
fitCurve <- data.frame(item = c(x2,x1),AFR = c(y2,y1),class = 'line')
# fitCurve <- subset(fitCurve,item %in% cmA$item)
ggplot(fitCurve,aes(x = item,y = AFR)) + geom_line()
p1 + geom_line(data = fitCurve,aes(x = item,y = AFR,group = 1)) 
  stat_smooth(data = fitCurve,aes(x = item,y = AFR),se = F,method = 'lm',formula = y~plot(x,2))
###################################
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

# virtDC <- virt_disk(tmp.f,tmp.cmdb)
# virtDC$survTime <- cut3mon(as.numeric(difftime(virtDC$f_time,virtDC$use_time,tz = 'UTC',units = 'days'))/365)
# tmp.df <- subset(virtDC,status == 'failed')
# tmp.dcmdb <- subset(virtDC,status == 'working')

# cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',1,dev = 'C')
# cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'shTime','shTime',12,dev = 'TS')