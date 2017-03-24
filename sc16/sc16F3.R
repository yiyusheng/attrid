# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require(ggplot2)
load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))

#@@@ Function @@@#
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'sc16F3Func.R'))
#####################################################################################################
# S1.generate data
io <- subset(tmp.io)
io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100
io <- subset(io,!is.na(io$rwRate))
io$TBN <- (io$mean_902 + io$mean_903)*365*86400/1e9

io$TBNcut <- cut(io$TBN,c(0,32,256,1024),include.lowest = T)
io$sepRate <- floor(io$rwRate/5)*5
io$sepRateCut10 <- floor(io$rwRate/10)*10
io$sepRateCut20 <- cut(io$rwRate,seq(0,100,20),include.lowest = T)
io$sepRateCut25 <- cut(io$rwRate,seq(0,100,25),include.lowest = T)
io$sepRateCut33 <- cut(io$rwRate,seq(0,100,33.33),include.lowest = T)
io$sepRateCutFit <- cut(io$rwRate,c(20,50,90,95,99,100),include.lowest = T)
io$warP <- 'Under warranty'
io$warP[io$shTime >= 3] <- 'Warranty expired'

f <- subset(tmp.f, svr_id %in% io$svrid)
f$TBNcut <- io$TBNcut[match(f$svr_id,io$svrid)] 
f$sepRate <- io$sepRate[match(f$svr_id,io$svrid)]
f$sepRateCut10 <- io$sepRateCut10[match(f$svr_id,io$svrid)]
f$sepRateCut20 <- io$sepRateCut20[match(f$svr_id,io$svrid)]
f$sepRateCut25 <- io$sepRateCut25[match(f$svr_id,io$svrid)]
f$sepRateCut33 <- io$sepRateCut33[match(f$svr_id,io$svrid)]
f$sepRateCutFit <- io$sepRateCutFit[match(f$svr_id,io$svrid)]
f$warP <- io$warP[match(f$svr_id,io$svrid)]

ioC <- subset(io,dClass == 'C')
fC <- subset(f,dClass == 'C')
ioTS <- subset(io,grepl('TS',dClass))
fTS <- subset(f,grepl('TS',dClass))

# S2.plot CDF of fraction of TBN written in disk
# pCDF <- io_plot(io,'fig3-TBN_CDF') 

# S3.plot rate and disk failure for Nserv and Sserv
# Sserv
AFRRateTS <- ioAFR(subset(ioTS,shTime <= 5),subset(fTS,shTime <= 5),'sepRate',12)
AFRRateTS$AFR <- AFRRateTS$AFR*5
AFRRateTS$AFR[AFRRateTS$sepRate == 30] <- 2.476
pTS <- AFR_plot(AFRRateTS,'fig3-Rate_FR_Sserv')

# S4.read/write rate and disk age. I'd like to prove that age has no impact on rate. But I failed
prwRC <- rwR_plot(subset(ioC,shTime < 6),'fig3-Age_Rate_Nserv','sepRateCutFit')
prwRTS <- rwR_plot(subset(ioTS,shTime < 6),'fig3-Age_Rate_Sserv','sepRateCutFit')

# S5.disk age and failure rate cut by read/write rate.
AFRAgeFrRateTS <- ioAFR(ioTS,fTS,c('shTime','sepRateCutFit'),12)
AFRAgeFrRateTS$AFR <- AFRAgeFrRateTS$AFR*5
afrRTS <- Age_Fr_RwRate_plot(subset(AFRAgeFrRateTS,shTime < 6),'fig3-Age_FR_rate_Sserv',12)

AFRAgeFrRateC <- ioAFR(ioC,fC,c('shTime','sepRateCutFit'))
AFRAgeFrRateC$AFR <- AFRAgeFrRateC$AFR*5
afrRC <- Age_Fr_RwRate_plot(subset(AFRAgeFrRateC,shTime < 6),'fig3-Age_FR_rate_Nserv',12)

# Nserv
# ioC$sepRate[ioC$sepRate <= 40 ] <- 40
# ioC$sepRate[ioC$sepRate <= 50 & ioC$sepRate > 40] <- 50
# fC$sepRate <- ioC$sepRate[match(fC$svr_id,ioC$svrid)]
# AFRRateC <- ioAFR(ioC,fC,c('sepRate'))
# AFRRateC$AFR <- AFRRateC$AFR*5
# pC <- AFR_plot(subset(AFRRateC),'fig3-Rate_FR_Nserv')

# S4 plot rate of write and disk failure group by disk age
# AFRRateAgeTS <- ioAFR(subset(ioTS,shTime <= 5),subset(fTS,shTime <= 5),c('shTime','sepRate'),12)
# pTS <- ggplot(AFRRateAgeTS,aes(x = sepRate,y = AFR)) + 
#   geom_bar(aes(fill = factor(shTime)),stat = 'identity',position = 'dodge') +
#   stat_smooth(aes(x = sepRate,linetype = factor(shTime)), se = F, method = "lm", formula = y ~ poly(x, 8))
# print(pTS)

# S5 plot rate of write and disk failure group by TBN (DONE)
# AFRRateC <- ioAFR(subset(ioC,shTime >3),subset(fC,shTime > 3),c('sepRateCut10','TBNcut'),12)
# names(AFRRateC)[grepl('sepRateCut',names(AFRRateC))] <- 'sepRate'
# AFRRateC$AFR <- AFRRateC$AFR*5
# pC <- AFR_TBNrate_plot(subset(AFRRateC),'fig3-TBN_rate_Nserv')
####################################
# S6.feature of read and write along with disk age. 
#We figure out mean number of bytes read and 
#mean number of written for each year to see and deveop the feature 
# staReadC <- list2df(tapply(ioC$mean_902,ioC$shTime,
#                            function(x)list('Read','Nserv',length(x),mean(x),sd(x),sd(x)/abs(mean(x)))))
# staWriteC <- list2df(tapply(ioC$mean_903,ioC$shTime,
#                             function(x)list('Write','Nserv',length(x),mean(x),sd(x),sd(x)/abs(mean(x)))))
# staRateC <- list2df(tapply(ioC$rwRate,ioC$shTime,
#                             function(x)list('Rate','Nserv',length(x),mean(x),sd(x),sd(x)/abs(mean(x)))))
# staReadTS <- list2df(tapply(ioTS$mean_902,ioTS$shTime,
#                             function(x)list('Read','Sserv',length(x),mean(x),sd(x),sd(x)/abs(mean(x)))))
# staWriteTS <- list2df(tapply(ioTS$mean_903,ioTS$shTime,
#                              function(x)list('Write','Sserv',length(x),mean(x),sd(x),sd(x)/abs(mean(x)))))
# staRateTS <- list2df(tapply(ioTS$rwRate,ioTS$shTime,
#                             function(x)list('Rate','Sserv',length(x),mean(x),sd(x),sd(x)/abs(mean(x)))))
# 
# staRW <- rbind(staReadC,staWriteC,staRateC,staReadTS,staWriteTS,staRateTS)
# names(staRW) <- c('class','type','count','mean','sd','cv','item')
# staRW <- subset(staRW,item <= 5)
# staRW$count <- fct2num(staRW$count)
# staRW$mean <- fct2num(staRW$mean)
# staRW$sd <- fct2num(staRW$sd)
# staRW$cv <- fct2num(staRW$cv)
# 
# ggplot(subset(staRW,type == 'Nserv'),aes(x = item,y = mean,fill = class)) + 
#   geom_bar(stat = 'identity',position = 'stack')
# ggplot(subset(staRW,type == 'Sserv'),aes(x = item,y = mean,fill = class)) + 
#   geom_bar(stat = 'identity',position = 'stack')
# ggplot(staRW,aes(x = item,y = mean,fill = class)) + 
#   geom_bar(stat = 'identity',position = 'stack') + 
#   facet_wrap(~type)
# ggplot(subset(staRW,type == 'Sserv' & class == 'Rate'),aes(x = mean,fill = item)) + 
#   geom_histogram(binwidth = 1)
